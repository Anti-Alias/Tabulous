package tabulous
import scala.io.{Source}
import java.io.{File, InputStream, OutputStream, BufferedOutputStream, FileOutputStream}
import java.net.{URL}


/**
* Abstract representation of an immutable Table.
*/
trait Table extends scala.collection.immutable.Seq[Row]
{
	// ---------- ABSTRACT ------------
	/**
	* @return names of all columns.
	*/
	def columns: Array[String]

	/**
	* Number of rows in the Table
	*/
	def numRows:Int



	// ----------- DEFINED --------------
	override def size:Int = numRows
	override def length:Int = numRows
	override def iterator = new Iterator[Row]
	{
		var rowIndex:Int = 0
		override def hasNext:Boolean = rowIndex < numRows
		override def next:Row =
		{
			val nextRow:Row = apply(rowIndex)
			rowIndex += 1
			nextRow
		}
	}

	/**
	* Number of columns in the Table
	*/
	def numColumns:Int = columns.size

	/**
	* Acquires an element in the Table
	*/
	def apply(rowIndex:Int, columnIndex:Int):Any = apply(rowIndex)(columnIndex)


	/**
	* Acquires an element in the Table
	*/
	def apply(rowIndex:Int, column:String):Any = apply(rowIndex)(column)

	/**
	* @return selection of the given columns.
	*/
	def select(columnNames:String*):Table =
	{
		// Determines indices to select, and validates them
		val indices:scala.collection.Seq[Int] = columnNames.map{name => columns.indexOf(name)}
		for(i <- 0 until indices.length)
		{
			if(indices(i) == -1)
				throw InvalidColumnNameException(columnNames(i))
		}

		// Returns result
		selecti(indices:_*)
	}

	/**
	* @return selection of the given column indices.
	*/
	def selecti(columnIndices: Int*):Table =
	{
		require(columnIndices.forall{index => index>=0 && index<numColumns}, "Column index was out of bounds.")
		new SelectionTable(this, columnIndices.toArray)
	}

	/**
	* @return Table such that the columns excluding the ones specified
	* are selected.
	*/
	def withouti(columnIndices: Int*):Table =
	{
		val selected:Seq[Int] = (0 until numColumns) filterNot{columnIndices.contains}
		selecti(selected:_*)
	}


	/**
	* @return Table such that the columns excluding the ones specified
	* are selected.
	*/
	def without(columns: String*):Table =
	{
		checkColumns(columns)
		val indices:Seq[Int] = columns map {column => columns.indexOf(column)}
		withouti(indices:_*)
	}


	/**
	* @param p Predicate Rows must satisfy.
	* @return a new Table such that each row
	* satisifies the predicate 
	*/
	override def filter(p:Row=>Boolean):Table =
	{
		val rowIndices:Seq[Int] = (0 until numRows)
			.filter{rowIndex:Int => p(apply(rowIndex))}
		FilterTable(this, rowIndices:Seq[Int])
	}

	/**
	* @param p Predicate Rows must satisfy.
	* @return a new Table such that each row
	* satisifies the predicate 
	*/
	def where(p:Row=>Boolean):Table = filter(p)


	/**
	* @return region of the Table specified.
	*/
	def region(start:Int, count:Int):Table = RegionTable(this, start, count)


	/**
	* Unsafe version of transform method.
	*/
	private def _transformi(transformations:Map[Int, Any=>Any]):Table =
	{
		// Converts transformations Map into an Array of transformations.
		// Keys that do not exist evaluate to the default function which
		// simply returns the original value.
		val defaultFunc:Any=>Any = (any)=>any
		val funcs:Array[Any=>Any] = (0 until columns.length)
			.map {columnIndex => transformations.getOrElse(columnIndex, defaultFunc)}
			.toArray

		// Builds the final transformation function.
		val transformFunc:Row=>Row = (row)=>
		{
			// Gets keys and values for this row
			val columns:Array[String] = row.columns
			val data:Array[Any] = row.dataArray

			// Trasforms existing data
			var i:Int = 0
			while(i < data.length)
			{
				val oldElem:Any = data(i)
				val newElem:Any = funcs(i)(oldElem)
				data(i) = newElem
				i += 1
			}

			// Returns Row generated
			FreeRow(columns, data)	
		}

		// Returns Table view that transforms its rows on the fly.
		MapTable(this, transformFunc)
	}



	/**
	* This applies transformations to this Table.
	* @param transformations Column indices
	* and the functions to apply to their corresponding columns.
	*/
	def transformi(transformations:Map[Int, Any=>Any]):Table =
	{
		// Checks column indices
		val indices:Iterable[Int] = transformations.keys
		require(indices.forall{index => index >= 0 && index < numColumns}, s"A column specified did not exist")

		// Invokes real code
		_transformi(transformations)
	}


	/**
	* This applies transformations to this Table.
	* @param transformations Column names
	* and the functions to apply to their corresponding columns.
	*/
	def transform(transformations:Map[String, Any=>Any]):Table =
	{
		// Validates column names
		val tcolumns:Iterable[String] = transformations.keys
		for(column:String <- tcolumns)
			if(!columns.contains(column))
				throw InvalidColumnNameException(column)

		// Converts from a Mapping of String to any functions to Int to any functions.
		val indices:Map[Int, Any=>Any] = transformations
			.map{ elem:(String, Any=>Any) => (columns.indexOf(elem._1), elem._2) }

		// Invokes the index version
		_transformi(indices)
	}


	/**
	* @param newNames New Seq of column names to replace the old ones.
	* @return This table such that
	* the existing column names are replaced with new ones.
	*/
	def renamei(newNames:String*):Table =
	{
		require(newNames.length == numColumns, s"Found ${newNames.length} column name(s) when renaming Table.  Required $numColumns")
		require(newNames.distinct == newNames, "Duplicate column names found when trying to rename Table")
		RenamedTable(this, newNames.toArray)
	}


	/**
	* @param replacements Vararg of tuples representing coumn name replacements.
	* Those not specified
	* will default to their original.
	* @return Table with replaced column names.
	*/
	def rename(replacements:(String,String)*):Table =
	{
		// Validates column names
		for(entry <- replacements)
		{
			if(!columns.contains(entry._1))
				throw new InvalidColumnNameException(entry._1)
		}

		// Gets all indices of replacements.
		val rIndices:Seq[(Int, String)] = replacements
			.map {entry => (columns.indexOf(entry._1), entry._2)}

		// Allocates array
		var newNames:Array[String] = columns.clone
		for(entry <- rIndices)
			newNames(entry._1) = entry._2

		// Invokes rename
		renamei(newNames:_*)
	}


	/**
	* Joins this Table with another on the specified column.
	*/
	def join(that:Table, column:String):Table =
	{
		// Validates column name on both Tables
		checkColumn(column)
		that.checkColumn(column)

		// Determines rows that need to join
		val joins:Seq[Int] = for
		{
			rowIndex:Int <- 0 until numRows
		}
		yield
		{
			val row:Row = apply(rowIndex)
			val thatRowIndex:Int = that.indexWhere{thatRow:Row => thatRow(column) == row(column)}
			println(rowIndex, thatRowIndex)
			thatRowIndex
		}

		// Returns JoinedTable
		JoinedTable(this, that.without(column), joins)
	}


	/**
	* Sorts Rows in this Table.
	*/
	override def sortWith(p:(Row, Row)=>Boolean):Table =
	{
		val arrangement:Seq[Int] = this
			.zipWithIndex
			.sortWith{(a, b)=> p(a._1, b._1)}
			.map(_._2)
		RearrangedTable(this, arrangement)		
	}


	/**
	* Forces all mutations, filters, selections, etc to
	* aggregate to a completely new compiled table.
	* This prevents the problem that occurs when too many Table
	* views pile up.
	*/
	def compile:Table = Table.copy(this)


	/**
	* Invokes code on all Rows in the Table.
	*/
	override def foreach[U](f:Row=>U):Unit = for(rowIndex <- 0 until numRows) f(apply(rowIndex))


	/**
	* @return String representation of this Table.
	*/
	override def toString:String =
	{
		// Gets widths of each column
		val columnWidths:Seq[Int] = calcColumnWidths
		
		// Appends legend.
		val builder = new StringBuilder()
		for(columnIndex:Int <- 0 until numColumns)
		{
			val name:String = columns(columnIndex)
			val colWidth:Int = columnWidths(columnIndex)
			builder.append(pad(name+", ", colWidth+2))
		}
		builder.append('\n')

		// Appends separator
		val len:Int = builder.length
		for(i <- 0 until len) builder.append('-')
		builder.append('\n')

		// Appends rows
		for(row:Row <- this) builder
			.append(row.toString(columnWidths))
			.append('\n')

		// Returns result
		builder.toString
	}


	/**
	* Width of each column as a String.
	*/
	private def calcColumnWidths:Seq[Int] = (0 until numColumns) map {widthOfColumn}

	/**
	* With of String representation of a given column.
	*/
	private def widthOfColumn(columnIndex:Int):Int =
	{
		// Gets all Anys in column
		val anys:Seq[Any] = (0 until numRows) map {rowIndex => apply(rowIndex, columnIndex)}
		
		// Finds longest String		
		val anyLen:Int = anys
			.map{any => any.toString.length}
			.max
		
		// Returns with result		
		math.max(anyLen, columns(columnIndex).toString.length)
	}


	/**
	* Converts this Table to an InputStream that can be read from.
	*/
	def toInputStream:InputStream = new InputStream
	{
		var rowIndex:Int = -1
		var charIndex:Int = 0
		var line:String = null

		override def read:Int =
		{
			// Validates line
			if(line == null)
			{
				if(rowIndex == -1) line = loadHeader() + "\n"
				else if(rowIndex < numRows) line = loadNextLine + "\n"
				else line = null
				charIndex = 0
				rowIndex += 1
			}

			// Interprets line, or returns -1
			if(line != null)
			{
				val c:Char = line.charAt(charIndex)
				charIndex += 1
				if(charIndex >= line.length) line = null
				c.toInt
			}
			else  -1
		}

		def loadHeader():String = String.join(",", columns:_*)
		def loadNextLine():String = apply(rowIndex).mkString(",")
	}


	/**
	* @return This Table as a CSV String.
	*/
	def toCSVString:String =
	{
		val buffer = new StringBuffer()
		val in:InputStream = toInputStream
		var b:Int = in.read
		while(b != -1)
		{
			buffer.append(b.toChar)
			b = in.read
		}
		in.close
		buffer.toString
	}

	/**
	* Writes contents of this Table as a CSV to the
	* supplied OutputStream
	*/
	def writeTo(out:OutputStream)
	{
		val in = toInputStream
		var b:Int = in.read
		while(b != -1)
		{
			out.write(b)
			b = in.read
		}
		in.close
		out.close
	}


	/**
	* Writes contents of this Table as a CSV to the
	* supplied File
	*/
	def writeTo(file:File):Unit = writeTo(new BufferedOutputStream(new FileOutputStream(file)))

	/**
	* Writes contents of this Table as a CSV to the
	* supplied File
	*/
	def writeToFile(fileName:String):Unit = writeTo(new File(fileName))


	/**
	* Checks column name
	*/
	private def checkColumn(column:String):Unit =
	{
		if(!columns.contains(column))
			throw InvalidColumnNameException(column)
	}


	/**
	* Checks colummn names
	*/
	private def checkColumns(columns:Seq[String]):Unit =
	{
		for(column:String <- columns)
			checkColumn(column)
	}
}


/**
* Helpful companion object
*/
object Table
{
	def fromFile(fileName:String):Table = fromFile(new File(fileName))
	def fromFile(file:File):Table = fromURL(file.toURI.toURL)

	/**
	* Gets a Table from a URL
	*/
	def fromURL(url:URL):Table =
	{
		val lines:Iterator[String] = Source.fromURL(url).getLines
		val columns:Array[String] = lines.next.split(",")
		val data:Array[String] = lines
			.flatMap{_.split(",")}
			.toArray
		ArrayTable(columns, data.as[Array[Any]])
	}


	/**
	* Constructs from data.
	* @param columns Columns names in the Table.
	* @param data All data in the Table as a flat Seq.  Length of data should
	* be divisible by the number of columns.
	*/
	def fromFlatData(columns:Seq[String], data:Seq[Any]):Table = ArrayTable(columns.toArray, data.toArray)
	

	/**
	* Constructs from data.
	* @param columns Columns names in the Table.
	* @param data All data in the Table as a jagged Seq.  Length of every row should be
	* equal to the number of columns.
	*/
	def fromData(columns:Seq[String], data:Seq[Seq[Any]]):Table =
	{
		// Validates row length
		require(data.forall{seq => seq.length == columns.length}, "Length of one Row is not the same as the number of columns.")
		fromFlatData(columns, data.flatten)
	}


	/**
	* Copies a Table.
	*/
	def copy(table:Table):Table =
	{
		val columns:Array[String] = table.columns
		val data:Array[Any] = table.flatMap{_.toSeq}.toArray
		ArrayTable(columns, data)
	}
}
