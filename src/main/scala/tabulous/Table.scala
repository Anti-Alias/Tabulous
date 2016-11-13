package tabulous
import scala.io.{Source}
import java.io.{File}
import java.net.{URL}


/**
* Abstract representation of an immutable Table.
*/
trait Table extends Traversable[Row]
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


	/**
	* Acquires a row by number
	*/
	def apply(rowIndex:Int):Row



	// ----------- DEFINED --------------
	override def size:Int = numRows

	/**
	* Number of columns in the Table
	*/
	def numColumns:Int = columns.size

	/**
	* Acquires an element in the Table
	*/
	def apply(rowIndex:Int, columnIndex:Int):Any = apply(rowIndex)(columnIndex)


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
	* Stringifies this Table.
	*/
	def toString(colWidth:Int):String =
	{
		// Creates StringBuilder
		val builder = new StringBuilder()

		// Appends legend.
		for(name:String <- columns)
			builder.append(pad(name, colWidth))
		builder.append('\n')

		// Appends separator
		val len:Int = builder.length
		for(i <- 0 until len) builder.append('-')
		builder.append('\n')

		// Appends rows
		for(row:Row <- this) builder
			.append(row.toString(colWidth))
			.append('\n')

		// Returns result
		builder.toString
	}
	override def toString:String =
	{
		val maxNameLen:Int = columns
			.map{_.length}
			.foldLeft(0) {(a:Int, b:Int) => if(a>b) a else b}
		toString(10)
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
	* Copies a Table.
	*/
	def copy(table:Table):Table =
	{
		val columns:Array[String] = table.columns
		val data:Array[Any] = table.flatMap{_.toSeq}.toArray
		ArrayTable(columns, data)
	}
}
