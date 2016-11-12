package tabulous
import exception._
import Util._
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
	def apply(rowIndex:Int, columnIndex:Int):Any = apply(rowIndex).apply(columnIndex)


	/**
	* @return selection of the given columns.
	*/
	def select(columnNames:String*):Table =
	{
		// Determines indices to select, and validates them
		val indices:Seq[Int] = columnNames.map{name => columns.indexOf(name)}
		for(i <- 0 until indices.length)
		{
			if(indices(i) == -1)
				throw InvalidColumnNameException(columnNames(i))
		}

		// Returns result
		selecti(indices:_*)
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
	* @return selection of the given column indices.
	*/
	def selecti(columnIndices: Int*):Table =
	{
		require(columnIndices.forall{index => index>=0 && index<numColumns}, "Column index was out of bounds.")
		new SelectionTable(this, columnIndices.toArray)
	}


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
* Represents an immutable row in a Table.
*/
trait Row
{
	// -------- ABSTRACT -----------
	/**
	* Accesses an element in the Row by column index
	*/
	def apply(columnIndex:Int):Any

	/**
	* Names of columns.  This is usually accessed
	* by the containing Table.
	*/
	def columns:Array[String]


	// --------- IMPLEMENTED ----------
	/**
	* Acceses an elemlent in the Row by column name.
	*/
	def apply(columnName:String):Any =
	{
		val index:Int = columns.indexOf(columnName)
		if(index != -1) apply(index)
		else throw InvalidColumnNameException(columnName)
	}

	/**
	* Number of columns in the Row.
	*/
	def numColumns:Int = columns.length


	/**
	* @return string representation of this Row
	* @param colWidth Number of characters each column should be.
	* columns that are too long will be truncated, while those too short
	* will be padded with spaces.
	*/
	def toString(colWidth:Int):String = (0 until numColumns)
		.map {columnIndex => apply(columnIndex)}	// To Anys
		.map {any => pad(any.toString, colWidth)}	// To paddes Strings
		.mkString("")								// Joined as a single String
	override def toString:String = toString(20)
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
}
