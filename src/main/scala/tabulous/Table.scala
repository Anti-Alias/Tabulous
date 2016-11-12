package tabulous
import tabulous.exception._
import Util._


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
	/**
	* Number of columns in the Table
	*/
	def numColumns:Int = columns.size


	/**
	* @return selection of the given columns.
	*/
	def select(columnNames:String*):Table =
	{
		// Determines indices to select, and validates them
		val indices:Seq[Int] = columns.map{name => columns.indexOf(name)}
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
		new TableSelection(this, columnIndices.toArray)
	}


	/**
	* Invokes code on all Rows in the Table.
	*/
	override def foreach[U](f:Row=>U):Unit = (0 until numColumns) map {rowIndex => apply(rowIndex)}


	/**
	* Stringifies this Table.
	*/
	def toString(colWidth:Int):String =
	{
		val builder = new StringBuilder()
		for(name:String <- columns) builder.append(pad(name, colWidth))
		for(row:Row <- this) builder.append(row.toString(colWidth))
		builder.toString
	}
	override def toString:String = toString(20)
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

	/**
	* Number of columns in the Row.
	*/
	def numColumns:Int = columns.length


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
	* @return string representation of this Row
	* @param colWidth Number of characters each column should be.
	* columns that are too long will be truncated, while those too short
	* will be padded with spaces.
	*/
	def toString(colWidth:Int):String = (0 until numColumns)
		.map {columnIndex => apply(columnIndex)}	// To Anys
		.map {any => "|"+pad(any.toString, 20)}		// To paddes Strings
		.mkString(" ")								// Joined as a single String.
	override def toString:String = toString(20)
}


/**
* Helpful companion object
*/
object Table
{
}
