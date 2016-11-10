package tabulous
import tabulous.exception._
import StringHelper._


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
	override def toString(colWidth:Int):String =
	{
		val builder = new StringBuilder()
		for(name:String <- columns) builder.append(pad(name, colWidth))
		for(row:Row <- this) builder.append(row.toString(colWidth))
		builder.toString
	}
}


/**
* Represents an immutable row in a Table.
*/
trait Row
{
	def apply(i:Int):Any
	def columns:Array[String]

	def apply(columnName:String):Any =
	{
		val index:Int = columns.indexOf(columnName)
		if(index != -1) apply(index)
		else throw InvalidColumnNameException(columnName)
	}
}


/**
* Helpful companion object
*/
object Table
{
}
