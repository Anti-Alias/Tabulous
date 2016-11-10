package tabulous
import tabulous.exception._


/**
* Abstract representation of an immutable Table.
*/
trait Table
{
	// ---------- ABSTRACT ------------
	/**
	* @return names of all columns.
	*/
	def columns: Seq[String]

	/**
	* Number of rows in the Table
	*/
	def numRows:Int

	/**
	* Acquires a row by number
	*/
	def apply(rowNum:Int):Row


	// ----------- DEFINED --------------
	/**
	* Number of columns in the Table
	*/
	def numColumns:Int = columns.size


	/**
	* @return selection of the given columns.
	*/
	def select(columnIndices: Int*):Table = new TableSelection(this, columnIndices.toArray)
}


/**
* Represents an immutable row in a Table.
*/
trait Row
{
	def apply(i:Int):Any
	def apply(columnName:String):Any =
	{
		val index:Int = entries.indexOf(columnName)
		if(index != -1) apply(index)
		else throw InvalidColumnNameException(columnName)
	}
	def entries:Seq[String]
}


/**
* Helpful companion object
*/
object Table
{
}
