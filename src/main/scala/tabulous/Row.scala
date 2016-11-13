package tabulous


/**
* Represents an immutable row in a Table.
*/
trait Row extends Traversable[Any]
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
	* Converts all data in columns to an array of their data.
	*/
	def dataArray:Array[Any] = (0 until numColumns)
		.map{columnIndex => apply(columnIndex)}
		.toArray


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
	* Used for iterating through each element in the Row
	*/
	override def foreach[U](f:Any=>U):Unit = (0 until numColumns) foreach {elem => f(elem)}


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
