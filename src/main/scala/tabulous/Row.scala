package tabulous


/**
* Represents an immutable row in a Table.
*/
trait Row extends Seq[Any]
{
	// -------- ABSTRACT -----------

	/**
	* Names of columns.  This is usually accessed
	* by the containing Table.
	*/
	def columns:Array[String]


	// --------- IMPLEMENTED ----------
	/**
	* Converts all data in columns to an array of their data.
	*/
	def dataArray:Array[Any] = (0 until numColumns)
		.map{columnIndex => apply(columnIndex)}
		.toArray

	override def iterator = new Iterator[Any]
	{
		var columnIndex:Int = 0
		override def hasNext:Boolean = columnIndex < numColumns
		override def next:Any =
		{
			val result:Any = apply(columnIndex)
			columnIndex += 1
			result
		}
	}
	override def length:Int = numColumns
	override def size:Int = numColumns

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
	override def foreach[U](f:Any=>U):Unit = (0 until numColumns) foreach {index => f(apply(index))}


	/**
	* @return string representation of this Row
	* columns that are too long will be truncated, while those too short
	* will be padded with spaces.
	*/
	def toString(columnWidths:Seq[Int]):String =
	{
		val asStrings:Seq[String] = this
			.zipWithIndex
			.map{elem:(Any, Int) =>
				val len:Int = columnWidths(elem._2)
				pad(elem._1.toString + ", ", len+2)
			}
		asStrings.mkString("")
	}

	override def toString:String = this
		.map{any => any.toString}
		.mkString("")
}
