package tabulous


/**
* Table that stores original data rather than acting as a view
* of an existing one.
*/
case class ArrayTable(override val columns:Array[String], data:Array[Any]) extends Table
{
	// Checks data
	require(data.length%columns.length == 0, "Number of data elements must be divisible by the number of columns.  Data invalid.")

	// ----------- OVERRIDDEN -------------
	override val numRows:Int = data.length/columns.length
	override def apply(rowIndex:Int):Row = DataRow(rowIndex)
	override def apply(rowIndex:Int, columnIndex:Int):Any = data(rowIndex*numColumns + columnIndex)


	/**
	* Row implementation for ArrayTable
	*/
	case class DataRow(rowIndex:Int) extends Row
	{
		override def apply(columnIndex:Int):Any = data(rowIndex*numColumns + columnIndex)
		override def columns:Array[String] = ArrayTable.this.columns
	}
}
