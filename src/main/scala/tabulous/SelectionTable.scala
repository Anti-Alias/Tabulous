package tabulous


/**
* Represents a Table selection.
* @param underlying Table being viewed
* @param columns Columns to include
*/
private[tabulous] class SelectionTable(val underlying:Table, columnIndices:Array[Int]) extends Table
{
	override val columns:Array[String] = columnIndices
		.map{index => underlying.columns(index)}
	override def numRows:Int = underlying.numRows
	override def apply(rowIndex:Int):Row = RowSelection(rowIndex)
	override def apply(rowIndex:Int, columnIndex:Int):Any = underlying(rowIndex, columnIndices(columnIndex))

	/**
	* Implementation of Row for this class
	*/
	case class RowSelection(rowIndex: Int) extends Row
	{
		override def columns:Array[String] = SelectionTable.this.columns
		override def apply(columnIndex:Int):Any = underlying(rowIndex, columnIndices(columnIndex))
	}
}
