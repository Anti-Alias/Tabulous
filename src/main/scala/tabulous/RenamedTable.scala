package tabulous


/**
* Represents a Table with renamed columns
*/
private[tabulous] case class RenamedTable(underlying: Table, override val columns:Array[String]) extends Table
{
	override def numRows:Int = underlying.numRows
	override def apply(rowIndex:Int):Row = RenamedRow(rowIndex)
	override def apply(rowIndex:Int, columnIndex:Int) = underlying.apply(rowIndex, columnIndex)

	case class RenamedRow(rowIndex:Int) extends Row
	{
		override def columns:Array[String] = RenamedTable.this.columns
		override def apply(columnIndex:Int):Any = underlying(rowIndex, columnIndex)
	}
}
