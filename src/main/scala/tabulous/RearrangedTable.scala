package tabulous


/**
* Table implementation such that all Rows are rearranged.
*/
private[tabulous] case class RearrangedTable(underlying:Table, arrangement:Seq[Int]) extends Table
{
	override def apply(rowIndex:Int):Row = underlying(arrangement(rowIndex))
	override def apply(rowIndex:Int, columnIndex:Int):Any = underlying(arrangement(rowIndex), columnIndex)
	override def numRows:Int = underlying.numRows
	override def columns:Array[String] = underlying.columns
}
