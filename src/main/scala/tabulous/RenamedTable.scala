package tabulous


/**
* Represents a Table with renamed columns
*/
private[tabulous] case class RenamedTable(underlying: Table, override val columns:Array[String]) extends Table
{
	override def numRows:Int = underlying.numRows
	override def apply(rowIndex:Int):Row = underlying.apply(rowIndex)
	override def apply(rowIndex:Int, columnIndex:Int) = underlying.apply(rowIndex, columnIndex)
}
