package tabulous


/**
* Table view that stores a subset of the existing Rows in
* an underlying Table.
*/
private[tabulous] case class FilterTable(underlying:Table, rowIndices:Seq[Int]) extends Table
{
	override def columns:Array[String] = underlying.columns
	override def numRows:Int = rowIndices.length
	override def apply(rowIndex:Int):Row = underlying.apply(rowIndices(rowIndex))
}
