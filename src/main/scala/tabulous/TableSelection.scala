package tabulous


/**
* Represents a Table selection.
* @param underlying Table being viewed
* @param columns Columns to include
*/
class TableSelection(val underlying:Table, columnIndices:Array[Int]) extends Table
{
	override lazy val columns:Seq[String] = columnIndices
		.map{index => underlying.columns(index)}
		.toSeq
	override def numRows:Int = underlying.numRows
	override def apply(rowIndex:Int):Row = underlying.apply(rowIndex)
}
//
