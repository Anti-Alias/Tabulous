package tabulous


/**
* Represents a Table selection.
* @param underlying Table being viewed
* @param columns Columns to include
*/
class TableSelection(val underlying:Table, columnIndices:Array[Int]) extends Table
{
	override val columns:Array[String] = columnIndices
		.map{index => underlying.columns(index)}
	override def numRows:Int = underlying.numRows
	override def apply(rowIndex:Int):Row = underlying.apply(rowIndex)
}
