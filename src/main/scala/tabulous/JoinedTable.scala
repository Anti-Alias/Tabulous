package tabulous


/**
* Represents the joining of two Tables.
*/
private[tabulous] case class JoinedTable(left:Table, right:Table, joins:Seq[Int]) extends Table
{
	override lazy val columns:Array[String] = left.columns ++ right.columns
	override def numRows:Int = joins.size
	override def apply(rowIndex:Int):Row = JoinedRow(rowIndex)

	/**
	* Implementation of Row
	*/
	case class JoinedRow(rowIndex:Int) extends Row
	{
		override def columns:Array[String] = JoinedTable.this.columns
		override def apply(columnIndex:Int):Any =
		{
			if(columnIndex<left.numColumns) left(rowIndex)(columnIndex)
			else right(joins(rowIndex))(columnIndex-left.numColumns)
		}
	}
}
