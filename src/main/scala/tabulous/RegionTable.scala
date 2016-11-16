package tabulous


/**
* Represents a Region of a Table.
*/
private[tabulous] case class RegionTable(underlying:Table, start:Int, count:Int) extends Table
{
	// Checks args
	require(start >= 0 && count >= 0 && start+count <= underlying.numRows, s"Region ($start, ${start+count}] is outside the range (0, ${underlying.numRows}]")
	
	// -------------- IMPLEMENTATION ---------------------
	override def columns:Array[String] = underlying.columns
	override def numRows:Int = count
	override def apply(rowIndex:Int):Row = underlying.apply(rowIndex+start)
	override def apply(rowIndex:Int, columnIndex:Int) = underlying.apply(rowIndex+start, columnIndex)

	/**
	* Optimized for iteration.
	*/
	override def slice(start:Int, count:Int):Table = underlying match
	{
		case same:RegionTable => same.slice(this.start + start, count)
		case _ => super.slice(start, count)
	}
}
