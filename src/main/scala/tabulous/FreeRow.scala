package tabulous


/**
* Represents a Row free of any
* encapsulating Table.
* @param replacements
*/
private[tabulous] case class FreeRow(override val columns:Array[String], values:Array[Any]) extends Row
{
	override def apply(columnIndex:Int):Any = values(columnIndex)
}
