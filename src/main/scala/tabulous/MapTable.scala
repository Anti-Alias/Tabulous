package tabulous
import scala.collection.immutable._



/**
* Represents a Table such that each row is transformed
* by some function.
* @param underlying Table this MapTable is a view of.
* @param func Function that is used to convert Rows.
*/
private[tabulous] case class MapTable(underlying:Table, func:Row=>Row) extends Table
{
	override def columns:Array[String] = underlying.columns
	override def numRows:Int = underlying.numRows
	override def apply(rowIndex:Int):Row = func(underlying(rowIndex))
}
