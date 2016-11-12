import tabulous._
import tabulous.Util._

object Main extends App
{
	val table:Table = Table.fromFile("TestTable.csv")
	println(table)
	println()
	val queried:Table = table
		.select("lname", "id")
		.filter{row => row("lname") == "henryton"}
	println(queried)
}
