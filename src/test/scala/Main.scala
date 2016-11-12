import tabulous._
import tabulous.Util._

object Main extends App
{
	println()
	val table:Table = Table.fromFile("TestTable.csv")
	println(table)
	println()

	val queried:Table = table
		.select("lname", "id")
		.where{row => row("lname") == "henryton"}
	println(queried)
}
