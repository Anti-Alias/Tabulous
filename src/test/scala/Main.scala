import tabulous._
import tabulous.Util._

object Main extends App
{
	// Gets original table
	val table:Table = Table.fromFile("TestTable.csv")
	println("\n\n########################## ORIGINAL TABLE ############################")
	println(table)

	// Builds converter functions
	val uppercaseFunc = (any:Any)=>any.asString.toUpperCase
	val sqrtFunc = (any:Any) => math.sqrt(any.parseDouble)
	val doNothingFunc = (any:Any) => any
	val add4Func = (any:Any) => any.parseInt + 4

	// Puts them together.
	val transformations:Map[String, Any=>Any] = Map(
		"lname" -> uppercaseFunc,
		"id" -> add4Func.andThen(sqrtFunc)
	)

	// Transforms table
	val transformed:Table = table
		.select("lname", "id")
		.where{row => row("lname") == "henryton"}
		.transform(transformations)
	
	// Outputs new table
	println("\n\n########################## TRANSFORMED TABLE ##########################")
	println(transformed)
}
