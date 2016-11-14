import tabulous._

object Main extends App
{
	// Gets original table
	val table:Table = Table.fromFile("TestTable.csv")
	println("\n\n########################## ORIGINAL TABLE ############################")
	println(table)

	// Builds converter functions

	// Puts them together.
	val toIntFunc = (any:Any) => any.toString.toInt
	val toStringFunc = (any:Any) => any.toString
	val castTransformations:Map[String, Any=>Any] = Map(
		"id" -> toIntFunc,
		"fname" -> toStringFunc
	)

	// Transforms table
	val transformed:Table = table
		.select("fname", "id")
		.transform(castTransformations)
		.where{row => row("fname") == "Henry"}
		.compile
	
	// Outputs new table
	println("\n\n########################## TRANSFORMED TABLE ##########################")
	println(transformed)
}
