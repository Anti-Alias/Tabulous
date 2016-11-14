import tabulous._

object Main extends App
{
	// Gets tables
	val person:Table = Table.fromFile("person.csv").rename("id" -> "pid")
	val spouse:Table = Table.fromFile("spouse.csv")

	// Joins tables
	val join:Table = person
		.join(spouse, "pid")
		.compile

	// Outputs tables
	println("Person")
	println(person)
	println("Spouse")
	println(spouse)
	println("Join")
	println(join)
}
