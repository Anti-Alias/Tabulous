import tabulous._

object Main extends App
{
	// Gets tables
	val person:Table = Table.fromFile("person.csv")
	val renamed:Table = person.rename("id"->"pid", "lname"->"lame")
	val selected:Table = renamed.select("pid","lame")

	println(person)
	println(renamed)
	println(selected)
}
