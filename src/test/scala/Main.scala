import tabulous._

object Main extends App
{
	// Gets tables
	val person:Table = Table.fromFile("person.csv").rename("id"->"pid")
	val sorted:Table = person.sortWith{(r1, r2) =>
		(r1("lname").toString compareTo r2("lname").toString) < 0
	}

	println("Person")
	println(person)

	println("Sorted Person")
	println(sorted)
}
