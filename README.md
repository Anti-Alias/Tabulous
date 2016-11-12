# Tabulous
This library is used to process .csv files as if they were Tables in a database.

### Example of Table creation:

```scala
import tabulous._

object Main extends App
{
  // Creates a table from a file, and prints out its contents
	val table:Table = Table.fromFile("TestTable.csv")

  // Selects only last name and id from table
  // and only considers the lines at which
  // the last name is henryton
	val queried:Table = table
		.select("lname", "id")
		.where{row => row("lname") == "henryton"}   // .filter() is also acceptable

    // Displays results
    println(queried)
}
```


### Example of Row access:
```scala
import tabulous._
import tabulous.Util._    // Used for implicit casting/conversion.

object Main extends App
{
  // Makes Table
  val table:Table = Table.fromFile("TestTable.csv")

  // Arbitrarily accesses elements
  val row:Row = table(0)
  val id:Any = row(0)
  val fname:String = row("fname").asInstanceOf[String]  // "Verbose" cast
  val lname:String = row("lname").as[String]            // Implicit cast from Util object.
}
```

### Disclaimer:
Every time you invoke a transformative method like .select, .filter and .convert, you are creating a view of the Table.  This is more efficient in most cases since the transformations are computed on the fly rather than at once.  The downside is that the views can end up aggregating.  Performance will likely degrade on a table that is too many views deep.  For this reason, if you intend to use a particular multiple times, be sure to invoke .compile on it.
