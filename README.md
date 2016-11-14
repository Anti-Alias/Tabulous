# Tabulous
This library is used to process .csv files as if they were Tables in a database.  Every Table is a Seq of Row instances and an array of column names.  A Table is not a generic type, but rather stores data as 'Any' at compile time.  The programmer must cast to the appropriate runtime type when working with the data.

### Simple example of Table usage:
In this example, a Table is being loaded from a File.  Because of this, the data loaded will be nothing but unparsed Strings.  New Tables can be created from old Tables using methods like .select, (.where and .filter), .region, .transform and more. These methods produce views of an underlying Tables, so the cost of invoking them is minimal.

```scala
import tabulous._

object Main extends App
{
  // Creates a Table from a file.
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

### Example of Table access:
Accessing Rows in a Table as simple as accessing an element from an Array or Seq.  Accessing an element from a Row is just as simple.  The programmer can access elements in a Table by using table(n1)(n2) syntax, or table(n1, n2) syntax.  The former first accesses or produces a Row, then accesses an element from it.  The latter can be more efficient if the Table implementation is optimized for it.

```scala
import tabulous._

object Main extends App
{
  // Makes Table
  val table:Table = Table.fromFile("TestTable.csv")

  // Arbitrarily accesses elements
  val row:Row = table(0)
  val fname:String = row("fname").asInstanceOf[String]  // "Verbose" cast
  val lname:String = table(0)("lname").as[String]       // Implicit cast.
  val id:Int = table(0, "id").toString.toInt            //
}
```


### Example of transformations:
Since the data in Tables loaded from Files are always Strings, you might want to it to the appropriate type.  For instance, if one column in a Table represents the id of a person, it wouldn't make too much sense to have each id be stored as a String.  It would make more sense for it to be an Int.  While tricky, casting entries to their appropriate runtime type is possible.  Transformations don't only have to be done for the sake of casting.  It can also be done to do things like force String entries to be either lower case or upper case.  Doubles can have sqrt() applied to them.  The sky is the limit.

```scala
import tabulous._

object Main extends App
{
	// Gets Table from file
	val table:Table = Table.fromFile("TestTable.csv")

	// Builds functions used to parse string data.
	val toIntFunc = (any:Any) => any.toString.toInt
	val toStringFunc = (any:Any) => any.toString
	val castTransformations:Map[String, Any=>Any] = Map(
		"id" -> toIntFunc,
		"fname" -> toStringFunc
	)

	// Transforms table
	val transformed:Table = table
		.transform(castTransformations)

	// Outputs new table
	println(transformed)
}
```

### Disclaimer:
Every time you invoke a transformative method like .select, .filter, .convert and .transform, you are creating a view of the Table.  This is more efficient in most cases since the transformations are computed on the fly rather than at once.  The downside is that the views can end up aggregating.  Performance will likely degrade on a Table that is too many views deep.  For this reason, if you intend to use a particular Table multiple times, be sure to invoke .compile on it.
