import tabulous._
import org.scalatest.FunSuite
import TestUtil._


/**
* Main Test
*/
class BasicTests extends FunSuite 
{
	// Loads tables
	val person:Table = Table.fromFile("person.csv")
	runPersonTests(person)


	// Runs Tests
	def runPersonTests(person:Table)
	{
		test("Table selection columns should match.")
		{
			val selected:Table = person.select("fname", "lname")
			assert(selected.columns.toSeq == Seq("fname", "lname"))
			assert(selected.numRows == person.numRows)
		}

		test("Table renamed should match")
		{
			val renamed1:Table = person.rename("id" -> "pid")
			val renamed2:Table = person.renamei("pid", "fame", "lame")
			assert(
				renamed1.columns.toSeq == Seq("pid", "fname", "lname") &&
				renamed2.columns.toSeq == Seq("pid", "fame", "lame")
			)
		}

		test("Table should rename correctly, and renamed table should have transformation functions applied correctly.")
		{
			// Transformations
			val toIntFunc = (any:Any) => any.toString.toInt
			val toUpperCase = (any:Any) => any.toString.toUpperCase
			val nop = (any:Any) => any
			val transformations = Map(
				"pid" -> toIntFunc,
				"fname" -> toUpperCase,
				"lname" -> toUpperCase
			)

			// Transformed table
			val transformed:Table = person
				.rename("id" -> "pid")
				.transform(transformations)

			// Assertion
			assert(
				transformed.columns.toSeq == Seq("pid", "fname", "lname") &&
				transformed.forall{row:Row => row("pid").getClass == classOf[java.lang.Integer]} &&
				transformed.forall{row:Row => isUpperCase(row("fname").toString)} &&
				transformed.forall{row:Row => isUpperCase(row("lname").toString)}
			)
		}
	}
}


// Contains utility functions for testing
object TestUtil
{
	def isUpperCase(str:String):Boolean = !containsLowerCase(str)
	def containsLowerCase(str:String):Boolean = str find {c:Char => c>=97 && c<=122} match
	{
		case Some(c) => true
		case None => false
	}
}
