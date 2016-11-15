import tabulous._
import java.io.{InputStream, FileOutputStream, BufferedOutputStream}



/**
* Main class that fires the application up
*/
object Main extends App
{
	// Transformation funcs
	val toUpperCase = (any:Any) => any.toString.toUpperCase
	val toInt = (any:Any) => any.toString.toInt

	// Transformations that should be made to the pokemon table.
	val transformations = Map(
		"id" -> toInt,
		"identifier" -> toUpperCase,
		"species_id" -> toInt,
		"height" -> toInt,
		"weight" -> toInt,
		"base_experience" -> toInt,
		"order" -> toInt
	)

	println("Loading table")
	val pokemon:Table = Table.fromFile("pokemon.csv")
		.transform(transformations)
		.rename("identifier" -> "species")
		.compile

	println("Finding Gs")
	val startsWithG:Table = pokemon
		.where{row => row("species").toString.startsWith("G")}
		.sortWith{(r1:Row, r2:Row) => r1("species").toString < r2("species").toString}
		.compile

	// Outputs tables
	println(startsWithG)
}
