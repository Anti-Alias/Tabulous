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

	// Loads Pokemon, Pokemon stats, and stat names
	val pokemon:Table = Table.fromFile("pokemon.csv")
		.rename("id" -> "pid", "identifier" -> "species")
		.compile
	val stats:Table = Table.fromFile("pokemon_stats.csv")
		.rename("pokemon_id" -> "pid")
		.compile
	val statNames:Table = Table.fromFile("stat_names.csv")
		.where{row => row("local_language_id") == "9"}
		.deselect("local_language_id")
		.compile

	// Queries for information
	val query:Table = stats
		.join(pokemon, "pid")
		.join(statNames, "stat_id")
		.rename("name" -> "stat_name")


	// Method for getting all pokemon from a Table.
	def toPokemon(table:Table):List[Pokemon] = (for
	{
		sub:Table <- table.sliding(6)
	}
	yield
	{
		val statColumn:Int = sub.columns.indexOf("base_stat")
		val species:String = sub(0, "species").toString
		val hp:Int = sub(0, statColumn).parseInt
		val attack:Int = sub(1, statColumn).parseInt
		val defense:Int = sub(2, statColumn).parseInt
		val sAttack:Int = sub(3, statColumn).parseInt
		val sDefense:Int = sub(4, statColumn).parseInt
		val speed:Int = sub(5, statColumn).parseInt
		Pokemon(species, hp, attack, defense, sAttack, sDefense, speed)
	}).toList

	println(toPokemon(query))
}

case class Pokemon(name:String, hp:Int, attack:Int, defense:Int, sAttack:Int, sDefense:Int, speed:Int)

