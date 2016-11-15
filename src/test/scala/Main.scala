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

	println("Loading tables")
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

	println("Joining tables")
	val joined:Table = stats
		.join(pokemon, "pid")
		.join(statNames, "stat_id")
		.rename("name" -> "stat_name")
		.compile


	def toPokemon(table:Table, current:Seq[Pokemon]=Seq.empty):Seq[Pokemon] =
	{
		if(table.numRows == 0) current
		else
		{
			val statColumn:Int = table.columns.indexOf("base_stat")
			val species:String = table(0, "species").toString
			val hp:Int = table(0, statColumn).parseInt
			val attack:Int = table(1, statColumn).parseInt
			val defense:Int = table(2, statColumn).parseInt
			val sAttack:Int = table(3, statColumn).parseInt
			val sDefense:Int = table(4, statColumn).parseInt
			val speed:Int = table(4, statColumn).parseInt
			val poke = Pokemon(species, hp, attack, defense, sAttack, sDefense, speed)
			val oneMore = current :+ poke
			toPokemon(table.region(6), oneMore)
		}
	}

	

	// Outputs tables
	println("Starting")
	val start:Long = System.currentTimeMillis
	val all:Seq[Pokemon] = toPokemon(joined)
	val elapsed = System.currentTimeMillis - start
	println(elapsed)
	println(toPokemon(joined).size)
}

case class Pokemon(name:String, hp:Int, attack:Int, defense:Int, sAttack:Int, sDefense:Int, speed:Int)

