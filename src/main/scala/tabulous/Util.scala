package tabulous
import scala.collection.immutable._


/**
* Holds implicit methods for casting Any type.
*/
object Util
{
	/**
	* Implicit class used to make casting easier.
	*/
	implicit class AnyExtended(a:Any)
	{
		def toInt:Int = a.as[String].toInt
		def as[T] = a.asInstanceOf[T]
	}


	/**
	* Forces a String to be a certain length.  Truncates it
	* if it is too long, and pads it if it is too short.
	* @param str String to pad.
	* @param len Desired length.
	*/
	def pad(str:String, len:Int):String = str.padTo(len, ' ').substring(0, len)
}
