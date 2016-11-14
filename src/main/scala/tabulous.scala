/**
* Holds useful implicits for the library
*/
package object tabulous
{
	/**
	* Implicit class used to make casting easier.
	*/
	implicit class AnyExtended(a:Any)
	{
		// Converters
		def parseDouble = a.toString.toDouble
		def parseInt = a.toString.toInt
		def parseChar = a.toString.charAt(0)
		def parseByte = a.toString.toByte
		def parseFloat = a.toString.toFloat
		def parseShort = a.toString.toShort
		def parseBoolean = a.toString.toBoolean
		def parseLong = a.toString.toLong

		// Casters
		def as[T] = a.asInstanceOf[T]
		def asDouble = a.asInstanceOf[Double]
		def asInt = a.asInstanceOf[Int]
		def asString = a.asInstanceOf[String]
		def asChar = a.asInstanceOf[Char]
		def asFloat = a.asInstanceOf[Float]
		def asBoolean = a.asInstanceOf[Boolean]
		def asShort = a.asInstanceOf[Short]
		def asLong = a.asInstanceOf[Long]
		def asByte = a.asInstanceOf[Byte]
	}


	/**
	* Forces a String to be a certain length.  Truncates it
	* if it is too long, and pads it if it is too short.
	* @param str String to pad.
	* @param len Desired length.
	*/
	def pad(str:String, len:Int):String = str.padTo(len, ' ').substring(0, len)
}
