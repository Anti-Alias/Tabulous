package tabulous


/**
* Object that holds helpful String operations
*/
object StringHelper
{
	/**
	* Pads a String to a certain length, and truncates it
	* if its too long.
	*/
	def pad(str:String, len:Int):String = str.padTo(len, ' ').substring(0, len)
}
