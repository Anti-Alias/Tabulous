package tabulous

/**
* Holds implicit methods for casting Any type.
*/
object AnyOps
{
	implicit class AnyExtended(a:Any)
	{
		def as[T] = a.asInstanceOf[T]
	}
}
