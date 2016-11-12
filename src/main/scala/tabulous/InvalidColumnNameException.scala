package tabulous
case class InvalidColumnNameException(column:String) extends RuntimeException(column)
