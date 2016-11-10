package tabulous.exception
case class InvalidColumnNameException(column:String) extends RuntimeException(column)
