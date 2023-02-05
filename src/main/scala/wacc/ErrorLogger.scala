package wacc

final case class TypeException(private val message: String = "", private val cause: Throwable = None.orNull) extends Exception(message, cause) {

    override def toString(): String = message

}

object ErrorLogger {

    import scala.Console.err


    def log(msg: String) = {
        throw new TypeException(msg, null)
    }

}