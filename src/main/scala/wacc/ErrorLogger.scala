package wacc

object ErrorLogger {

    import scala.Console.err

    def log(msg: String) = {
        err.println(msg)
        sys.exit(200)
    }

}