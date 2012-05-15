package nz.ubermouse.dailyprogrammer.challenge51.intermediate

/**
 * Created by IntelliJ IDEA.
 * User: UberMouse
 * Date: 5/14/12
 * Time: 11:26 PM
 */

object BrainfuckCompiler {

  val TEMPLATE_PROG =
    """
      | SET J, 10000
      | {CODE}
      | SET PC, end
      |
      | :end
      | SET PC, end
    """.stripMargin

  val assemblerFunctions = Map(">" -> "ADD J, 1",
                               "<" -> "SUB J, 1",
                               "+" -> "ADD [J], 1",
                               "-" -> "SUB [J], 1",
                               "[" -> "",
                               "]" -> "")

  def main(args: Array[String]) {
    print(apply("++>++<-"))
  }

  def apply(code: String) = {
    print(TEMPLATE_PROG.replace("{CODE}", assemble(code)))
  }

  private def assemble(code: String): String = {
    if (code.length == 1) assemblerFunctions(code) + "\n"
    else assemble(code.substring(0, 1)) + assemble(code.substring(1))
  }
}