package nz.ubermouse.dailyprogrammer.challenge51

/**
 * Created by IntelliJ IDEA.
 * User: UberMouse
 * Date: 5/12/12
 * Time: 11:37 PM
 */

object BrainfuckParser_Intermediate {

  val MEMORY_SIZE            = 64000
  var cells                  = Array.fill(MEMORY_SIZE)(0)
  val parserFunctionMappings = Map(">" -> ((ptr: Int) => ptr + 1),
                                   "<" -> ((ptr: Int) => ptr - 1),
                                   "-" -> ((ptr: Int) => {cells(ptr) -= 1; ptr}),
                                   "+" -> ((ptr: Int) => {cells(ptr) += 1; ptr}))

  def apply(code: String) = {
    cells = Array.fill(MEMORY_SIZE)(0)
    parse(code.replaceAll(" ", ""))
  }

  private def parse(code: String, ptr: Int = 0):Int = {
    if (code.length == 1) parserFunctionMappings(code)(ptr)
    else {
      parse(code.substring(1), parse(code.substring(0, 1), ptr))
    }
  }
}