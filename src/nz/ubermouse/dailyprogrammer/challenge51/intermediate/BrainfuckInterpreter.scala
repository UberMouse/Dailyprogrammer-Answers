package nz.ubermouse.dailyprogrammer.challenge51.intermediate

import collection.immutable.{Stack, HashMap}


/**
 * Created by IntelliJ IDEA.
 * User: UberMouse
 * Date: 5/12/12
 * Time: 11:37 PM
 */

object BrainfuckInterpreter {

  val MEMORY_SIZE            = 64000
  var cells                  = Array.fill(MEMORY_SIZE)(0)
  val parserFunctionMappings = Map(">" -> ((ptr: Int, offset: Int) => (ptr + 1, offset)),
                                   "<" -> ((ptr: Int, offset: Int) => (ptr - 1, offset)),
                                   "-" -> ((ptr: Int, offset: Int) => {
                                     cells(ptr) -= 1;
                                     (ptr, offset)
                                   }),
                                   "+" -> ((ptr: Int, offset: Int) => {
                                     cells(ptr) += 1;
                                     (ptr, offset)
                                   }),
                                   "." -> ((ptr: Int, offset: Int) => {
                                     print(String.valueOf(cells(ptr).asInstanceOf[Char]));
                                     (ptr, offset)
                                   }),
                                   "," -> ((ptr: Int, offset: Int) => {
                                     cells(ptr) = readChar().asInstanceOf[Int];
                                     (ptr, offset)
                                   }),
                                   "[" -> ((ptr: Int, offset: Int) => {
                                     if (cells(ptr) == 0) (ptr, jumps(offset))
                                     else (ptr, offset)
                                   }),
                                   "]" -> ((ptr: Int, offset: Int) => {
                                     if (cells(ptr) != 0) (ptr, jumps(offset))
                                     else (ptr, offset)
                                   }))
  var jumps: Map[Int, Int]   = new HashMap[Int, Int]

  def main(args: Array[String]) {
    apply("++++++++++[>>++++++>+++++++++++>++++++++++>+++++++++>+++>+++++>++++>++++++++>+[<]<-]>>+++++++.>+.-.>+++.<++++.>>+++++++.<<++.+.>+++++.>.<<-.>---.<-----.-.+++++.>>>+++.-.<<-.<+..----.>>>>++++++++.>+++++++..<<<<+.>>>>-.<<<<.++++.------.<+++++.---.>>>>>.<<<++.<<---.>++++++.>>>>+.<<<-.--------.<<+.>>>>>>+++.---.<-.<<<<---.<.>---.>>>>>>.")
  }

  def findJumps(code: String): Map[Int, Int] = {
    def recursiveWrapper(code: String, offset: Int = 0,
                         map: Map[Int, Int] = new HashMap[Int, Int],
                         stack: Stack[Int] = new Stack[Int]): (Map[Int, Int], Stack[Int]) = {
      if (code.length == 1 || offset == code.length - 1) {
        String.valueOf(code.last) match {
          case "[" => (map, stack push offset)
          case "]" => {
            val (jumpPoint, newStack) = (stack.head, stack.pop)
            val backJump = (offset, jumpPoint)
            val forwardJump = (jumpPoint, offset)
            println(backJump, forwardJump)
            (map + backJump + forwardJump, newStack)
          }
          case _ => (map, stack)
        }
      }
      else {
        val (newMap, newStack) = recursiveWrapper(code.substring(offset, offset + 1), offset, map, stack)
        recursiveWrapper(code, offset + 1, newMap, newStack)
      }
    }
    recursiveWrapper(code)._1
  }

  def apply(code: String, mode: Int = 0) = {
    jumps = findJumps(code)
    cells = Array.fill(MEMORY_SIZE)(0)
    parse(code.replaceAll(" ", ""))
  }

  private def parse(code: String, offset: Int = 0, ptr: Int = 0): (Int, Int) = {
    if (code.length == 1 || code.length - 1 == offset) parserFunctionMappings(String.valueOf(code.last))(ptr, offset)
    else {
      val (newPtr, newOffset) = parse(code.substring(offset, offset + 1), offset, ptr)
      parse(code, newOffset + 1, newPtr)
    }
  }
}