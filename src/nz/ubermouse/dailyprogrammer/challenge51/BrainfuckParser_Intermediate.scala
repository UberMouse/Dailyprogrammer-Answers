package nz.ubermouse.dailyprogrammer.challenge51

import collection.immutable.HashMap

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
                                   "-" -> ((ptr: Int) => {
                                     cells(ptr) -= 1;
                                     ptr
                                   }),
                                   "+" -> ((ptr: Int) => {
                                     cells(ptr) += 1;
                                     ptr
                                   }),
                                   "." -> ((ptr: Int) => {
                                     print(String.valueOf(cells(ptr).asInstanceOf[Char]));
                                     ptr
                                   }),
                                   "," -> ((ptr: Int) => {
                                     cells(ptr) = readChar().asInstanceOf[Int];
                                     ptr
                                   }))
  var jumps: Map[Int, Int]   = new HashMap[Int, Int]

  def findJumps(code: String): Map[Int, Int] = {
    var map = new HashMap[Int, Int]
    def findMatchingJump(index: Int, offset: Int = 0): Int = {
      String.valueOf(code(index)) match {
        case "[" => {
          val nextLeftBracket = code.indexOf("[", index + offset + 1)
          val nextRightBracket = code.indexOf("]", index + offset + 1)
          if (nextLeftBracket > nextRightBracket || nextLeftBracket == -1)
            nextRightBracket
          else
            findMatchingJump(index, nextRightBracket - index)
        }
        case "]" => {
          for((k, v) <- map) {
            if (v == index)
              return k
          }
          -1
        }
      }
    }
    for (i <- 0 until code.length) {
      val char = code(i)
      if (String.valueOf(char) == "[") map += i -> findMatchingJump(i)
      else if (String.valueOf(char) == "]") map += i -> findMatchingJump(i)
    }
    map
  }

  def apply(code: String) = {
    jumps = findJumps(code)
    cells = Array.fill(MEMORY_SIZE)(0)
    //println(code, code.length)
    parse(code.replaceAll(" ", ""))
  }

  private def parse(code: String, offset: Int = 0, ptr: Int = 0): Int = {
    if (code.length == 1 || code.length - 1 == offset) parserFunctionMappings(String.valueOf(code.last))(ptr)
    else parse(code, offset + 1, parse(code.substring(offset, offset + 1), 0, ptr))
  }
}