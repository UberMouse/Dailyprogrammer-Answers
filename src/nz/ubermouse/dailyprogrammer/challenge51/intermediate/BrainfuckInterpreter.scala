package nz.ubermouse.dailyprogrammer.challenge51.intermediate

import collection.immutable.{Stack, HashMap}
import nz.ubermouse.dailyprogrammer.challenge51.intermediate.BrainfuckParser._


/**
 * Created by IntelliJ IDEA.
 * User: UberMouse
 * Date: 5/12/12
 * Time: 11:37 PM
 */

object BrainfuckInterpreter {

  val MEMORY_SIZE            = 64000
  var cells                  = Array.fill(MEMORY_SIZE)(0)

  def main(args: Array[String]) {
    apply("++++++++++[>>++++++>+++++++++++>++++++++++>+++++++++>+++>+++++>++++>++++++++>+[<]<-]>>+++++++.>+.-.>+++.<++++.>>+++++++.<<++.+.>+++++.>.<<-.>---.<-----.-.+++++.>>>+++.-.<<-.<+..----.>>>>++++++++.>+++++++..<<<<+.>>>>-.<<<<.++++.------.<+++++.---.>>>>>.<<<++.<<---.>++++++.>>>>+.<<<-.--------.<<+.>>>>>>+++.---.<-.<<<<---.<.>---.>>>>>>.")
  }



  def apply(code: String, mode: Int = 0) = {
    cells = Array.fill(MEMORY_SIZE)(0)
    interpret(BrainfuckParser(code))
  }

  private def interpret(code: List[BrainfuckParser.Node], offset: Int = 0, ptr: Int = 0): (Int, Int) = {
    if (code.length == 1 || code.length - 1 == offset) {
      code.last match {
        case node:PtrAdd => (ptr + node.amount, offset)
        case node:PtrSub => (ptr - node.amount, offset)
        case node:DataAdd => {
          cells(ptr) += node.amount
          (ptr, offset)
        }
        case node:DataSub => {
          cells(ptr) -= node.amount
          (ptr, offset)
        }
        case node:PrintChar => {
          print(cells(ptr).asInstanceOf[Char])
          (ptr, offset)
        }
        case node:ReadChar => {
          cells(ptr) = readByte()
          (ptr, offset)
        }
        case node:ForwardJump => {
          (ptr, if(cells(ptr) == 0) node.point else offset)
        }
        case node:BackJump => {
          (ptr, if(cells(ptr) != 0) node.point else offset)
        }
      }
    }
    else {
      val (newPtr, newOffset) = interpret(List(code(offset)), offset, ptr)
      interpret(code, newOffset + 1, newPtr)
    }
  }
}