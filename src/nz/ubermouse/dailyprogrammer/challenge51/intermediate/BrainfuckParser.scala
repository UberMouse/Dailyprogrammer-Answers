package nz.ubermouse.dailyprogrammer.challenge51.intermediate

import util.matching.Regex
import collection.immutable.{Stack, HashMap}

/**
 * Created by IntelliJ IDEA.
 * User: UberMouse
 * Date: 5/15/12
 * Time: 1:10 PM
 */

object BrainfuckParser {

  //Define a super case class Node for transporting case classes
  case class Node()

  //Define a case class for each operator in Brainfuck, capable storing data used by the interpreter/compiler
  case class PtrAdd(amount: Int) extends Node

  case class PtrSub(amount: Int) extends Node

  case class DataAdd(amount: Int) extends Node

  case class DataSub(amount: Int) extends Node

  case class PrintChar() extends Node

  case class ReadChar() extends Node

  case class ForwardJump(point: Int) extends Node

  case class BackJump(point: Int) extends Node

  val regexTemplate = "^({CHAR}+)"

  private def makeRegex(char: String) = new Regex(regexTemplate.replace("{CHAR}", char))

  private def realLength(code: String) = code.replaceAll(">+", ">")
                                         .replaceAll("<+", "<")
                                         .replaceAll("\\++", "+")
                                         .replaceAll("-+", "-").length

  def findJumps(code: String): Map[Int, Int] = {

    //Use a sub function to make the function signature cleaner, only return what is needed. The Jump Map, instead of
    //both the Map and the Stack
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

  def parse(code: String,
            offset: Int = 0,
            jumps: Map[Int, Int],
            parsedSyntax: List[Node] = List[Node]()): List[Node] = {
    val (parsedNode, newOffset) = code.substring(offset, offset + 1) match {
      case ">" => {
        val length = makeRegex(">").findFirstMatchIn(code.substring(offset)).get.group(1).length
        (new PtrAdd(length), offset + length)
      }
      case "<" => {
        val length = makeRegex("<").findFirstMatchIn(code.substring(offset)).get.group(1).length
        (new PtrSub(length), offset + length)
      }
      case "+" => {
        val length = makeRegex("\\+").findFirstMatchIn(code.substring(offset)).get.group(1).length
        (new DataAdd(length), offset + length)
      }
      case "-" => {
        val length = makeRegex("-").findFirstMatchIn(code.substring(offset)).get.group(1).length
        (new DataSub(length), offset + length)
      }
      case "." => (new PrintChar(), offset + 1)
      case "," => (new ReadChar(), offset + 1)
      case "[" => (new ForwardJump(jumps(realLength(code.substring(0, offset)))), offset + 1)
      case "]" => (new BackJump(jumps(realLength(code.substring(0, offset)))), offset + 1)
    }
    val updatedNodeList = parsedSyntax ++ List(parsedNode)
    if (code.length - 1 == newOffset - 1)
      updatedNodeList
    else
      parse(code, newOffset, jumps, updatedNodeList)
  }

  def apply(code: String) = {
    val spacesRemoved = code.replaceAll(" ", "")
    parse(spacesRemoved,
          0,
          findJumps(spacesRemoved
                    .replaceAll(">+", ">")
                    .replaceAll("<+", "<")
                    .replaceAll("\\++", "+")
                    .replaceAll("-+", "-")))
  }
}