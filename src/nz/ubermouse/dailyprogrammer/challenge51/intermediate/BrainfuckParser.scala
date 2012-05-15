package nz.ubermouse.dailyprogrammer.challenge51.intermediate

import util.matching.Regex

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

  def parse(code: String, offset: Int = 0, parsedSyntax: List[Node] = List[Node]()): List[Node] = {
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
    }
    val updatedNodeList = parsedSyntax ++ List(parsedNode)
    if (code.length - 1 == newOffset-1)
      updatedNodeList
    else
      parse(code, newOffset, updatedNodeList)
  }

  def apply(code: String) = {
    parse(code.replaceAll(" ", ""))
  }
}