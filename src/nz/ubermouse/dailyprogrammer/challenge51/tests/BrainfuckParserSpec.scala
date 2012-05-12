package nz.ubermouse.dailyprogrammer.challenge51.tests

import org.scalatest.WordSpec
import nz.ubermouse.dailyprogrammer.challenge51.{BrainfuckParser_Intermediate => BrainfuckParser}
import java.io.{OutputStream, PrintStream}
import collection.mutable.ListBuffer

/**
 * Created by IntelliJ IDEA.
 * User: UberMouse
 * Date: 5/12/12
 * Time: 11:38 PM
 */

class BrainfuckParserSpec extends WordSpec {

  val support = afterWord("Support")
  val outStream = new OutputStream() {
    override def write(b: Int) {
      output += String.valueOf(b.asInstanceOf[Char])
    }
    override def write(b:Array[Byte], off:Int, len:Int) {
      output += new String(b, off, len)
    }
    override def write(b:Array[Byte]) {
      write(b, 0, b.length)
    }
  }
  val output = new ListBuffer[String]
  Console.setOut(outStream)

  "The Brainfuck Parser" should support {
    "incrementing the Data Pointer with >" in {
      expect(1) {
        BrainfuckParser(">")
      }

      expect(5) {
        BrainfuckParser(">>>>>")
      }
    }

    "decrementing the Data Pointer with <" in {
      expect(1) {
        BrainfuckParser(">><")
      }

      expect(5) {
        BrainfuckParser(">>>>>>><<")
      }
    }

    "incrementing the byte at the current Data Pointer with +" in {
      expect(1) {
        BrainfuckParser("+")
        BrainfuckParser.cells(0)
      }

      expect(5) {
        BrainfuckParser("+++++")
        BrainfuckParser.cells(0)
      }
    }

    "decrementing the byte at the current Data Pointer with -" in {
      expect(-1) {
        BrainfuckParser("-")
        BrainfuckParser.cells(0)
      }

      expect(-5) {
        BrainfuckParser("-----")
        BrainfuckParser.cells(0)
      }
    }

    "the ability to output an ASCII character using the value the current DataPointer refers to with ." in {
      expect("!") {
        BrainfuckParser("+++++++++++++++++++++++++++++++++.")
        output(0)
      }
    }

    "the ability to store a byte of input at the current Data Pointer with ," in {
      pending
    }

    "forward command jumping to after the next ] with [ on a 0 byte pointer" in {
      pending
    }

    "backward command jumping to the previous [ with ] on a positive byte pointer" in {
      pending
    }
  }

}