package nz.ubermouse.dailyprogrammer.challenge51.intermediate.tests

import org.scalatest.WordSpec
import java.io.{InputStream, OutputStream, PrintStream}
import collection.mutable.{Queue, ListBuffer}
import nz.ubermouse.dailyprogrammer.challenge51.intermediate.BrainfuckInterpreter

/**
 * Created by IntelliJ IDEA.
 * User: UberMouse
 * Date: 5/12/12
 * Time: 11:38 PM
 */

class BrainfuckInterpreterSpec extends WordSpec {

  val support     = afterWord("support")
  val outStream   = new OutputStream() {
    override def write(b: Int) {
      output += String.valueOf(b.asInstanceOf[Char])
    }

    override def write(b: Array[Byte], off: Int, len: Int) {
      output += new String(b, off, len)
    }

    override def write(b: Array[Byte]) {
      write(b, 0, b.length)
    }
  }
  val inputStream = new InputStream {

    val inputQueue = new Queue[Byte]()
    inputQueue += 33
    inputQueue += 48

    def read() = inputQueue.dequeue()

    override def read(b: Array[Byte]) = read()

    override def read(b: Array[Byte], offset: Int, len: Int) = read()
  }
  val output      = new ListBuffer[String]
  Console.setIn(inputStream)


  "The Brainfuck Interpreter" should support {
    "incrementing the Data Pointer with >" in {
      expect(1) {
        BrainfuckInterpreter(">")._1
      }

      expect(5) {
        BrainfuckInterpreter(">>>>>")._1
      }
    }

    "decrementing the Data Pointer with <" in {
      expect(1) {
        BrainfuckInterpreter(">><")._1
      }

      expect(5) {
        BrainfuckInterpreter(">>>>>>><<")._1
      }
    }

    "incrementing the byte at the current Data Pointer with +" in {
      expect(1) {
        BrainfuckInterpreter("+")
        BrainfuckInterpreter.cells(0)
      }

      expect(5) {
        BrainfuckInterpreter("+++++")
        BrainfuckInterpreter.cells(0)
      }
    }

    "decrementing the byte at the current Data Pointer with -" in {
      expect(-1) {
       BrainfuckInterpreter("-")
       BrainfuckInterpreter.cells(0)
     }

      expect(-5) {
       BrainfuckInterpreter("-----")
       BrainfuckInterpreter.cells(0)
     }
    }

    "the ability to output an ASCII character using the value the current DataPointer refers to with ." in {
      val oldOut = Console.out
      Console.setOut(outStream)
      expect("!") {
        output.clear()
        BrainfuckInterpreter("+++++++++++++++++++++++++++++++++.")
        output(0)
      }

      expect("0") {
        output.clear()
        BrainfuckInterpreter("++++++++++++++++++++++++++++++++++++++++++++++++.")
        output(0)
      }
      Console.setOut(oldOut)
    }

    "the ability to store a byte of input at the current Data Pointer with ," in {
      expect("!") {
        //BrainfuckInterpreter(",.")
        //output(0)
        pending
      }

      expect("0") {
        //BrainfuckInterpreter(",.")
        //output(0)
        pending
      }
    }

    "forward command jumping to after the next ] with [ on a 0 byte pointer" in {
      expect(1) {
        BrainfuckInterpreter("[+++++]+")
        BrainfuckInterpreter.cells(0)
      }

      expect(4) {
        BrainfuckInterpreter("[+++++]++[++>]+")
        BrainfuckInterpreter.cells(0)
      }
    }

    "backward command jumping to the previous [ with ] on a positive byte pointer" in {
      expect(11) {
       BrainfuckInterpreter("+>++[<+++++>-]+")
       BrainfuckInterpreter.cells(0)
     }

      expect(8) {
        BrainfuckInterpreter("+>++[<+++++>-]++>+++[<++>-]+")
        BrainfuckInterpreter.cells(1)
      }
    }
  }

}