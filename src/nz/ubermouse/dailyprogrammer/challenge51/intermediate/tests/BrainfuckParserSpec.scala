package nz.ubermouse.dailyprogrammer.challenge51.intermediate.tests

import org.scalatest.WordSpec
import nz.ubermouse.dailyprogrammer.challenge51.intermediate.BrainfuckParser

/**
 * Created by IntelliJ IDEA.
 * User: UberMouse
 * Date: 5/15/12
 * Time: 3:30 PM
 */

class BrainfuckParserSpec extends WordSpec {
  "The Brainfuck parser" should {
    "transform characters into Node representations" in {
      expect("List(PtrAdd(1))") {
        BrainfuckParser(">").toString()
      }

      expect("List(PtrSub(1))") {
        BrainfuckParser("<").toString()
      }

      expect("List(DataAdd(1))") {
        BrainfuckParser("+").toString()
      }

      expect("List(DataSub(1))") {
        BrainfuckParser("-").toString()
      }

      expect("List(PrintChar())") {
        BrainfuckParser(".").toString()
      }

      expect("List(ReadChar())") {
        BrainfuckParser(",").toString()
      }
    }

    "optimize away multiples of the same expression into one" in {
      expect("List(PtrAdd(5))") {
        BrainfuckParser(">>>>>").toString()
      }

      expect("List(PtrSub(5))") {
        BrainfuckParser("<<<<<").toString()
      }

      expect("List(DataAdd(5))") {
        BrainfuckParser("+++++").toString()
      }

      expect("List(DataSub(5))") {
        BrainfuckParser("-----").toString()
      }
    }

    "optimize and parse complex expressions" in {
      expect("List(DataAdd(1), ")
    }
  }
}