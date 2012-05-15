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

      expect("List(ForwardJump(1), BackJump(0))") {
        BrainfuckParser("[]").toString()
      }
    }

    "optimize away multiples of the same expression into one (where applicable)" in {
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
      expect("List(DataAdd(1), PtrAdd(1), DataAdd(5), PtrAdd(3), DataSub(10))") {
        BrainfuckParser("+>+++++>>>----------").toString()
      }

      expect("List(PtrAdd(5), DataAdd(2), DataSub(1), DataAdd(2), DataSub(1), DataAdd(1), DataSub(1), DataAdd(1), DataSub(1), DataAdd(1), PtrSub(4), PtrAdd(2), DataSub(2), DataAdd(1))") {
        BrainfuckParser(">>>>>++-++-+-+-+<<<<>>--+").toString()
      }
    }

    "find all jump points in the code" in {
      expect(Map(1 -> 4, 4 -> 1)) {
        BrainfuckParser.findJumps("++[++-]--".replaceAll(">+", ">")
                            .replaceAll("<+", "<")
                            .replaceAll("\\++", "+")
                            .replaceAll("-+", "-"))
      }

      expect(Map(14 -> 17, 6 -> 12, 17 -> 14, 12 -> 6, 11 -> 8, 8 -> 11)) {
        BrainfuckParser
        .findJumps(">>+<-->+[++[-->]]--[<<<+]".replaceAll(">+", ">")
                            .replaceAll("<+", "<")
                            .replaceAll("\\++", "+")
                            .replaceAll("-+", "-"))
      }
    }
  }
}