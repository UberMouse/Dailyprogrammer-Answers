package nz.ubermouse.dailyprogrammer.challenge51.tests

import org.scalatest.WordSpec

/**
 * Created by IntelliJ IDEA.
 * User: UberMouse
 * Date: 5/12/12
 * Time: 11:38 PM
 */

class BrainfuckParserSpec extends WordSpec {

  val support = afterWord("Support")

  "The Brainfuck Parser" should support {
    "incrementing the Data Pointer with >" in {
      pending
    }

    "decrementing the Data Pointer with <" in {
      pending
    }

    "incrementing the byte at the current Data Pointer with +" in {
      pending
    }

    "decerementing the byte at the current Data Pointer with -" in {
      pending
    }

    "the ability to output an ASCII character using the value the current DataPointer refers to with ." in {
      pending
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