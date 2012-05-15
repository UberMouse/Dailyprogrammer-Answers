package nz.ubermouse.dailyprogrammer.challenge50.difficult.tests

import nz.ubermouse.dailyprogrammer.challenge50.difficult.T9Spelling
import org.scalatest.WordSpec

/**
 * Created by IntelliJ IDEA.
 * User: UberMouse
 * Date: 5/10/12
 * Time: 6:11 PM
 */

class T9Spelling extends WordSpec {

  "The T9 spelling object" should {
    "convert a string of text to it's representation on a T9 Keypad" in {
      val testString = "hello world"
      val result = "4433555 555666096667775553"
      expect(result) {
                       T9Spelling.parse(testString)
                     }
    }

    "accept non alphabetical characters in the input" in {
      val testString = "sup son!11!"
      val result = "777788707777666 66!11!"
      expect(result) {
                       T9Spelling.parse(testString)
                     }
    }
    "insert a space between characters of the same number group" in {
      val testString = "ss aa def"
      val result = "7777 777702 203 33 333"
      expect(result) {
                       T9Spelling.parse(testString)
                     }
    }
  }

}