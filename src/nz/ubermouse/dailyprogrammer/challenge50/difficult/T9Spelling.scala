package nz.ubermouse.dailyprogrammer.challenge50.difficult

/**
 * Created by IntelliJ IDEA.
 * User: UberMouse
 * Date: 5/10/12
 * Time: 6:11 PM
 */

object T9Spelling {

  val mappings = Map("a" -> "2",
                     "b" -> "22",
                     "c" -> "222",
                     "d" -> "3",
                     "e" -> "33",
                     "f" -> "333",
                     "g" -> "4",
                     "h" -> "44",
                     "i" -> "444",
                     "j" -> "5",
                     "k" -> "55",
                     "l" -> "555",
                     "m" -> "6",
                     "n" -> "66",
                     "o" -> "666",
                     "p" -> "7",
                     "q" -> "77",
                     "r" -> "777",
                     "s" -> "7777",
                     "t" -> "8",
                     "u" -> "88",
                     "v" -> "888",
                     "w" -> "9",
                     "x" -> "99",
                     "y" -> "999",
                     "z" -> "9999",
                     " " -> "0")

  def char2group(char: String) = {
    if (mappings.contains(char)) mappings(char).substring(0, 1)
    else "-1"
  }

  def parse(s: String, prevGroup: String = ""): String = {
    if (s.length == 1) {
      if (mappings.contains(s))
        (if (char2group(s) == prevGroup) " " else "") + mappings(s)
      else
        s
    }
    else {
      val substring = s.substring(0, 1)
      parse(substring, prevGroup) + parse(s.substring(1), char2group(substring))
    }

  }

}