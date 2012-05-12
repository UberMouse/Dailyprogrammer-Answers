package nz.ubermouse.dailyprogrammer.challenge39

/**
 * Created by IntelliJ IDEA.
 * User: UberMouse
 * Date: 4/13/12
 * Time: 10:55 PM
 */

object FizzBuzz_easy {

  def main(args: Array[String]) {
    fizzbuzz(20)
  }

  def fizzbuzz(times: Int) {
    val fizzbuzz = (i: Int) => {
      if (i % 3 == 0 && i % 5 == 0)
        "fizzbuzz"
      else if (i % 5 == 0)
        "buzz"
      else if (i % 3 == 0)
        "fizz"
      else
        i.toString

    }
    (1 to times).map(fizzbuzz).foreach(println)
  }

}