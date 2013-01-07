package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

 

  /**
   * Exercise 1
   * Caclulaton of element on Pascal Triangle.
   */
  def pascal(c: Int, r: Int): Int = {
    def fact(c: Int): Int = if (c == 0) 1 else c * fact(c-1)

    fact(r) / (fact(r-c) * fact(c))
  }

  /**
   * Exercise 2
   * Caculation if string has correct brackets.
   */
  def balance(chars: List[Char]): Boolean = {
    def equal_number_of_brackets(chars: List[Char]): Boolean = chars.count(c => c == '(') == chars.count(c => c == ')')

    def find_open(chars: List[Char]): Int = chars.indexOf('(')
    def find_close(chars: List[Char]): Int = chars.lastIndexOf(')')
    def has_brackets(chars: List[Char]): Boolean = chars.indexOf('(') >= 0 || chars.lastIndexOf('(') >= 0
    def closing_bracket_position(chars: List[Char]):Int = 1

    def subexpressions(chars: List[Char]) = {
      // Return list of subexpression inside brackets in string.
      // In brackets are incorrect - return an exception.
      val open_pos = find_open(chars)
      val close_pos = open_pos + closing_bracket_position(chars.drop(open_pos+1))
      val expr_inside_brackets = chars.slice(open_pos+1, close_pos+open_pos)
      val rest = chars.slice(close_pos + 1, chars.length)
      print("First expression: ")
      println(expr_inside_brackets)

      print("Rest of expression: ")
      println(rest)

      true
      // List(List('1'))
    }

    def parse_list(chars: List[Char]): Boolean = { 
      if (has_brackets(chars)) {
        print("There are brackets. ")
        println("Subexpressions:")
        println("   " + subexpressions(chars))
        // subexpressions(chars).foreach(println(_)) 
        false
        }
      else { 
        println("No brackets")
        true
      }
    }

    println("================")
    println(chars)
    if (equal_number_of_brackets(chars)) {println("Brackets test passed."); true}
    else {println("Brackets test failed."); false}
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
