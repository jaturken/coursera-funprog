object times {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(58); 
  println("Welcome to the Scala worksheet");$skip(463); 
   def times(chars: List[Char]): List[(Char, Int)] = {
    def char_with_freq(char: Char): (Char, Int) = (char, chars.count(_ == char))

    def unique[A](list: List[A]) = {
	  def loop(set: Set[A], ls: List[A]): List[A] = list match {
	    case head :: tail if set contains head => loop(set, tail)
	    case head :: tail => head :: loop(set + head, tail)
	    case Nil => Nil
	  }
	
	  loop(Set(), list)
	}
    
  unique(chars).map{ char_with_freq(_) }
    
  };System.out.println("""times: (chars: List[Char])List[(Char, Int)]""")}
 }