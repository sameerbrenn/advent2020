import scala.io.Source

object DayTwo extends App {
  case class PasswordLine(min: Int, max: Int, letter: Char, password: String)

  def countLetters(letter: Char, str: String): Int = {
    str.toCharArray.count(_ == letter)
  }

  def lineToPasswordLine(line: String): PasswordLine = {
    val Array(rule, password) = line.split(":")
    val Array(range, char) = rule.split(" ")
    val Array(min, max) = range.split("-").map(_.toInt)
    PasswordLine(min, max, char.toCharArray.head, password.trim)
  }

  def count(input: Seq[String]): Int = {
    input.count { line =>
      val passwordLine: PasswordLine = lineToPasswordLine(line)
      val count = countLetters(passwordLine.letter, passwordLine.password)
      count >= passwordLine.min && count <= passwordLine.max
    }
  }
  val source = Source.fromFile("src/main/scala/day2.txt")
  val input = source.getLines().toSeq
  println(count(input))
  source.close()
}
