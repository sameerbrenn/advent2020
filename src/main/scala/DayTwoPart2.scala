import scala.io.Source

object DayTwoPart2 extends App {
  case class PasswordLine(posOne: Int, posTwo: Int, letter: Char, password: String)

  def lineToPasswordLine(line: String): PasswordLine = {
    val Array(rule, password) = line.split(":")
    val Array(range, char) = rule.split(" ")
    val Array(posOne, posTwo) = range.split("-").map(_.toInt)
    PasswordLine(posOne, posTwo, char.toCharArray.head, password.trim)
  }

  def logicalXor(x: Boolean, y: Boolean): Boolean = {
    ( x || y ) && ! ( x && y )
  }
  def count(input: Seq[String]): Int = {
    input.count { line =>
      val passwordLine: PasswordLine = lineToPasswordLine(line)
      val one = passwordLine.password(passwordLine.posOne-1) == passwordLine.letter
      val two = passwordLine.password(passwordLine.posTwo-1) == passwordLine.letter
      logicalXor(one, two)
    }
  }
  val source = Source.fromFile("src/main/scala/day2.txt")
  val input = source.getLines().toSeq
  println(count(input))
  source.close()
}
