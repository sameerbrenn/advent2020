import scala.io.Source
import scala.util.Try

object DayOne extends App {
  def findSum(input: Seq[Long]): Try[Long] = {
    val maybeResult: Either[Set[Long], Long] = input.foldLeft[Either[Set[Long], Long]](Left(Set())) {
      case (Left(set), value) => if (set.contains(2020 - value)) {
        Right(value * (2020 - value))
      } else {
        Left(set + value)
      }
      case (Right(result), _) =>
        Right(result)
    }

    val leftException: Either[RuntimeException, Long] = maybeResult.left.map { _ =>
      new RuntimeException("No valid result")
    }

    leftException.toTry
  }

  val source = Source.fromFile("src/main/scala/day1.txt")
  val input = source.getLines().toSeq.map(_.toLong)
  println(findSum(input).get)
  source.close()
}
