import scala.io.Source
import scala.util.Try

object DayOne extends App {
  def findSum(input: Seq[Long]): Try[Long] = {
    val maybeResult: Either[Set[Long], Long] = input.foldLeft[Either[Set[Long], Long]](Left(Set())) {
      case (maybeSet, value) => maybeSet.left.flatMap { set =>
        if (set.contains(2020 - value)) {
          Right(value * (2020 - value))
        } else {
          Left(set + value)
        }
      }
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
