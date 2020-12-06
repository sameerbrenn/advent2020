import scala.io.Source

object DayOnePart2 extends App {
  def findTripleSum(input: Seq[Long]): Option[Long] = {
    val pairs = for (x <- input; y <- input) yield (x, y)
    val sumsAndProducts = pairs.map {
      case (a, b) => a + b -> a * b
    }.toMap
    input.find { candidate =>
      sumsAndProducts.contains(2020 - candidate)
    }.map { value =>
      sumsAndProducts(2020 - value) * value
    }
  }

  val source = Source.fromFile("src/main/scala/day1.txt").getLines()
  val input = source.toSeq.map(_.toLong)
  println(findTripleSum(input).getOrElse(throw new RuntimeException("No valid result")))
}
