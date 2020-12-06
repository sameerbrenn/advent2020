import scala.io.Source

object DayThree extends App {
  def stringToRow(in: String): Seq[Boolean] = {
    in.map {
      case '.' => false
      case '#' => true
    }
  }

  def traverse(rows: Seq[Seq[Boolean]], right: Int, down: Int): Long = {
    val width = rows.head.length
    val positions = LazyList.iterate(0 -> 0) {
      case (col, row) =>
        ((col + right) % width) -> (row + down)
    }
    positions.takeWhile {
      case (_, row) => row < rows.length
    }.map {
        case (col, row) =>
          rows(row)(col)
      }.count(identity)
    }

  val source = Source.fromFile("src/main/scala/day3.txt")
  val rows = source.getLines().toSeq.map(stringToRow)
  println(traverse(rows, 3, 1))

  println(traverse(rows, 1, 1) *
    traverse(rows, 3, 1) *
    traverse(rows, 5, 1) *
    traverse(rows, 7, 1) *
    traverse(rows, 1, 2)
  )

  source.close()
}
