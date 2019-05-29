package skiing

import java.awt.Point

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

class ProcessMap {

  private var dim = new Point(0, 0)
  private var map = ArrayBuffer[Array[Int]]()
  private var mapChecked = Array[Array[Boolean]]()
  private var longRoute = ListBuffer[Point]()

  // Initialization
  {
    val file = Source.fromFile("map.txt")
    for (line ← file.getLines()) {
      val nums = line.split(" ").map(_.toInt)

      if (dim.x == 0) { // First line
        dim = new Point(nums.head, nums(1))
      } else {
        map += nums
      }
    }
    file.close()
    mapChecked = Array.fill(dim.y, dim.x)(false) // Create a matrix of [dim.x * dim.y] with 'false' values
  }

  def start(): Unit = {
    for (y ← 0 until dim.y) {
      for (x ← 0 until dim.x) {
        if (!mapChecked(y)(x)) {
          check(new Point(x, y), ListBuffer()) // Start checking new route
        }
      }
    }

    val first = longRoute.head
    println(s"(${first.x}, ${first.y})=${map(first.y)(first.x)}")
    println(s"length: ${longRoute.size}")
    println(longRoute)
  }

  private def check(p: Point, route: ListBuffer[Point]) {
    mapChecked(p.y)(p.x) = true
    route += p
    val pVal = map(p.y)(p.x)

    if (
      (p.x == 0 || map(p.y)(p.x - 1) >= pVal) &&
        (p.x == dim.x - 1 || map(p.y)(p.x + 1) >= pVal) &&
        (p.y == 0 || map(p.y - 1)(p.x) >= pVal) &&
        (p.y == dim.y - 1 || map(p.y + 1)(p.x) >= pVal)
    ) {
      // End of this route
      if (route.size > longRoute.size) {
        longRoute = route.clone
      } else if (route.size == longRoute.size) {
        // Steepest vertical drop
        if (Math.abs(route.head.y - route.last.y) > Math.abs(longRoute.head.y - longRoute.last.y)) {
          longRoute = route.clone
        }
      }
    } else {
      // To right
      if (p.x + 1 < dim.x) {
        if (map(p.y)(p.x + 1) < pVal) {
          check(new Point(p.x + 1, p.y), route)
        }
      }

      // To left
      if (p.x > 0) {
        if (map(p.y)(p.x - 1) < pVal) {
          check(new Point(p.x - 1, p.y), route)
        }
      }

      // To down
      if (p.y + 1 < dim.y) {
        if (map(p.y + 1)(p.x) < pVal) {
          check(new Point(p.x, p.y + 1), route)
        }
      }

      // To Up
      if (p.y > 0) {
        if (map(p.y - 1)(p.x) < pVal) {
          check(new Point(p.x, p.y - 1), route)
        }
      }
    }

    route.remove(route.size - 1)
  }
}
