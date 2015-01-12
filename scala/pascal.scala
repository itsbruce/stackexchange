// http://stackoverflow.com/questions/18945141/convert-normal-recursion-to-tail-recursion/

def pascal(column: Int, row: Int):Option[Int] = (column, row) match {
  case (c, r) if (c > r + 1) => None
  case (c, r) if (c == 0) || (c == r) => Some(1)
  case (c, r) => for {x <- pascal(c - 1, r - 1); y <- pascal(c, r - 1)} yield (x + y)
}

def pascal(column: Long, row: Long):Long = {
  type Point = (Long, Long)
  type Points = List[Point]
  type Triangle = Map[Point,Long]
  def above(p: Point) = (p._1, p._2 - 1)
  def aboveLeft(p: Point) = (p._1 - 1, p._2 - 1)
  def find(ps: Points, t: Triangle): Long = ps match {
    // Found the ultimate goal
    case ((c, r) :: Nil) if t contains (c, r) => t(c, r)
    // Found an intermediate point: pop the stack and carry on
    case ((c, r) :: rest) if t contains (c, r) => find(rest, t)
    // Hit a triangle edge, add it to the triangle
    case ((c, r) :: _) if (c == 0) || (c == r) => find(ps, t + ((c,r) -> 1))
    // Triangle contains (c - 1, r - 1)...
    case (p :: _) if t contains aboveLeft(p) => if (t contains above(p))
        // And it contains (c, r - 1)!  Add to the triangle
        find(ps, t + (p -> (t(aboveLeft(p)) + t(above(p)))))
      else
        // Does not contain(c, r -1).  So find that
        find(above(p) :: ps, t)
    // If we get here, we don't have (c - 1, r - 1).  Find that.
    case (p :: _) => find(aboveLeft(p) :: ps, t)
  }
  require(column >= 0 && row >= 0 && column <= row)
  (column, row) match {
    case (c, r) if (c == 0) || (c == r) => 1
    case p => find(List(p), Map())
  }
}
