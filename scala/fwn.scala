//http://stackoverflow.com/questions/18783147/

def filterWithNeighbours[E](l: List[E])(p: E => Boolean) = l match {
  case Nil => Nil
  case li if li.size < 3 => if (l exists p) l else Nil
  case _ => ((l.head :: l) zip (l.tail :+ l.last)) zip l collect {
    case ((a, b), c) if (p (a) || p (b) || p (c) ) => c
  }
}

val l = List(1, 1, 1, 10, 2, 1, 1, 1)

def test (i: Int) = i >= 10

((l.head :: l) zip (l.tail :+ l.last)) zip l filter {
  case ((a, b), c) => (test (a) || test (b) || test (c) )
} map { case ((a, b), c ) => c }
