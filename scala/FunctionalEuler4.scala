// http://codereview.stackexchange.com/questions/105126/project-euler-4-the-functional-way

// This example has a few too many magic numbers.

// Lazily generate all 999 palindromic 6-digit numbers in reverse order
def ps6 = for (i <- (9 to 1 by -1).iterator;
              j <- (9 to 0 by -1).iterator;
              k <- (9 to 0 by -1).iterator)
              yield (i + 10 * j + 100 * k + 1000 * k + 10000 * j + 100000 * i)

// Even more lazy generation of the 5-digit palindromes
def ps5 = ps6 map (i => (i / 1000) * 100 + i % 100)

// Put them together.  Iterator ++ is lazy
def ps = (ps6 dropWhile (_ > 999 * 999)) ++ (ps5 takeWhile (_ > 9999))

/* Simple way to look for palindromes which are multiples of two 3 digit
 * numbers.
 */
ps find {p =>
  (999 to 100 by -1).iterator takeWhile (p / _ < 1000) exists (p % _ == 0)
}

// Lazily generate all products of 3-digit numbers in descending order
def productsFrom(i: Int): Stream[Int] = {
  // Merge two descending streams in sorted order
  def descending(s1: Stream[Int], s2: Stream[Int]): Stream[Int] = {
    val (higher, lower) = s1 span (_ > s2.head)
    higher ++ s2.head #:: descending(lower, s2.tail)
  }
  val first = i * i
  first #:: (first - i) #::
    descending(productsFrom(i - 1), Stream.from(first - 2 * i, -i))
}

def findBiggestMultiple(pals: Stream[Int], products: Stream[Int]): Option[Int] =
  pals.headOption flatMap { x =>
    val rest = products dropWhile (_ > x)
    rest.headOption flatMap { y =>
      if (y == x) Some(x) else findBiggestMultiple(pals.tail, rest)
    }
  }


findBiggestMultiple(ps.toStream, productsFrom(999))

/*
  def descending(s1: Stream[Int], s2: Stream[Int]): Stream[Int] = {
    val (higher, lower) = s1 span (_ > s2.head)
    higher ++ s2.head #:: descending(lower, s2.tail)
  }
  */
