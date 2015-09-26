// http://codereview.stackexchange.com/questions/105126/project-euler-4-the-functional-way

// This example has a few too many magic numbers.

// Lazily generate all 999 palindromic 6-digit numbers in reverse order
def ps6 = for (i <- Iterator.from(9, -1) take 9;
              j <- Iterator.from(9, -1) take 10;
              k <- Iterator.from(9, -1) take 10)
              yield (i + 10 * j + 100 * k + 1000 * k + 10000 * j + 100000 * i)

// Even more lazy generation of the 5-digit palindromes
def ps5 = ps6 map (i => (i / 1000) * 100 + i % 100)

// Put them together.  Iterator ++ is lazy
def ps = (ps6 dropWhile (_ > 999 * 999)) ++ (ps5 takeWhile (_ > 9999))

/* Simple way to look for palindromes which are multiples of two 3 digit
 * numbers.
 */
ps find {p =>
  Iterator.from(999, -1) take 900 takeWhile (p / _ < 1000) exists (p % _ == 0)
}

