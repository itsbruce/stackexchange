// http://codereview.stackexchange.com/questions/105126/project-euler-4-the-functional-way

// Lazily generate all 999 palindromic 6-digit numbers in reverse order
val ps = for (i <- (9 to 1 by -1).iterator;
              j <- (9 to 0 by -1).iterator;
              k <- (9 to 0 by -1).iterator)
              yield (i + 10 * j + 100 * k + 1000 * k + 10000 * j + 100000 * i)

/* Simple way to look for palindromes which are multiples of two 3 digit
 * numbers.
 */
ps find {p =>
    (999 to 100 by -1).iterator takeWhile (p / _ < 1000) exists (p % _ == 0)
}
