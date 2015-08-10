// http://codereview.stackexchange.com/questions/99211/scala-fraction-addition

class Fraction(c: BigInt, d: BigInt) {
  val divisor = c.gcd(d);
  def counter = c / divisor;
  def denominator = d / divisor;

  def lcm(a: BigInt, b: BigInt):BigInt = (a * b).abs / a.gcd(b)

  def + (f: Fraction): Fraction = {
    val lcmVal = lcm(f.denominator, denominator)
    val apply = (count: BigInt, div: BigInt) => lcmVal / div * count
    new Fraction(
      apply(f.counter, f.denominator)
        + apply(counter, denominator)
      , lcmVal)
  }

  def + (i: Int): Fraction = this + (new Fraction(i, 1))

  override def toString(): String = {
    return String.format("%s / %s", counter, denominator)
  }
}

object Fraction {
  def apply(c: BigInt, d: BigInt) = new Fraction(c, d)
  def apply(i: Int) = new Fractioin(i, 1)

  implicit object NumericFraction extends Numeric[Fraction] {
    // Required by scala.math.Numeric
    def fromInt(x: Int): Fraction = Fraction(x, 1)
    def minus(x: Fraction, y: Fraction): Fraction = x + negate(y)
    def negate(x: Fraction) = Fraction( - x.counter, x.denominator)
    def plus(x: Fraction, y: Fraction): Fraction = x + y
    def times(x: Fraction, y: Fraction): Fraction =
      Fraction(x.counter * y.counter, x.denominator * y.denominator)
    def toDouble(x: Fraction): Double = x.counter.toDouble / x.denominator.toDouble
    def toFloat(x: Fraction): Float = x.counter.toFloat / x.denominator.toFloat
    def toInt(x: Fraction): Int = (x.counter / x.denominator).toInt
    def toLong(x: Fraction): Long = (x.counter / x.denominator).toLong
    // Required by scala.math.Ordering
    def compare(x: Fraction, y: Fraction) = (x.counter * y.denominator) compare (y.counter * x.denominator)
  }

  // Allows Int + Fraction
  implicit class FractionalInt(i: Int) {
    def + (that: Fraction) = that + i
  }
}
