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
  // Implicit classes added in Scala 2.10
  implicit class FractionalInt(i: Int) {
    def + (that: Fraction) = that + i
  }
}
