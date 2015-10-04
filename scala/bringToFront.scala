// http://codereview.stackexchange.com/questions/106135/extending-seqlike-move-an-element-to-the-front-of-the-sequence

import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

implicit class SeqLikeOps[A, F[_]](seq: SeqLike[A, F[A]])(implicit ev: F[A] <:< SeqLike[A, F[A]]) {

// CPS-style for fun
  private sealed trait Step {
    def iter: Iterator[A]
    def run(x: A): Step
  }

  private case class Found(iter: Iterator[A]) extends Step {
    def run(x: A) = Found(iter ++ Iterator(x))
  }

  private case class NotFound(iter: Iterator[A], val a: A) extends Step {
    def run(x: A) =
      if (x == a)
        Found(Iterator(x) ++ iter)
      else
        NotFound(iter ++ Iterator(x), a)
  }

  def bringToFront(a: A)(implicit cbf: CanBuildFrom[F[A], A, F[A]]): F[A] = {
    val result = seq.iterator.foldLeft(NotFound(Iterator[A](), a): Step) {
      (s, a) => s.run(a)
    }
    result.iter.to[F]
  }

/* Recursion-free version of OP's solution.  Certainly involves less
 * traversal and fewer no builds (if value a is not present or already
 * at the front
 */
  def bTF(a: A)(implicit cbf: CanBuildFrom[F[A], A, F[A]]): F[A] = {
    seq.indexOf(a) match {
      case i if i > 1 => {
        val b = cbf()
        b.sizeHint(seq)
        b += a
        b ++= seq take i
        b ++= seq drop (i + 1)
        b.result
      }
      case _ => seq.repr
    }
  }

// Variant on the above which only ever traverses once
  def bTFi(a: A)(implicit cbf: CanBuildFrom[F[A], A, F[A]]): F[A] = {
    val iter = seq.iterator
    val pred = iter.takeWhile(_ != a).to[F]
    if (iter.isEmpty)
      seq.repr
    else {
      val b = cbf()
      b.sizeHint(seq)
      b += a
      b ++= pred
      b ++= iter
      b.result
    }
  }

// Less "safe" variant
   def bTFi2(a: A)(implicit cbf: CanBuildFrom[F[A], A, F[A]]): F[A] = {
    val iter = seq.iterator
    val pred = iter.takeWhile(_ != a)
    val b = cbf()
    b.sizeHint(seq)
    b += a
    b ++= pred
    if (iter.isEmpty)
      seq.repr
    else {
      b ++= iter
      b.result
    }
  }

// And another, even simpler one
  def bTFi3(a: A)(implicit cbf: CanBuildFrom[F[A], A, F[A]]): F[A] = {
    val iter = seq.iterator
    val pred = iter.takeWhile(_ != a)
    (Iterator(a) ++ pred ++ iter).to[F]
  }
}
