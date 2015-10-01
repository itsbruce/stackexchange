// http://codereview.stackexchange.com/questions/106135/extending-seqlike-move-an-element-to-the-front-of-the-sequence

import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

implicit class SeqLikeOps[A, F[_]](seq: SeqLike[A, F[A]])(implicit cbf: CanBuildFrom[F[A], A, F[A]]) {
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
  def bringToFront(a: A): F[A] = {
    val result = seq.iterator.foldLeft(NotFound(Iterator[A](), a): Step) {
      (s, a) => s.run(a)
    }
    result.iter.to[F]
  }

}
