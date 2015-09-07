// http://codereview.stackexchange.com/questions/102470/binary-tree-implementation-in-scala

sealed abstract class BinaryTree[+A] {
  def isEmpty: Boolean
  def isValid: Boolean
}

case object EmptyTree extends BinaryTree[Nothing] {
  def isEmpty = true
  def isValid = true
}

case class NonEmptyTree[A](
    var data: A,
    var left: BinaryTree[A],
    var right: BinaryTree[A])
    (implicit ord: Ordering[A]) extends BinaryTree[A] {
  def isEmpty = false
  def isValid: Boolean = {
    import ord._
    def isValidWith(f: A => Boolean, t: BinaryTree[A]): Boolean = t match {
      case NonEmptyTree(that, _, _) => f(that) && t.isValid
      case EmptyTree => true
    }
    isValidWith(data < _, left) && isValidWith(data > _, right)
  }
}

/* isValid is actually pointless.  I explain in my answer that the OP
 * should create an add method to provide a real implementation of a
 * binary tree
 */
