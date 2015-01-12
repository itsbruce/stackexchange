// http://codereview.stackexchange.com/questions/77050/list-containsslice

def hasSublist[T](list: List[T], sub: List[T]): Boolean = {
  def matchSub(ls: List[T], ss: List[T]): Option[Boolean] = (ls, ss) match {
    case (_, Nil) => Some(true)
    case (Nil, _) => None
    case (x :: xs, y :: ys) if x == y => matchSub(xs, ys)
    case (_, _) => Some(false)
  }

  matchSub(list, sub) match {
    case None => false
    case Some(false) => hasSublist(list.tail, sub)
    case _ => true
 }
}

/* 
 * map and getOrElse instead of pattern matching
 *
  matchSub(list, sub) map { found => if (found) true else hasSublist(list.tail, sub) } getOrElse false
}
 *
 * fold instead of pattern matching
 *
  matchSub(list, sub).fold(false) (found => if (found) true else hasSublist(list.tail, sub))
}
*/

