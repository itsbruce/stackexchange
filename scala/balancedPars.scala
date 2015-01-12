// http://codereview.stackexchange.com/questions/75028/checking-for-balanced-parentheses/

def balance(chars: List[Char]): Boolean = {
    def go(cs: List[Char], level: Int): Boolean = cs match {
        case Nil => level == 0
        case ')' :: _ if level < 1 => false
        case ')' :: xs => go(xs, level - 1)
        case '(' :: xs => go(xs, level + 1)
        case _ :: xs => go(xs, level)
    }
    go(chars, 0)
}
