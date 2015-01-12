def calculateBags(as: List[Int], bs: List[Int]): Double = {
    val (xs, ys) = if (as.size <= bs.size) (as, bs) else (bs, as)

    val xCounts = xs.groupBy(x => x).map(x => x._1 -> x._2.size)

    val mins = ys.foldLeft(Map(): Map[Int, Int]) {(ms, y) =>
      lazy val yCount: Int = ms.getOrElse(y,0)
      if (xCounts.contains(y) && xCounts(y) > yCount) {
        ms.updated(y, yCount + 1)
      } else {
        ms
      }
    }

    mins.valuesIterator.sum.toDouble / (as.size + bs.size)
}   
