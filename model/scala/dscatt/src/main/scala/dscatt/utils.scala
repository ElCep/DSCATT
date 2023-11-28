package dscatt

package object utils {

  def gini(l: Seq[Double]) = {
    if (l.size == 0) println("Gini size should be > 0")
    val sumOfDifference = l.flatMap(v1 => l.map(v2 => Math.abs(v1 - v2))).sum
    val mean = l.sum / l.size
    if (mean == 0) 0.0
    else sumOfDifference / (2 * l.size * l.size * mean)
  }

  implicit class CSVWrapper(val prod: Seq[Seq[Any]]) extends AnyVal {
    def toCSV() = prod.map { l =>
      l.mkString(",")
    }.mkString("\n")
  }

  implicit def average(ts: Seq[Double]): Double = ts.sum / ts.size
  
  implicit def average(ts: Array[Double]): Double = ts.sum / ts.size
}