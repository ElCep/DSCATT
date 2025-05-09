package dscatt

import scala.util.Random
import scala.math.{Numeric, Fractional}
import scala.math.Numeric.Implicits._

package object utils {

  def gini(values: Seq[Double]) =
    val n = values.length
    val mean = values.sum / n
    if mean == 0
    then 0.0
    else
      val diffSum = values.flatMap(x => values.map(y => math.abs(x - y))).sum
      diffSum / (2 * n * n * mean)


  trait FromDouble[A] {
    def fromDouble(d: Double): A
  }

  object FromDouble {
    given FromDouble[Double] with
      def fromDouble(d: Double): Double = d

    given FromDouble[Float] with
      def fromDouble(d: Double): Float = d.toFloat

    given FromDouble[Int] with
      def fromDouble(d: Double): Int = d.toInt

    given FromDouble[Long] with
      def fromDouble(d: Double): Long = d.toLong
  }

  import FromDouble.given

  def collectionWithGini[A](targetGini: Double,
                            collectionSize: Int,
                            elementsSumOrMax: Either[A, A],
                            tolerance: Double = 0.02,
                            maxAttempts: Int = 200)
                           (using num: Numeric[A], fromDouble: FromDouble[A]): Seq[A] = {

    val rand = new Random()
    val isFractional = num.isInstanceOf[Fractional[A]]

    def randomUpTo(max: A): A =
      if isFractional then
        val maxD = num.toDouble(max)
        fromDouble.fromDouble(rand.nextDouble() * maxD)
      else
        val maxInt = math.max(1, num.toInt(max))
        num.fromInt(rand.nextInt(maxInt + 1))

    def collectionWithGini0(attempts: Int, result: Seq[A], g: Double): Seq[A] =
      if attempts >= maxAttempts || math.abs(g - targetGini) <= tolerance then
        println(s"# attempts: $attempts")
        result
      else

        def fillSeq(filled: Seq[A], loopNumber: Int): Seq[A] =
          if loopNumber == collectionSize then filled
          else {
            val nextEl = elementsSumOrMax match {
              case Left(total) =>
                val remaining = num.minus(total, filled.sum)
                val slotsLeft = num.fromInt(collectionSize - loopNumber)
                val maxForEl = num.minus(remaining, slotsLeft)
                randomUpTo(maxForEl)
              case Right(maxVal) =>
                randomUpTo(maxVal)
            }
            fillSeq(filled :+ nextEl, loopNumber + 1)
          }

        val seq = fillSeq(Seq(), 1)

        val lastValue = elementsSumOrMax match {
          case Left(total) => num.minus(total, seq.sum)
          case Right(maxVal) => randomUpTo(maxVal)
        }

        val values = seq :+ lastValue
        val giniVal = gini(values.map(num.toDouble))
        collectionWithGini0(attempts + 1, values, giniVal)

    collectionWithGini0(0, Seq.empty[A], 1000.0)
  }



  implicit class CSVWrapper(val prod: Seq[Seq[Any]]) extends AnyVal {
    def toCSV() = prod.map { l =>
      l.mkString(",")
    }.mkString("\n")
  }

  implicit def average(ts: Seq[Double]): Double = ts.sum / ts.size
  
  implicit def average(ts: Array[Double]): Double = ts.sum / ts.size
}