package dscatt

import scala.util.Random
import scala.math.{Numeric, Fractional}
import scala.math.Numeric.Implicits._
import scala.reflect.ClassTag

package object utils {

  implicit class CSVWrapper(val prod: Seq[Seq[Any]]) extends AnyVal {
    def toCSV() = prod.map { l =>
      l.mkString(",")
    }.mkString("\n")
  }

  implicit def average(ts: Seq[Double]): Double = ts.sum / ts.size

  implicit def average(ts: Array[Double]): Double = ts.sum / ts.size

  def gini(values: Seq[Double]) =
    val n = values.length
    val mean = values.sum / n
    if mean == 0
    then 0.0
    else
      val diffSum = values.flatMap(x => values.map(y => math.abs(x - y))).sum
      diffSum / (2 * n * n * mean)

  def collectionWithGini(
                          targetGini: Double,
                          sequenceSize: Int,
                          sequenceConstraint: SequenceConstraint
                        ): Seq[Double] = {

    val rnd = new Random()

    // Génère une distribution exponentielle biaisée pour approximer le Gini
    val raw = Array.fill(sequenceSize)(math.pow(rnd.nextDouble(), 1 - targetGini))
    val total = raw.sum

    // Mise à l'échelle simple pour commencer
    val base = raw.map(_ * sequenceSize / total)

    // Applique la contrainte choisie
    val result = sequenceConstraint match {
      case SumTarget(sum) =>
        val correction = (sum.toDouble - base.sum) / sequenceSize
        base.map(_ + correction).toSeq
      case MaxElement(maxVal) =>
        base.map(x => math.min(x, maxVal)).toSeq
    }

    result
  }

  def computeGini(values: Seq[Double]): Double = {
    val sorted = values.sorted
    val n = values.size
    val cumSum = sorted.scanLeft(0.0)(_ + _).tail
    val sumY = cumSum.last
    if (sumY == 0) 0.0
    else {
      val gini = 1.0 - (2.0 / (n - 1)) * ((n - cumSum.indices.map(_ + 1).zip(cumSum).map {
        case (i, c) => c / sumY * i
      }.sum))
      gini
    }
  }


  trait FromDouble[A] extends (Double => A)

  object FromDouble {
    implicit val doubleFromDouble: FromDouble[Double] = (d: Double) => d
    implicit val intFromDouble: FromDouble[Int] = (d: Double) => d.round.toInt
  }

  def collectionWithMeanAndStd[A: Numeric : ClassTag : FromDouble](
                                                                    n: Int,
                                                                    meanTarget: Double,
                                                                    e: Double,
                                                                    constraint: SequenceConstraint,
                                                                    seed: Long
                                                                  ): Seq[A] = {
    n match
      case 0=> Seq[A]()
      case 1 =>
        val value = constraint match {
          case SumTarget(sum)    => sum.toDouble
          case MaxElement(max)   => math.min(meanTarget, max)
        }

        Seq(implicitly[FromDouble[A]].apply(math.max(0.0, value)))
      case _=>
        val rnd = new Random(seed)
        val num = implicitly[Numeric[A]]
        val fromDouble = implicitly[FromDouble[A]]
        val isInt = implicitly[ClassTag[A]].runtimeClass == classOf[Int]

        // Étape 1 : valeurs uniformes [0, 1]
        val raw = Vector.fill(n)(rnd.nextDouble())

        // Étape 2 : centrer à 0, std ≈ 1
        val rawMean = raw.sum / n
        val centered = raw.map(_ - rawMean)
        val std = math.sqrt(centered.map(x => x * x).sum / n)
        val standardized = centered.map(_ / std)

        // Étape 3 : appliquer écart-type et recentrer pour la bonne moyenne
        val scaled = standardized.map(x => x * e + meanTarget)

        // Étape 4 : forcer toutes les valeurs à être ≥ 0
        val nonNegative = scaled.map(x => math.max(0.0, x))

        // Étape 5 : appliquer contrainte
        val constrained: Seq[Double] = constraint match {
          case SumTarget(sum) =>
            val factor = sum.toDouble / nonNegative.sum
            nonNegative.map(_ * factor)

          case MaxElement(maxVal) =>
            nonNegative.map(x => math.min(x, maxVal))
        }

        // Étape 6 : conversion + ajustement somme si besoin
        val rounded: Seq[A] = constrained.map(fromDouble)

        (constraint, isInt) match {
          case (SumTarget(target), true) =>
            val intValues = rounded.map(num.toInt)
            val currentSum = intValues.sum
            val diff = target - currentSum
            val sortedIdx = constrained.zipWithIndex.sortBy { case (v, _) => v - v.round }.map(_._2)
            val updated = intValues.zipWithIndex.map {
              case (v, i) =>
                val delta = if (sortedIdx.take(math.abs(diff)).contains(i)) math.signum(diff) else 0
                v + delta
            }
            updated.map(num.fromInt)

          case _ =>
            rounded
        }
  }
}