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
    val rnd = new Random(seed)
    val num = implicitly[Numeric[A]]
    val fromDouble = implicitly[FromDouble[A]]
    val isInt = implicitly[ClassTag[A]].runtimeClass == classOf[Int]

    // Génération initiale centrée (moyenne 0) std 1
    val raw = Vector.fill(n)(rnd.nextDouble() - 0.5)
    val centered = {
      val mean = raw.sum / n
      raw.map(_ - mean)
    }
    val std = math.sqrt(centered.map(x => x * x).sum / n)
    val standardized = centered.map(_ / std)

    // Appliquer moyenne et écart-type cibles
    val scaled = standardized.map(x => x * e + meanTarget)

    val constrained: Seq[Double] = constraint match {
      case SumTarget(sum) =>
        val correction = (sum.toDouble - scaled.sum) / n
        scaled.map(_ + correction)

      case MaxElement(maxVal) =>
        // Ne pas rescaler, juste clipper après génération
        scaled.map(x => math.min(x, maxVal))
    }

    val rounded: Seq[A] = constrained.map(fromDouble)

    // Clip final (en Double) pour s'assurer que rien ne dépasse maxVal
    val finalSeq = constraint match {
      case MaxElement(maxVal) =>
        rounded.map { v =>
          val dv = num.toDouble(v)
          if (dv > maxVal) fromDouble(maxVal)
          else v
        }
      case _ => rounded
    }
    println("OO " + finalSeq)
    finalSeq
  }
}