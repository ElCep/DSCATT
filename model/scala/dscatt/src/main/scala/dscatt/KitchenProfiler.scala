package dscatt

import dscatt.Cost.HerdGrazing
import dscatt.HerdSizeStrategy.LSUByArea
import dscatt.SwitchType.Mulching

sealed trait SequenceConstraint

case class SumTarget(sum: Int) extends SequenceConstraint

case class MaxElement(value: Double) extends SequenceConstraint

sealed trait DistributionBuilder

case class Gini(
                 kitchenGini: Double,
                 solidarityGini: Double,
                 soilCareGini: Double,
                 mutualizedHerdGrazingGini: Double,
                 faidherbiaGini: Double,
                 maxFaidherbia: Int,
                 breederGini: Double,
                 maxBreeder: Double,
               ) extends DistributionBuilder

case class MeanStd(
                    kitchenMean: Double,
                    kitchenStd: Double,
                    solidarityMean: Double,
                    solidarityStd: Double,
                    soilCareMean: Double,
                    soilCareStd: Double,
                    mutualizedHerdGrazingMean: Double,
                    mutualizedHerdGrazingStd: Double,
                    faidherbiaMean: Double,
                    faidherbiaStd: Double,
                    maxFaidherbia: Int,
                    breederMean: Double,
                    breederStd: Double,
                    maxBreeder: Double,
                  ) extends DistributionBuilder

case class Distributions(
                          kitchen: Seq[Int],
                          solidarity: Seq[Int],
                          soilCare: Seq[Int],
                          mutualizeGrazing: Seq[Int],
                          faidherbia: Seq[Int],
                          breeder: Seq[Double]
                        )

case class KitchenProfiler(
                            kitchenPartition: KitchenPartition,
                            soilCareScore: Double,
                            solidarityScore: Double,
                            herdGrazingScore: Double,
                            faidherbiaScore: Double,
                            breederScore: Double
                          )

object KitchenProfiler:

  import org.json4s._
  import org.json4s.JsonDSL._
  import org.json4s.jackson.JsonMethods._

  def kitchenPartitionJsonFormat(kitchenPartition: KitchenPartition) =
    kitchenPartition.profiles.map: p =>
      ("id" -> p._1.id) ~
      ("numberOfKitchens" -> p._2) ~
      ("kitchenSize" -> p._1.kitchenSize) ~
      ("rotationCycle" -> p._1.rotationCycle.toString) ~
      ("ownFallowUse" -> p._1.ownFallowUse.toString) ~
      ("loanStrategy" -> p._1.loanStrategy.toString) ~
      ("foodDonationStrategy" -> p._1.foodDonationStrategy.toString) ~
      ("drySeasonHerdStrategy" -> p._1.drySeasonHerdStrategy.toString) ~
      ("wetSeasonHerdStrategy" -> p._1.wetSeasonHerdStrategy.toString) ~
      ("herdSizeStrategy" -> p._1.herdSizeStrategy.toString) ~
      ("mulchingStrategy" -> p._1.mulchingStrategy.toString) ~
      ("nbFaidherbia" -> p._1.nbFaidherbia)


  def toJsonContent(kitchenPartition: KitchenPartition) =
    import org.json4s._
    import org.json4s.JsonDSL._
    import org.json4s.jackson.JsonMethods._

    pretty(render(KitchenProfiler.kitchenPartitionJsonFormat(kitchenPartition)))

  def toJsonFile(kitchenPartition: KitchenPartition, path: String): java.io.File =

    val file = better.files.File(path)
    file.write(toJsonContent(kitchenPartition))
    file.toJava

  /*
  1- solidarity // loan - foodDonation (6)
  2- soilCare // cropRotation - ownFallowUse - mulching (16)
  3- herdGrazing // dryHerdGrazing - wetHerdGrazing (9)
  4- herdUsage // LSU (contiunous)
  5- faidherbia // #faidherbia (12)
  */

  //1
  val sortedLoanModalities =
    Seq(
      LoanStrategy.Selfish,
      LoanStrategy.ExtraParcelsExceptFallowLoaner,
      LoanStrategy.AllExtraParcelsLoaner
    )

  val sortedFoodDonationModalities =
    Seq(
      FoodDonationStrategy.FoodForUsOnlyStrategy,
      FoodDonationStrategy.FoodForAllStrategy
    )

  val solidarityModalities =
    for l <- sortedLoanModalities
        f <- sortedFoodDonationModalities
    yield (l, f)

  //2
  val sortedCultureDiversityModalities =
    Seq(
      RotationCycle.MilletOnly,
      RotationCycle.MilletPeanut,
      RotationCycle.MilletFallow,
      RotationCycle.FallowMilletPeanut
    )

  val sortedOwnFallowUseModalities =
    Seq(
      OwnFallowUse.UseFallowIfNeeded,
      OwnFallowUse.NeverUseFallow
    )
  val sortedMulchingModalities =
    Seq(
      MulchingStrategy.NoMulching,
      MulchingStrategy.CropResidue
    )

  val soilCareQModalities =
    for rc <- sortedCultureDiversityModalities
        fu <- sortedOwnFallowUseModalities
        m <- sortedMulchingModalities
    yield (rc, fu, m)

  //3 dry / wet
  val sortedMutualizedHerdGrazingModalities =
    Seq(
      HerdGrazingStrategy.OwnerOnly,
      HerdGrazingStrategy.EverywhereByDayOwnerByNight,
      HerdGrazingStrategy.AnywhereAnyTime
    )

  //dry / wet
  val mutualizedHerdGrazingModalities =
    for dg <- sortedMutualizedHerdGrazingModalities
        wg <- sortedMutualizedHerdGrazingModalities
    yield (dg, wg)

  def solidarityScore(loanStrategy: LoanStrategy, foodDonationStrategy: FoodDonationStrategy) =
    solidarityModalities.indexOf((loanStrategy, foodDonationStrategy))

  def soilCareScore(rotationCycle: RotationCycle, ownFallowUse: OwnFallowUse, mulchingStrategy: MulchingStrategy) =
    sortedCultureDiversityModalities.indexOf((rotationCycle, ownFallowUse, mulchingStrategy))

  def mutualizedHerdGrazingScore(dryGrazingStrategy: HerdGrazingStrategy, wetGrazingStrategy: HerdGrazingStrategy) =
    sortedMutualizedHerdGrazingModalities.indexOf((dryGrazingStrategy, wetGrazingStrategy))

  def score[A](modalityDistribution: Seq[A], kitchenDistribution: Seq[Int])(using num: Numeric[A]): Double =

    val totalKitchens = kitchenDistribution.sum.toDouble

    modalityDistribution
      .zip(kitchenDistribution)
      .map { case (modality, kitchenCount) =>
        num.toDouble(modality) * kitchenCount / totalKitchens
      }
      .sum

  def printDistributionsAndScores(distributions: Distributions) =

    println("Solidarity dist " + distributions.solidarity)
    println("Soil care dist " + distributions.soilCare)
    println("Grazing dist " + distributions.mutualizeGrazing)
    println("Faid dist " + distributions.faidherbia)
    println("Breeder dist " + distributions.mutualizeGrazing)

    println("sol score:  " + score[Int](distributions.solidarity, distributions.kitchen))
    println("soil care score:  " + score[Int](distributions.soilCare, distributions.kitchen))
    println("mutualized grazing score " + score[Int](distributions.mutualizeGrazing, distributions.kitchen))
    println("faid score:  " + score[Int](distributions.faidherbia, distributions.kitchen))
    println("breeder score:  " + score[Double](distributions.breeder, distributions.kitchen))
  
  def build(
             nbKitchenProfile: Int,
             initialTotalNumberOfKitchen: Int,
             initialKitchenSize: Int,
             drySeasonManureCriteria: (Parcel, RotationCycle) => Boolean,
             distributionBuilder: DistributionBuilder,
             seed: Long
           ) =

    val distributions =
      distributionBuilder match
        case gini: Gini =>
          val kitchenDist =
            val dist = utils.collectionWithGini(targetGini = gini.kitchenGini, sequenceSize = nbKitchenProfile, sequenceConstraint = SumTarget(initialTotalNumberOfKitchen))
            val zero = dist.indexOf(0)
            if zero > 0
            then
              val max = dist.max
              dist.updated(zero, 1.0).updated(dist.indexOf(max), max - 1.0).map(_.toInt)
            else dist.map(_.toInt)

          val solidarityDist = utils.collectionWithGini(gini.solidarityGini, nbKitchenProfile, MaxElement(solidarityModalities.size - 1))
          val soilCareDist = utils.collectionWithGini(gini.soilCareGini, nbKitchenProfile, MaxElement(soilCareQModalities.size - 1))
          val mutualizedGrazingDist = utils.collectionWithGini(gini.mutualizedHerdGrazingGini, nbKitchenProfile, MaxElement(mutualizedHerdGrazingModalities.size - 1))
          val faidherbiaDist = utils.collectionWithGini(gini.faidherbiaGini, nbKitchenProfile, MaxElement(gini.maxFaidherbia))
          val breederDist = utils.collectionWithGini(gini.breederGini, nbKitchenProfile, MaxElement(gini.maxBreeder))

          Distributions(
            kitchen = kitchenDist.map(_.toInt),
            solidarity = solidarityDist.map(_.toInt),
            soilCare = soilCareDist.map(_.toInt),
            mutualizeGrazing = mutualizedGrazingDist.map(_.toInt),
            faidherbia = faidherbiaDist.map(_.toInt),
            breeder = breederDist
          )
        case ms: MeanStd =>
          val kitchenDist = utils.collectionWithMeanAndStd[Int](nbKitchenProfile, ms.kitchenMean, ms.kitchenStd, SumTarget(initialTotalNumberOfKitchen), seed = seed)

          val solidarityDist = utils.collectionWithMeanAndStd[Int](nbKitchenProfile, ms.solidarityMean, ms.solidarityStd, MaxElement(solidarityModalities.size - 1), seed = seed)
          val soilCareDist = utils.collectionWithMeanAndStd[Int](nbKitchenProfile, ms.soilCareMean, ms.soilCareStd, MaxElement(soilCareQModalities.size - 1), seed = seed)
          val mutualizedGrazingDist = utils.collectionWithMeanAndStd[Int](nbKitchenProfile, ms.mutualizedHerdGrazingMean, ms.mutualizedHerdGrazingStd, MaxElement(mutualizedHerdGrazingModalities.size - 1), seed = seed)
          val faidherbiaDist = utils.collectionWithMeanAndStd[Int](nbKitchenProfile, ms.faidherbiaMean, ms.faidherbiaStd, MaxElement(ms.maxFaidherbia), seed = seed)
          val breederDist = utils.collectionWithMeanAndStd[Double](nbKitchenProfile, ms.breederMean, ms.breederStd, MaxElement(ms.maxBreeder), seed = seed)

          Distributions(
            kitchen = kitchenDist,
            solidarity = solidarityDist,
            soilCare = soilCareDist,
            mutualizeGrazing = mutualizedGrazingDist,
            faidherbia = faidherbiaDist,
            breeder = breederDist
          )


    val soilCare = {
      distributions.soilCare.map: sc =>
        val mod = soilCareQModalities(sc)
        (mod._1, mod._2, mod._3)
    }

    val solidarity =
      distributions.solidarity.map: s =>
        val mod = solidarityModalities(s)
        (mod._1, mod._2)

    val herdGrazing =
      distributions.mutualizeGrazing.map: g =>
        val mod = mutualizedHerdGrazingModalities(g)
        (mod._1, mod._2)

    KitchenProfiler(
      KitchenPartition(
        (0 until nbKitchenProfile).map: kpID =>
          (
            KitchenProfile(
              kpID,
              kitchenSize = initialKitchenSize,
              rotationCycle = soilCare(kpID)._1,
              cropingStrategy = CropingStrategy.PeanutForInexcess(0.0),
              ownFallowUse = soilCare(kpID)._2,
              loanStrategy = solidarity(kpID)._1,
              foodDonationStrategy = solidarity(kpID)._2,
              drySeasonHerdStrategy = herdGrazing(kpID)._1,
              wetSeasonHerdStrategy = herdGrazing(kpID)._2,
              herdSizeStrategy = HerdSizeStrategy.LSUByArea(distributions.breeder(kpID)), // = 0.42, // in [0.0; 0.68] 0.68 is more or less equivalent to 140 LSU, which is a maximum possible for the whole area
              drySeasonManureCriteria,
              FertilizerStrategy.UniformFertilizing,
              mulchingStrategy = soilCare(kpID)._3,
              nbFaidherbia = distributions.faidherbia(kpID)
            ),
            distributions.kitchen(kpID)
          )
      ),
      soilCareScore = score[Int](distributions.soilCare, distributions.kitchen),
      solidarityScore = score[Int](distributions.solidarity, distributions.kitchen),
      herdGrazingScore = score[Int](distributions.mutualizeGrazing, distributions.kitchen),
      faidherbiaScore = score[Int](distributions.faidherbia, distributions.kitchen),
      breederScore = score[Double](distributions.breeder, distributions.kitchen)
    )
