package dscatt

import dscatt.Cost.HerdGrazing
import dscatt.SwitchType.Mulching


case class KitchenProfiler(
                            kitchenPartition: KitchenPartition,
                            soilCareScore: Double,
                            solidarityScore: Double,
                            herdGrazingScore: Double,
                            faidherbiaScore: Double,
                            breezerScore: Double
                          )

object KitchenProfiler:

  /*
  1- solidarity // loan - foodDonation
  2- soilCare // cropRotation - ownFallowUse - mulching
  3- herdGrazing // dryHerdGrazing - wetHerdGrazing
  4- herdUsage // LSU
  5- faidherbia // #faidherbia
  */

  //1
  private val sortedLoanModalities =
    Seq(
      LoanStrategy.Selfish,
      LoanStrategy.ExtraParcelsExceptFallowLoaner,
      LoanStrategy.AllExtraParcelsLoaner
    )

  private val sortedFoodDonationModalities =
    Seq(
      FoodDonationStrategy.FoodForUsOnlyStrategy,
      FoodDonationStrategy.FoodForAllStrategy
    )

  private val solidarityModalities =
    for l <- sortedLoanModalities
        f <- sortedFoodDonationModalities
    yield (l, f)

  //2
  private val sortedCultureDiversityModalities =
    Seq(
      RotationCycle.MilletOnly,
      RotationCycle.MilletPeanut,
      RotationCycle.MilletFallow,
      RotationCycle.FallowMilletPeanut
    )

  private val sortedOwnFallowUseModalities =
    Seq(
      OwnFallowUse.UseFallowIfNeeded,
      OwnFallowUse.NeverUseFallow
    )
  private val sortedMulchingModalities =
    Seq(
      MulchingStrategy.NoMulching,
      MulchingStrategy.CropResidue
    )

  private val soilCareQModalities =
    for rc <- sortedCultureDiversityModalities
        fu <- sortedOwnFallowUseModalities
        m <- sortedMulchingModalities
    yield (rc, fu, m)

  //3 dry / wet
  private val sortedMutualizedHerdGrazingModalities =
    Seq(
      HerdGrazingStrategy.OwnerOnly,
      HerdGrazingStrategy.EverywhereByDayOwnerByNight,
      HerdGrazingStrategy.AnywhereAnyTime
    )

  //dry / wet
  private val mutualizedHerdGrazingModalities =
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


  def build(
             nbKitchenProfile: Int,
             initialTotalNumberOfKitchen: Int,
             initialKitchenSize: Int,
             kitchenGini: Double,
             solidarityGini: Double,
             soilCareGini: Double,
             mutualizedHerdGrazingGini: Double,
             faidherbiaGini: Double,
             maxFaidherbia: Int,
             breederGini: Double,
             maxBreeder: Double,
             drySeasonManureCriteria: (Parcel, RotationCycle) => Boolean,
           ) =

    val kitchenDist =
      val dist = utils.collectionWithGini(kitchenGini, nbKitchenProfile, Left(initialTotalNumberOfKitchen))
      val zero = dist.indexOf(0)
      if zero > 0
      then
        val max = dist.max
        dist.updated(zero, 1).updated(dist.indexOf(max), max - 1)
      else dist

    val solidarityDist = utils.collectionWithGini(solidarityGini, nbKitchenProfile, Right(solidarityModalities.size - 1))
    val soilCareDist = utils.collectionWithGini(soilCareGini, nbKitchenProfile, Right(soilCareQModalities.size - 1))
    val mutualizedGrazingDist = utils.collectionWithGini(mutualizedHerdGrazingGini, nbKitchenProfile, Right(mutualizedHerdGrazingModalities.size - 1))
    val faidherbiaDist = utils.collectionWithGini(faidherbiaGini, nbKitchenProfile, Right(maxFaidherbia))
    val breederDist = utils.collectionWithGini(breederGini, nbKitchenProfile, Right(maxBreeder))

    println("Kitchen dist " + kitchenDist + ", G: " + utils.gini(kitchenDist.map(_.toDouble)))
    //    println("Solid dist " + solidarityDist + ", G: " + utils.gini(solidarityDist.map(_.toDouble)))
    //    println("culture diversity dist " + soilCareDist + ", G: " + utils.gini(soilCareDist.map(_.toDouble)))
    //    println("Grazing dist " + mutualizedGrazingDist + ", G: " + utils.gini(mutualizedGrazingDist.map(_.toDouble)))
    //    println("Faid dist " + faidherbiaDist + ", G: " + utils.gini(faidherbiaDist.map(_.toDouble)))
    //    println("Breeder dist " + breederDist + ", G: " + utils.gini(breederDist))
    //
    //    println("sol score:  " + score[Int](solidarityDist, kitchenDist))
    //    println("soil care score:  " + score[Int](soilCareDist, kitchenDist))
    //    println("mutualized grazing score " + score[Int](mutualizedGrazingDist, kitchenDist))
    //    println("faid score:  " + score[Int](faidherbiaDist, kitchenDist))
    //    println("breeder score:  " + score[Double](breederDist, kitchenDist))

    val soilCare =
      soilCareDist.map: sc =>
        val mod = soilCareQModalities(sc)
        (mod._1, mod._2, mod._3)

    val solidarity =
      solidarityDist.map: s =>
        val mod = solidarityModalities(s)
        (mod._1, mod._2)

    val herdGrazing =
      mutualizedGrazingDist.map: g =>
        val mod = mutualizedHerdGrazingModalities(g)
        (mod._1, mod._2)

    println("ROC " + soilCare)

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
              herdSizeStrategy = HerdSizeStrategy.LSUByArea(breederDist(kpID)), // = 0.42, // in [0.0; 0.68] 0.68 is more or less equivalent to 140 LSU, which is a maximum possible for the whole area
              drySeasonManureCriteria,
              FertilizerStrategy.UniformFertilizing,
              mulchingStrategy = soilCare(kpID)._3,
              nbFaidherbia = faidherbiaDist(kpID)
            ),
            kitchenDist(kpID)
          )
      ),
      soilCareScore = score[Int](soilCareDist, kitchenDist),
      solidarityScore = score[Int](solidarityDist, kitchenDist),
      herdGrazingScore = score[Int](mutualizedGrazingDist, kitchenDist),
      faidherbiaScore = score[Int](faidherbiaDist, kitchenDist),
      breezerScore = score[Double](breederDist, kitchenDist)
    )
