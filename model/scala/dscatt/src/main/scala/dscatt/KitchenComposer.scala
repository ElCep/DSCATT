package dscatt

import dscatt.HerdSizeStrategy.LSUByArea

import scala.annotation.tailrec

object KitchenComposer:

  case class ControlInContext[C <: Control](control: C, incompatibleWith: Seq[Control] = Seq())

  case class RequiredControls(rotation: RotationCycle, ownFallowUse: OwnFallowUse, loan: LoanStrategy, donation: FoodDonationStrategy, herdSizeStrategy: HerdSizeStrategy, dryHerdGrazing: HerdGrazingStrategy, wetHerdGrazing: HerdGrazingStrategy, mulching: MulchingStrategy)

  // controlCompositionID from 0 to 1151
  case class KitchenProfileBuilder(kitchenProfileID: KitchenProfileID, ratio: Double, nbFaidherbia: Int, lsu: Double)


  implicit class controlWrapper(c: Control):
    def toDefaultContext = ControlInContext(c, Seq())

  val milletOnly = ControlInContext(RotationCycle.MilletOnly, Seq(OwnFallowUse.UseFallowIfNeeded, LoanStrategy.ExtraParcelsExceptFallowLoaner))
  val milletPeanutRotation = ControlInContext(RotationCycle.MilletOnly, Seq(OwnFallowUse.UseFallowIfNeeded, LoanStrategy.ExtraParcelsExceptFallowLoaner))
  val anywhereAnyTimeHerdGrazing = ControlInContext(HerdGrazingStrategy.AnywhereAnyTime, Seq(HerdSizeStrategy.NoHerd))
  val ownerOnlyHerdGrazing = ControlInContext(HerdGrazingStrategy.OwnerOnly, Seq(HerdSizeStrategy.NoHerd))
  val nightAndDyHerdGrazing = ControlInContext(HerdGrazingStrategy.EverywhereByDayOwnerByNight, Seq(HerdSizeStrategy.NoHerd))
  val noHerd = ControlInContext(HerdSizeStrategy.NoHerd, Seq(HerdGrazingStrategy.AnywhereAnyTime, HerdGrazingStrategy.OwnerOnly, HerdGrazingStrategy.EverywhereByDayOwnerByNight))

  val rotationCycleInContext =
    Seq(
      milletOnly,
      RotationCycle.MilletFallow.toDefaultContext,
      RotationCycle.FallowMilletPeanut.toDefaultContext,
      milletPeanutRotation,
    )

  val ownFallowUseInContext =
    Seq(
      OwnFallowUse.UseFallowIfNeeded.toDefaultContext,
      OwnFallowUse.NeverUseFallow.toDefaultContext
    )

  val loanStrategyInContext =
    Seq(
      LoanStrategy.ExtraParcelsExceptFallowLoaner.toDefaultContext,
      LoanStrategy.AllExtraParcelsLoaner.toDefaultContext,
      LoanStrategy.Selfish.toDefaultContext
    )

  val foodDonationInContext =
    Seq(
      FoodDonationStrategy.FoodForUsOnlyStrategy.toDefaultContext,
      FoodDonationStrategy.FoodForAllStrategy.toDefaultContext
    )

  val herdSizeInContext =
    Seq(
      noHerd,
      HerdSizeStrategy.LSUByArea(99).toDefaultContext,
      HerdSizeStrategy.FullCapacity.toDefaultContext
    )

  val herdGrazingInContext =
    Seq(
      anywhereAnyTimeHerdGrazing,
      ownerOnlyHerdGrazing,
      nightAndDyHerdGrazing
    )

  val mulchingInContext =
    Seq(
      MulchingStrategy.NoMulching.toDefaultContext,
      MulchingStrategy.CropResidue.toDefaultContext
    )

  val allCompositions = // 1152 compositions
    val opts = for
      r <- rotationCycleInContext
      o <- ownFallowUseInContext
      l <- loanStrategyInContext
      d <- foodDonationInContext
      hs <- herdSizeInContext
      dh <- herdGrazingInContext
      wh <- herdGrazingInContext
      m <- mulchingInContext
    yield
      val incompatible = r.incompatibleWith ++ o.incompatibleWith ++ l.incompatibleWith ++ d.incompatibleWith ++ hs.incompatibleWith ++ dh.incompatibleWith ++ wh.incompatibleWith ++ m.incompatibleWith
      incompatible intersect Seq(r.control, o.control, l.control, d.control, hs.control, dh.control, wh.control, m.control) match
        case Seq() => Some(RequiredControls(r.control.asInstanceOf[RotationCycle], o.control.asInstanceOf[OwnFallowUse], l.control.asInstanceOf[LoanStrategy], d.control.asInstanceOf[FoodDonationStrategy], hs.control.asInstanceOf[HerdSizeStrategy], dh.control.asInstanceOf[HerdGrazingStrategy], wh.control.asInstanceOf[HerdGrazingStrategy], m.control.asInstanceOf[MulchingStrategy]))
        case _ => None

    opts.flatten

  val manureDepositStategyMilNextYear = { (p: Parcel, r: RotationCycle) =>
    Croping.evolveCrop(p.crop, r, Croping.evolveCropZone(p.cropZone, r)) == Croping.Millet
  }

  def compose(totalPopulation: Int, kitchenProfileBuilders: Seq[KitchenProfileBuilder]): KitchenPartition =

    // Assume that sum of ratios is 1.0
    @tailrec
    def assign(kPBuilders: Seq[KitchenProfileBuilder], remainingPop: Int, kp: KitchenPartition): KitchenPartition =
      if (kPBuilders.isEmpty || remainingPop == 0 ) kp
      else
        val kPBuilder = kPBuilders.head
        val nbK = (kPBuilder.ratio * totalPopulation / 16).floor.toInt
        val composition = allCompositions(kPBuilder.kitchenProfileID)
        val herdSizeStrategy =
          composition.herdSizeStrategy match
            case LSUByArea(_)=> LSUByArea(kPBuilder.lsu)
            case x=> x

        val part = kp.copy(
          profiles = kp.profiles :+
            (KitchenProfile(
              kPBuilder.kitchenProfileID,
              16,
              composition.rotation,
              CropingStrategy.PeanutForInexcess(0.0),
              composition.ownFallowUse,
              composition.loan,
              composition.donation,
              composition.dryHerdGrazing,
              composition.wetHerdGrazing,
              herdSizeStrategy,
              manureDepositStategyMilNextYear,
              FertilizerStrategy.UniformFertilizing,
              composition.mulching,
              kPBuilder.nbFaidherbia
            ), nbK)
          )

        assign(kPBuilders.tail, remainingPop - 16 * nbK, part)

    @tailrec
    def assignExtraKitchen(sortedPartition: KitchenPartition, iteration: Int, nbExtraKitchen: Int): KitchenPartition =
      if (nbExtraKitchen == 0) sortedPartition
      else
        val kp = sortedPartition.profiles(iteration)
        assignExtraKitchen(sortedPartition.copy(profiles = sortedPartition.profiles.updated(iteration, (kp._1,kp._2 + 1))), iteration + 1, nbExtraKitchen - 1)


    val partition = assign(kitchenProfileBuilders, totalPopulation, KitchenPartition())
    val effectivePopulation = partition.profiles.map(_._2).sum * 16
    val possibleExtraKitchens = ((totalPopulation - effectivePopulation) / 16.0).floor.toInt
    //large kitchen are consider first because it will absorb more smoothly the overcrowding
    assignExtraKitchen(partition.copy(profiles = partition.profiles.sortBy(_._2).reverse), 0, possibleExtraKitchens)




