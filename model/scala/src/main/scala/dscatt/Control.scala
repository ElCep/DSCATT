package dscatt


trait Control
type KitchenSize = Int

sealed trait RotationCycle extends Control
object ThreeYears extends RotationCycle
object TwoYears extends RotationCycle

sealed trait CropingStrategy extends Control
object AsMuchAsWeCan extends CropingStrategy // Up to the available manpower
case class Provisioning(exceedingProportion: Double) extends CropingStrategy // while crop stock and manpower exist. In culture: foodNeeds x (1 + exceedingProportion)
object Parsimonious extends CropingStrategy // no more than necessary (needs)

sealed trait OwnFallowUse extends Control
object NeverUseFallow extends OwnFallowUse
object UseFallowIfNeeded extends OwnFallowUse

sealed trait LoanStrategy extends Control
object AllExtraParcelsLoaner extends LoanStrategy // loans its extra parcels
object ExtraParcelsExceptFallowLoaner extends LoanStrategy
object Selfish extends LoanStrategy // must be used with AsMuchAsweCan

sealed trait FoodDonationStrategy extends Control
object FoodForUsOnlyStrategy extends FoodDonationStrategy // extra food keeps in the kitchen (for being sold)
object FoodForAllStrategy extends FoodDonationStrategy // extra food can be given to a demanding kitchen

sealed trait ManPowerProvision extends Control
object WorkInOwnKitchenOnly extends ManPowerProvision // manpower is never loaned to another kitchen
object HelpOnDemand extends ManPowerProvision // manpower is loaned to another kitchen if required

sealed trait MigrantStrategy extends Control
object LeaveForEver extends MigrantStrategy // when someone leaves a kitchen, it does once and for all
object SeasonalPresence extends MigrantStrategy // some people are present in kitchen when there is work, in town the rest of the time

sealed trait HerdStrategy extends Control
object AnywhereAnyTime extends HerdStrategy // all herd beasts are grazing on the full area (fallow or crop)
object EverywhereByDayOwnerByNight extends HerdStrategy // the herd is grazing evrywhere by day and only on the manured kitchen parcels by night
object OwnerOnly extends HerdStrategy // the herd of the kitchen are grazing on the manured kitchen parcels only

sealed trait FertilizerStrategy extends Control
object UniformFertilizing extends FertilizerStrategy
object Nominal150Fertilizing extends FertilizerStrategy // nominal 150kg per hectare (ie 0.015kg / m2)
case class PriorityFetilizedParcels(criteria: Parcel => Boolean) extends FertilizerStrategy // Fertilizer is set in priority on a given set of Parcels

sealed trait MulchingStrategy extends Control
case class Mulching(leftOnTheGroundRatio: Double = 0.0) extends MulchingStrategy // Mulch ratio led on the ground to enrich it (and not used for winter herd food)

sealed trait FertilizerAttribution
object UniformAttribution extends FertilizerAttribution
object UniformForTaxPayer extends FertilizerAttribution
object BagRouletteForTaxPayer extends FertilizerAttribution

sealed trait FaidherbiaStrategy extends Control
object NoFaidherbiaAttention extends FaidherbiaStrategy
object FaidherbiaRegrowPreservation extends FaidherbiaStrategy

case class KitchenProfile(
                           size: KitchenSize,
                           rotationCycle: RotationCycle,
                           cropingStrategy: CropingStrategy,
                           ownFallowUse: OwnFallowUse,
                           loanStrategy: LoanStrategy,
                           foodDonationStrategy: FoodDonationStrategy,
                           herdSize: Int,
                           drySeasonHerdStrategy: HerdStrategy,
                           wetSeasonHerdStrategy: HerdStrategy,
                           drySeasonManureCriteria: (Parcel, RotationCycle) => Boolean,
                           fertilizerStrategy: FertilizerStrategy,
                           mulchingStrategy: MulchingStrategy
                         )

object KitchenProfile {
  val default = KitchenProfile(10, ThreeYears, Parsimonious, NeverUseFallow, AllExtraParcelsLoaner, FoodForAllStrategy, 15, EverywhereByDayOwnerByNight, EverywhereByDayOwnerByNight, (_, _) => true, UniformFertilizing, Mulching(0.0))
}

case class KitchenPartition(profiles: (KitchenProfile, KitchenSize)*)