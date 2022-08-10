package dscatt


trait Control
type KitchenSize = Int

sealed trait RotationCycle extends Control
object ThreeYears extends RotationCycle
object TwoYears extends RotationCycle

sealed trait CropingStrategy extends Control
object Intensive extends CropingStrategy // all parcels in culture all the time
object Parsimonious extends CropingStrategy // no more than necessary (needs)

sealed trait LoanStrategy extends Control
object AllExtraParcelsLoaner extends LoanStrategy // loans its extra parcels
object ExtraParcelsExceptFallowLoaner extends LoanStrategy
object Selfish extends LoanStrategy

sealed trait InterKitchenFoodExchange extends Control
object FoodForUsOnly extends InterKitchenFoodExchange // extra food keeps in the kitchen (for being sold)
object FoodDonation extends InterKitchenFoodExchange // extra food can be given to a demanding kitchen

sealed trait MigrantStrategy extends Control
object LeaveForEver extends MigrantStrategy // when someone leaves a kitchen, it does once and for all
object SeasonalPresence extends MigrantStrategy // some people are present in kitchen when there is work, in town the rest of the time

sealed trait HerdStrategy extends Control
object SmallYearRound extends HerdStrategy // small herd present in village all year round
object BigSeasonal extends HerdStrategy // big herd present part time in village and part time in transhumance

sealed trait FaidherbiaStrategy extends Control
object  NoFaidherbiaAttention extends FaidherbiaStrategy
object  FaidherbiaRegrowPreservation extends FaidherbiaStrategy

case class KitchenProfile(
                           size: KitchenSize,
                           rotationCycle: RotationCycle,
                           cropingStrategy: CropingStrategy,
                           loanStrategy: LoanStrategy
                         )

object KitchenProfile {
  val default = KitchenProfile(10, ThreeYears, Parsimonious, AllExtraParcelsLoaner)
}

case class KitchenPartition(profiles: (KitchenProfile, KitchenSize)*)