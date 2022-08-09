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
object Loaner extends LoanStrategy // loans its extra parcels
object Selfish extends LoanStrategy

case class KitchenProfile(
                           size: Int,
                           rotationCycle: RotationCycle,
                           cropingStrategy: CropingStrategy,
                           loanStrategy: LoanStrategy
                         )

object KitchenProfile {
  val default = KitchenProfile(10, ThreeYears, Parsimonious, Loaner)
}

case class KitchenPartition(profiles: (KitchenProfile, KitchenSize)*)