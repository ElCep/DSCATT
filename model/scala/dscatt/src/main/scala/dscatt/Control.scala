package dscatt

import FoodDonationStrategy.FoodForAllStrategy
import Data._


trait Control
type KitchenSize = Int

enum RotationCycle extends Control:
        case ThreeYears extends RotationCycle
        case TwoYears extends RotationCycle

enum CropingStrategy extends Control:
// Peanut food provisioned from the food needs: cultivated = FoodNeeds + savvingRate(MaxCultivable-FoodNeed)
// savingRate = 0 : parsimonious
// savingRate = 1 : as much as we can
    case PeanutForInexcess(savingRate: Double) extends CropingStrategy

//object AsMuchAsWeCan extends CropingStrategy // Up to the available manpower
//case class Provisioning(exceedingProportion: Double) extends CropingStrategy // while crop stock and manpower exist. In culture: foodNeeds x (1 + exceedingProportion)
//object Parsimonious extends CropingStrategy // no more than necessary (needs)

enum OwnFallowUse extends Control:
    case NeverUseFallow extends OwnFallowUse
    case UseFallowIfNeeded extends OwnFallowUse

enum LoanStrategy extends Control:
    case AllExtraParcelsLoaner extends LoanStrategy // loans its extra parcels
    case ExtraParcelsExceptFallowLoaner extends LoanStrategy
    case Selfish extends LoanStrategy // must be used with AsMuchAsweCan

enum FoodDonationStrategy extends Control:
    case FoodForUsOnlyStrategy extends FoodDonationStrategy // extra food keeps in the kitchen (for being sold)
    case FoodForAllStrategy extends FoodDonationStrategy // extra food can be given to a demanding kitchen

/*
enum ManPowerProvision extends Control:
    case WorkInOwnKitchenOnly extends ManPowerProvision // manpower is never loaned to another kitchen
    case HelpOnDemand extends ManPowerProvision // manpower is loaned to another kitchen if required

enum MigrantStrategy extends Control:
    case LeaveForEver extends MigrantStrategy // when someone leaves a kitchen, it does once and for all
    case SeasonalPresence extends MigrantStrategy // some people are present in kitchen when there is work, in town the rest of the time
*/

enum HerdGrazingStrategy extends Control:
    case AnywhereAnyTime extends HerdGrazingStrategy // all herd beasts are grazing on the full area (fallow or crop)
    case EverywhereByDayOwnerByNight extends HerdGrazingStrategy // the herd is grazing evrywhere by day and only on the manured kitchen parcels by night
    case OwnerOnly extends HerdGrazingStrategy // the herd of the kitchen are grazing on the manured kitchen parcels only

enum HerdSizeStrategy extends Control:
    case LSUByArea(lsuByHa: Double) extends HerdSizeStrategy
    case FullCapacity extends HerdSizeStrategy
    case NoHerd extends HerdSizeStrategy

enum FertilizerStrategy extends Control:
    case UniformFertilizing extends FertilizerStrategy
    case Nominal150Fertilizing extends FertilizerStrategy // nominal 150kg per hectare (ie 0.015kg / m2)
    case PriorityFetilizedParcels(criteria: Parcel => Boolean) extends FertilizerStrategy // Fertilizer is set in priority on a given set of Parcels


enum MulchingStrategy extends Control:
    case CropResidue(leftOnTheGroundRatio: Double = 0.0) extends MulchingStrategy // Mulch ratio led on the ground to enrich it (and not used for winter herd food)
    case CropResidueAmendment(mass: KG_BY_HA = 0.0) extends MulchingStrategy
/*
enum FertilizerAttribution:
    case UniformAttribution extends FertilizerAttribution
    case UniformForTaxPayer extends FertilizerAttribution
    case BagRouletteForTaxPayer extends FertilizerAttribution



enum FaidherbiaStrategy extends Control:
    case NoFaidherbiaAttention extends FaidherbiaStrategy
    case FaidherbiaRegrowPreservation extends FaidherbiaStrategy
*/
case class KitchenProfile(
                           kitchenSize: KitchenSize,
                           rotationCycle: RotationCycle,
                           cropingStrategy: CropingStrategy,
                           ownFallowUse: OwnFallowUse,
                           loanStrategy: LoanStrategy,
                           foodDonationStrategy: FoodDonationStrategy,
                           drySeasonHerdStrategy: HerdGrazingStrategy,
                           wetSeasonHerdStrategy: HerdGrazingStrategy,
                           herdSizeStrategy: HerdSizeStrategy,
                           drySeasonManureCriteria: (Parcel, RotationCycle) => Boolean,
                           fertilizerStrategy: FertilizerStrategy,
                           mulchingStrategy: MulchingStrategy,
                           nbFaidherbia: TREE_BY_HA
                         )

object KitchenProfile {
  val default = KitchenProfile(
      10,
      RotationCycle.ThreeYears,
      CropingStrategy.PeanutForInexcess(0.0),
      OwnFallowUse.NeverUseFallow,
      LoanStrategy.AllExtraParcelsLoaner,
      FoodDonationStrategy.FoodForAllStrategy,
      HerdGrazingStrategy.EverywhereByDayOwnerByNight,
      HerdGrazingStrategy.EverywhereByDayOwnerByNight,
      HerdSizeStrategy.LSUByArea(0.42),
      (_, _) => true,
      FertilizerStrategy.UniformFertilizing,
      MulchingStrategy.CropResidue(0.0),
      4
  )
}

case class KitchenPartition(profiles: (KitchenProfile, Int)*)