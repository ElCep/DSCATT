package dscatt

import dscatt.FoodDonationStrategy.FoodForAllStrategy


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

enum ManPowerProvision extends Control:
    case WorkInOwnKitchenOnly extends ManPowerProvision // manpower is never loaned to another kitchen
    case HelpOnDemand extends ManPowerProvision // manpower is loaned to another kitchen if required

enum MigrantStrategy extends Control:
    case LeaveForEver extends MigrantStrategy // when someone leaves a kitchen, it does once and for all
    case SeasonalPresence extends MigrantStrategy // some people are present in kitchen when there is work, in town the rest of the time

enum HerdStrategy extends Control:
    case AnywhereAnyTime extends HerdStrategy // all herd beasts are grazing on the full area (fallow or crop)
    case EverywhereByDayOwnerByNight extends HerdStrategy // the herd is grazing evrywhere by day and only on the manured kitchen parcels by night
    case OwnerOnly extends HerdStrategy // the herd of the kitchen are grazing on the manured kitchen parcels only

enum FertilizerStrategy extends Control:
    case UniformFertilizing extends FertilizerStrategy
    case Nominal150Fertilizing extends FertilizerStrategy // nominal 150kg per hectare (ie 0.015kg / m2)
    case PriorityFetilizedParcels(criteria: Parcel => Boolean) extends FertilizerStrategy // Fertilizer is set in priority on a given set of Parcels

enum MulchingStrategy extends Control:
    case Mulching(leftOnTheGroundRatio: Double = 0.0) extends MulchingStrategy // Mulch ratio led on the ground to enrich it (and not used for winter herd food)

enum FertilizerAttribution:
    case UniformAttribution extends FertilizerAttribution
    case UniformForTaxPayer extends FertilizerAttribution
    case BagRouletteForTaxPayer extends FertilizerAttribution

enum FaidherbiaStrategy extends Control:
    case NoFaidherbiaAttention extends FaidherbiaStrategy
    case FaidherbiaRegrowPreservation extends FaidherbiaStrategy

case class KitchenProfile(
                           kitchenSize: KitchenSize,
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
  val default = KitchenProfile(
      10,
      RotationCycle.ThreeYears, 
      CropingStrategy.PeanutForInexcess(0.0),
      OwnFallowUse.NeverUseFallow,
      LoanStrategy.AllExtraParcelsLoaner,
      FoodDonationStrategy.FoodForAllStrategy, 
      15,
      HerdStrategy.EverywhereByDayOwnerByNight,
      HerdStrategy.EverywhereByDayOwnerByNight,
      (_, _) => true,
      FertilizerStrategy.UniformFertilizing,
      MulchingStrategy.Mulching(0.0))
}

case class KitchenPartition(profiles: (KitchenProfile, KitchenSize)*)