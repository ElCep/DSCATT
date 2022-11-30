package dscatt

import dscatt.Croping.Crop
import dscatt.Croping.*
import dscatt.Constants.*
import dscatt.Simulation.SimulationState

object Fertility {

  type FertilityBoost = Double

  def uniformManureEffect(areaInM2: Double, herdSize: Int): FertilityBoost = {
    val areaInHa = areaInM2 / 10000
    if (areaInHa == 0.0) 1.0
    else {
      FERTILITY_BOOST_PER_MANURE_KG_PER_HA * KG_OF_MANURE_PER_COW_PER_YEAR * herdSize / areaInHa
    }
  }

  def uniformFertilizerEffect(fertilizerWeight: Int): FertilityBoost = FERTILITY_BOOST_PER_FERTILIZER_KG * fertilizerWeight

  def cropFertilityBoost(crop: Crop): FertilityBoost = {
    crop match {
      case Mil => MIL_EFFECT_ON_FERTILITY_BOOST
      case Peanut => PEANUT_EFFECT_ON_FERTILITY_BOOST
      case _ => FALLOW_EFFECT_FERTILITY_BOOST // the parcel is resting whatever it is NotAssigned or Fallow
    }
  }

  case class HStrategy(dry: HerdStrategy, wet: HerdStrategy, kitchenParcelsArea: Double, kitchenFallowParcelsArea: Double, herdSize: Int)

  def fertiliyBoost(hStrategy: HStrategy, allParcelsArea: Double, allFallowParcelsArea: Double): FertilityBoost = 1.0 + (hStrategy.dry match {
    case EverywhereByDayOwnerByNight =>
      0.83 * (0.2 * uniformManureEffect(allParcelsArea, hStrategy.herdSize) /* day */ + 0.8 * uniformManureEffect(hStrategy.kitchenParcelsArea, hStrategy.herdSize) /* night */) + // dry season
        0.17 * (0.2 * uniformManureEffect(allFallowParcelsArea, hStrategy.herdSize /* day */) + 0.8 * uniformManureEffect(hStrategy.kitchenFallowParcelsArea, hStrategy.herdSize) /* night */) // wet season
    case EverywhereAllTime =>
      0.83 * uniformManureEffect(allParcelsArea, hStrategy.herdSize) + 0.17 * uniformManureEffect(allFallowParcelsArea, hStrategy.herdSize)
    case OwnerOnly =>
      0.83 * uniformManureEffect(hStrategy.kitchenParcelsArea, hStrategy.herdSize) + 0.17 * uniformManureEffect(hStrategy.kitchenFallowParcelsArea, hStrategy.herdSize)
  }
    )

  def assign(state: SimulationState): SimulationState = {

    val herdStrategies = state.kitchens.map { k =>
      val ownedParcels = World.ownedParcelsForKitchen(state.world, k)

      k.id -> HStrategy(k.drySeasonHerdStrategy, k.wetSeasonHerdStrategy, ownedParcels.map(_.area).sum, ownedParcels.filter(_.crop == Fallow).map(_.area).sum, k.herdSize)
    }

    val allParcelsArea = state.world.parcels.map(_.area).sum
    val allFallowParcelsArea = World.fallowParcels(state.world).map(_.area).sum


    val manureFertilityBoost = herdStrategies.map { case (id, hs) =>
      id -> fertiliyBoost(hs, allParcelsArea, allFallowParcelsArea)
    }.toMap

    val updatedFertilityParcels = state.world.parcels.map { p =>
      p.copy(fertility = p.fertility * cropFertilityBoost(p.crop) * manureFertilityBoost(p.ownerID))
    }

    state.copy(world = state.world.copy(parcels = updatedFertilityParcels), history = state.history.updateFertilitySats(state.year, state.world, state.kitchens))
  }

}
