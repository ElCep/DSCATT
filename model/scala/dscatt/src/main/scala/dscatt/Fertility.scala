package dscatt

import Croping.Crop
import Croping.*
import History.History
import Kitchen.KitchenID
import Parcel.{ManureDeposit, ParcelID}
import Simulation.SimulationState
import Data._

import scala.annotation.tailrec

object Fertility {

  case class AgronomicMetrics(availableNitrogen: Double = 0.0, soilQuality: Double = 0.0)

  case class Metrics(year: Int, crop: Crop = Fallow, manureMass: Double = 0.0, mulchingMass: Double = 0.0, agronomicMetrics: AgronomicMetrics = AgronomicMetrics(0.0, 0.0))

  def assign(state: SimulationState, data: Data): SimulationState = {

    val effectiveHerdSizeByKitchen =
      state.kitchens.map { k =>
        val herdFood = World.parcelsForKitchen(state.world, k).map { p =>
          p.crop match {
            case Fallow => fallowNRF(p,data) * fallowFullPotential(data.RAIN_FALL) * p.area
            case Mil => k.mulchingStrategy match {
              case MulchingStrategy.Mulching(leftOnTheGroundRatio: Double) => milNRF(p, data) * milFullPotential(data.RAIN_FALL) * p.area * (1 - leftOnTheGroundRatio) * data.MIL_STRAW_RATIO
            }
            case _ => 0.0
          }
        }.sum
        k.id -> (data.EXPANDING_HERD_SIZE * herdFood / data.KG_OF_STRAW_PER_COW_PER_YEAR).floor.toInt
      }.toMap

    def manureVillageForPFor(parcel: Parcel, parcels: Seq[Parcel], state: SimulationState) =
      state.kitchens.map { k =>
        effectiveHerdSizeByKitchen(k.id)
      }.sum * data.KG_OF_MANURE_PER_COW_PER_YEAR / parcels.map {
        _.area
      }.sum * parcel.area


    // A parcel is divided into 4 periods of time. First dry (0.7 of the year) and wet (0.3 of the year) season and then
    // night (0.8 of the dung production) and day (0.2 of the dung production).
    @tailrec
    def fertilize(allParcels: Seq[Parcel], fertilityUpdated: Seq[Parcel], data: Data): Seq[Parcel] = {
      if (allParcels.isEmpty) fertilityUpdated
      else {
        val parcel = allParcels.head
        val kitchenOption = Kitchen.kitchen(state.kitchens, parcel.farmerID)

        val mulchingMass =
          parcel.crop match {
            case Mil => kitchenOption.map {
              _.mulchingStrategy
            } match {
              case Some(MulchingStrategy.Mulching(leftOnTheGroundRatio: Double)) =>
                milNRF(parcel, data) * milFullPotential(data.RAIN_FALL) * parcel.area * leftOnTheGroundRatio * data.MIL_STRAW_RATIO
              case _ => 0.0
            }
            case _ => 0.0
          }

        val manureMass = kitchenOption.map { kitchen =>
          val dryToBeManuredArea = World.farmedParcelsForKitchen(state.world, kitchen).filter(p => kitchen.drySeasonManureCriteria(p, kitchen.rotationCycle)).map {
            _.area
          }.sum

          val manureKforAssignedP = effectiveHerdSizeByKitchen(kitchen.id) * data.KG_OF_MANURE_PER_COW_PER_YEAR * parcel.area / dryToBeManuredArea
          val dryManureVillageForP = manureVillageForPFor(
            parcel,
            state.world.parcels.filter(p => kitchen.drySeasonManureCriteria(p, kitchen.rotationCycle)),
            state)

          val dryMass =
            kitchen.drySeasonManureCriteria(parcel, kitchen.rotationCycle) match {
              case true =>
                kitchen.drySeasonHerdStrategy match {
                  case HerdStrategy.EverywhereByDayOwnerByNight => 0.56 * manureKforAssignedP + 0.14 * dryManureVillageForP // 0.7 * 0.8 and 0.7 * 0.2
                  case HerdStrategy.AnywhereAnyTime => 0.7 * dryManureVillageForP
                  case HerdStrategy.OwnerOnly => 0.7 * manureKforAssignedP
                }
              case false => 0.0
            }

          val wetManureVillageForP = manureVillageForPFor(parcel, World.fallowParcels(state.world), state)
          val manureKforFallow = effectiveHerdSizeByKitchen(kitchen.id) * data.KG_OF_MANURE_PER_COW_PER_YEAR * parcel.area / World.fallowParcelsForKitchen(state.world, kitchen).map {
            _.area
          }.sum

          val wetMass = parcel.crop match {
            case Fallow =>
              kitchen.wetSeasonHerdStrategy match {
                case HerdStrategy.EverywhereByDayOwnerByNight => 0.24 * manureKforFallow + 0.06 * wetManureVillageForP // 0.3 * 0.8 and 0.3 * 0.2
                case HerdStrategy.AnywhereAnyTime => 0.3 * wetManureVillageForP
                case HerdStrategy.OwnerOnly => 0.3 * manureKforFallow
              }
            case _ => 0.0
          }
          dryMass + wetMass
        }.getOrElse(0.0)
        
        val metrics = Fertility.Metrics(state.year, parcel.crop, manureMass, mulchingMass, Fertility.agronomicMetrics(parcel, data))
        fertilize(
          allParcels.tail,
          fertilityUpdated :+ parcel.copy(fertilityHistory = parcel.fertilityHistory :+ metrics),
          data
        )
      }
    }

    val newParcels = fertilize(state.world.parcels, Seq(), data)

    val newWorld = state.world.copy(parcels = newParcels)
    state.copy(world = newWorld,
      history = state.history
        .updateFertilitySats(state.year, newWorld, state.kitchens)
        .updateHerdStats(state.year, newWorld, effectiveHerdSizeByKitchen)
    )
  }

  private def soilQuality(parcel: Parcel, data: Data) = {

    val manureBoost =
      // Last 2 years of manure are used to compute the boost: 0.6 for the most recent deposit and 0.4 for the oldest
      val last2Years = parcel.fertilityHistory.takeRight(2)
      last2Years.lastOption.map(_.manureMass * 0.00012).getOrElse(0.0) + last2Years.headOption.map(_.manureMass * 0.00008).getOrElse(0.0)

    val mulchingBoost = parcel.fertilityHistory.lastOption.map(_.mulchingMass).getOrElse(0.0) * 0.001

    // The soil quality it computed at the begining of the year before the the rotation process, so that the parcel crop here
    // is equivalent to the crop of the previous year
    val fallowBoost = parcel.crop match {
      case Croping.Fallow => data.FALLOW_BOOST
      case _ => 0.0
    }

    val faidherbiaBoost = parcel.faidherbiaTrees * 0.06

    data.SOIL_QUALITY_BASIS + manureBoost + mulchingBoost + fallowBoost + faidherbiaBoost
  }

  // in kg. Computed from previous year soil quality and manure production
  private def availableNitrogen(parcel: Parcel, data: Data) = {

    val airNitrogen = data.ATMOSPHERIC_NITROGEN * parcel.area

    val soilNitrogen = data.NITROGEN_MINERALIZATION * parcel.area

    val manureNitrogen =
      val last2Years = parcel.fertilityHistory.takeRight(2)
      data.NITROGEN_PROPORTION_PER_MANURE_KG * (last2Years.lastOption.map {
        _.manureMass * 0.6
      }.getOrElse(0.0) + last2Years.headOption.map {
        _.manureMass * 0.4
      }.getOrElse(0.0))

    val faidherbiaNitrogen = 4 * parcel.faidherbiaTrees

    airNitrogen + soilNitrogen + manureNitrogen + faidherbiaNitrogen

  }

  def agronomicMetrics(parcel: Parcel, data: Data) = {
    Fertility.AgronomicMetrics(availableNitrogen(parcel, data), soilQuality(parcel, data))
  }

  def milNRF(parcel: Parcel, data: Data) =
    val metrics = agronomicMetrics(parcel: Parcel, data)
    val nrf = metrics.soilQuality * ((metrics.availableNitrogen / parcel.area) match
      case n if n < 18 => 0.25
      case n if n >= 18 && n <= 83 => 0.501 * Math.log(n) - 1.2179
      case _ => 1.0
      )
    // Ensure that soilQuality boost does not make NRF exceed 1
    if (nrf > 1) 1.0 else nrf

  def fallowNRF(parcel: Parcel, data: Data) =
    val metrics = agronomicMetrics(parcel: Parcel, data)
    val nrf = metrics.soilQuality * ((metrics.availableNitrogen / parcel.area) match
      case n if n < 10 => 0.25
      case n if n >= 10 && n <= 50 => 0.501 * Math.log(n) - 1.2179
      case _ => 1.0
      )
    // Ensure that soilQuality boost does not make NRF exceed 1
    if (nrf > 1) 1.0 else nrf

  def peanutNRF = 1.0

  def milFullPotential(rainFall: MM): KG_BY_HA = rainFall match
    case n if n < 317 => 0.0
    case n if n >= 317 && n < 805 => 1000 * (1.8608 * math.log(rainFall) - 8.6756)
    case _ => 3775

  def milSeedFullPontential(data: Data): KG_BY_HA = milFullPotential(data.RAIN_FALL) * data.MIL_SEED_RATIO

  val peanutFullPotential: KG_BY_HA = 1300
  def peanutSeedFullPotential(data:Data): KG_BY_HA = peanutFullPotential * data.PEANUT_SEED_RATIO

  def fallowFullPotential(rainFall: MM): KG_BY_HA = rainFall match
    case n if n < 317 => 0.0
    case n if n >= 317 && n < 805 => 1000 * (0.4322 * math.log(rainFall) - 1.195)
    case _ => 3775
}
