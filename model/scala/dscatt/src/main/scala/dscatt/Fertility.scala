package dscatt

import Croping.Crop
import Croping.*
import History.History
import Kitchen.{KitchenID, parcelFoodProduction}
import Parcel.{ManureDeposit, ParcelID}
import Simulation.SimulationState
import Data.*

import scala.annotation.tailrec

object Fertility {

  case class AgronomicMetrics(availableNitrogen: Double = 0.0, soilQuality: Double = 0.0)

  case class Metrics(year: Int, crop: Crop = Fallow, manureMass: Double = 0.0, mulchingMass: Double = 0.0, agronomicMetrics: AgronomicMetrics = AgronomicMetrics(0.0, 0.0))

  def assign(state: SimulationState, data: Data): SimulationState = {

    val effectiveHerdSizeByKitchen =
      state.kitchens.map { k =>
        val herdFood = World.parcelsForKitchen(state.world, k).map { p =>
          p.crop match {
            case Fallow => fallowNRF(p, data, state.year) * fallowFullPotential(data.RAIN_FALL) * p.area
            case Millet => k.mulchingStrategy match {
              case MulchingStrategy.Mulching(leftOnTheGroundRatio: Double) => milNRF(p, data, state.year) * milFullPotential(data.RAIN_FALL) * p.area * (1 - leftOnTheGroundRatio) * data.MIL_STRAW_RATIO
            }
            case _ => 0.0
          }
        }.sum
        k.id -> (herdFood / data.KG_OF_STRAW_PER_COW_PER_YEAR).floor.toInt
      }.toMap

    val sortedHerdByKitchen = effectiveHerdSizeByKitchen.toSeq.sortBy(_._2)

    //data.HERD_SIZE_FEEDED_BY_MARIGOT_DURING_DRY_SEASON biggest kitchens get 1 more cow during dry season
    val nbKitchens = effectiveHerdSizeByKitchen.keys.size
    val drySeasonEffectiveHerdSizeByKitchen =
      (
        sortedHerdByKitchen.takeRight(data.HERD_SIZE_FEEDED_BY_MARIGOT_DURING_DRY_SEASON).map { case (k, v) => k -> (v + 1) } ++
          sortedHerdByKitchen.take(nbKitchens - data.HERD_SIZE_FEEDED_BY_MARIGOT_DURING_DRY_SEASON)
        ).toMap

    val totalProducedManure = state.kitchens.map { k =>
      effectiveHerdSizeByKitchen(k.id)
    }.sum * data.KG_OF_MANURE_PER_COW_PER_YEAR

    val totalProducedManureDuringDrySeason = state.kitchens.map { k =>
      drySeasonEffectiveHerdSizeByKitchen(k.id)
    }.sum * data.KG_OF_MANURE_PER_COW_PER_YEAR

    def manureVillageForPFor(parcel: Parcel, parcels: Seq[Parcel]) = totalProducedManure * parcel.area / parcels.map(_.area).sum

    def manureVillageForPForDrySeasonFor(parcel: Parcel, parcels: Seq[Parcel]) = totalProducedManureDuringDrySeason * parcel.area / parcels.map(_.area).sum


    @tailrec
    def fertilizeByKitchen(kitchens: Seq[Kitchen], fertilityUpdated: Seq[Parcel]): Seq[Parcel] = {
      if (kitchens.isEmpty) fertilityUpdated
      else {
        val kitchen = kitchens.head
        val parcelsForK = World.parcelsForKitchen(state.world, kitchen)

        // A parcel is divided into 4 periods of time. First dry (0.7 of the year) and wet (0.3 of the year) season and then
        // night (0.8 of the dung production) and day (0.2 of the dung production).
        @tailrec
        def fertilize(allParcels: Seq[Parcel], fertilityUpdated: Seq[Parcel]): Seq[Parcel] = {
          if (allParcels.isEmpty) fertilityUpdated
          else {
            val parcel = allParcels.head

            val mulchingMass =
              parcel.crop match {
                case Millet => kitchen.mulchingStrategy match {
                  case MulchingStrategy.Mulching(leftOnTheGroundRatio: Double) =>
                    milNRF(parcel, data, state.year) * milFullPotential(data.RAIN_FALL) * parcel.area * leftOnTheGroundRatio * data.MIL_STRAW_RATIO
                  case _ => 0.0
                }
                case _ => 0.0
              }

            val manureMass =
              val dryToBeManuredArea = parcelsForK.filter(p => kitchen.drySeasonManureCriteria(p, kitchen.rotationCycle)).map {
                _.area
              }.sum

              val dryManureKforAssignedP = drySeasonEffectiveHerdSizeByKitchen(kitchen.id) * data.KG_OF_MANURE_PER_COW_PER_YEAR * parcel.area / dryToBeManuredArea
              val dryManureVillageForP = manureVillageForPForDrySeasonFor(
                parcel,
                state.world.parcels.filter(p => kitchen.drySeasonManureCriteria(p, kitchen.rotationCycle))
              )

              val dryMass =
                kitchen.drySeasonManureCriteria(parcel, kitchen.rotationCycle) match {
                  case true =>
                    kitchen.drySeasonHerdStrategy match {
                      case HerdStrategy.EverywhereByDayOwnerByNight => 0.14 * dryManureVillageForP + 0.56 * dryManureKforAssignedP // 0.7 * 0.2 and 0.7 * 0.8
                      case HerdStrategy.AnywhereAnyTime => 0.7 * dryManureVillageForP
                      case HerdStrategy.OwnerOnly => 0.7 * dryManureKforAssignedP
                    }
                  case false => 0.0
                }

              val wetManureVillageForP = manureVillageForPFor(parcel, World.fallowParcels(state.world))
              val manureKforFallow = effectiveHerdSizeByKitchen(kitchen.id) * data.KG_OF_MANURE_PER_COW_PER_YEAR * parcel.area / World.fallowParcels(parcelsForK).map {
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


            val metrics = Fertility.Metrics(state.year, parcel.crop, manureMass, mulchingMass, Fertility.agronomicMetrics(parcel, data, state.year))
            fertilize(
              allParcels.tail,
              fertilityUpdated :+ parcel.copy(fertilityHistory = parcel.fertilityHistory :+ metrics)
            )
          }
        }

        fertilizeByKitchen(kitchens.tail, fertilityUpdated = fertilize(parcelsForK, fertilityUpdated))
      }
    }

    val newParcels = fertilizeByKitchen(state.kitchens, Seq())

    val newWorld = state.world.copy(parcels = newParcels)
    state.copy(world = newWorld,
      history = state.history
        .updateFertilitySats(state.year, newWorld, state.kitchens)
        .updateHerdStats(state.year, drySeasonEffectiveHerdSizeByKitchen)
    )
  }

  private def soilQuality(parcel: Parcel, data: Data, year: Int) = {

    // manureMASS: KG, manureBoost: SQ <-> KG * ??? => Est-ce que les coef multiplicateur (0.00012 et 0.0008) sont en QS / KG ?
    val manureBoost =
      // Last 2 years of manure are used to compute the boost: 0.6 for the most recent deposit and 0.4 for the oldest
      val last2Years = parcel.fertilityHistory.takeRight(2)
      last2Years.lastOption.map(_.manureMass * data.MANURE_BOOST_1_YEAR_AGO).getOrElse(0.0) + last2Years.headOption.map(_.manureMass * data.MANURE_BOOST_2_YEARS_AGO).getOrElse(0.0)

    // mulchingMASS: KG, mulchingBoost: SQ <-> KG * ??? => Est-ce que les coef multiplicateur (MULCHING_BOOST) sont en QS / KG ?
    val mulchingBoost = parcel.fertilityHistory.lastOption.map(_.mulchingMass).getOrElse(0.0) * data.MULCHING_BOOST

    // The soil quality it computed at the begining of the year before the the rotation process, so that the parcel crop here
    // is equivalent to the crop of the previous year

    // SQ / HA * HA
    val fallowBoost = parcel.crop match {
      case Croping.Fallow => data.FALLOW_BOOST * parcel.area
      case _ => 0.0
    }

    // TREE x SQ / TREE <-> SQ
    val faidherbiaBoost = parcel.faidherbiaTrees * data.FAIDHERBIA_BOOST_PER_TREE

    // SQ / HA * HA <-> SQ
    val soilQualityPreviousYearOnParcel = parcel.fertilityHistory.lift(year - 2).map(_.agronomicMetrics.soilQuality).getOrElse(data.SOIL_QUALITY_BASIS * parcel.area)

    //(soilQualityPreviousYear + manureBoost + mulchingBoost + faidherbiaBoost) * fallowBoost * data.EROSION
    soilQualityPreviousYearOnParcel * data.EROSION + manureBoost + mulchingBoost + faidherbiaBoost + fallowBoost

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

  def agronomicMetrics(parcel: Parcel, data: Data, year: Int) = {
    Fertility.AgronomicMetrics(availableNitrogen(parcel, data), soilQuality(parcel, data, year))
  }

  def milNRF(parcel: Parcel, data: Data, year: Int) =

    val metrics = agronomicMetrics(parcel: Parcel, data, year)
    val nrf = metrics.soilQuality * ((metrics.availableNitrogen / parcel.area) match
      case n if n < 18 => 0.25
      case n if n >= 18 && n <= 83 => 0.501 * Math.log(n) - 1.2179
      case _ => 1.0
      )
    // Ensure that soilQuality boost does not make NRF exceed 1
    if (nrf > 1) 1.0 else nrf

  def fallowNRF(parcel: Parcel, data: Data, year: Int) =

    val metrics = agronomicMetrics(parcel: Parcel, data, year)
    val nrf = metrics.soilQuality * ((metrics.availableNitrogen / parcel.area) match
      case n if n < 10 => 0.25
      case n if n >= 10 && n <= 50 => 0.501 * Math.log(n) - 1.2179
      case _ => 1.0
      )
    // Ensure that soilQuality boost does not make NRF exceed 1
    //println("NRF " + nrf)
    if (nrf > 1) 1.0 else nrf

  def peanutNRF = 1.0

  def milFullPotential(rainFall: MM): KG_BY_HA = rainFall match
    case n if n < 317 => 0.0
    case n if n >= 317 && n < 805 => 1000 * (1.8608 * math.log(rainFall) - 8.6756)
    case _ => 3775

  def milSeedFullPontential(data: Data): KG_BY_HA = milFullPotential(data.RAIN_FALL) * data.MIL_SEED_RATIO

  val peanutFullPotential: KG_BY_HA = 1300

  def peanutSeedFullPotential(data: Data): KG_BY_HA = peanutFullPotential * data.PEANUT_SEED_RATIO

  def fallowFullPotential(rainFall: MM): KG_BY_HA = rainFall match
    case n if n < 317 => 0.0
    case n if n >= 317 && n < 805 => 1000 * (0.4322 * math.log(rainFall) - 1.195)
    case _ => 3775
}
