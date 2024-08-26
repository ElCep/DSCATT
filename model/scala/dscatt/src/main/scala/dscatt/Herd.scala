package dscatt


import Croping.Crop
import Croping.*
import dscatt.Data.*
import dscatt.Kitchen.KitchenID
import dscatt.Simulation.SimulationState

object Herd {

  private def maximumLiveStockUnitByKitchen(state: SimulationState, data: Data) =
      val rainFall = data.rainFallIn(state.year)
      state.kitchens.map { k =>
        val herdFood = World.parcelsForKitchen(state.world, k).map { p =>
          p.crop match {
            case Fallow => Fertility.fallowNRF(p, data, state.year) * Fertility.fallowFullPotential(rainFall) * p.area
            case Millet => k.mulchingStrategy match {
              case MulchingStrategy.CropResidue => 0.0
              case MulchingStrategy.NoMulching=> Fertility.milNRF(p, data, state.year) * Fertility.milFullPotential(rainFall) * p.area * data.MIL_STRAW_RATIO
            }
            case _ => 0.0
          }
        }.sum
        k -> (herdFood / data.KG_OF_STRAW_PER_COW_PER_YEAR)
      }.toMap

  def liveStockUnitByKitchen(state: SimulationState, data: Data) =
    val maximumLSU = maximumLiveStockUnitByKitchen(state, data)

    val requiredHSByKitchen = maximumLSU.map { case (kitchen, hsMaximum) =>
      val requiredHerdSize = kitchen.herdSizeStrategy match
        case HerdSizeStrategy.LSUByArea(lsuByHa: Double)=> lsuByHa * World.ownedAreaForKitchen(state.world,kitchen)
        case HerdSizeStrategy.FullCapacity=> hsMaximum
        case HerdSizeStrategy.NoHerd=> 0.0
      kitchen-> math.min(requiredHerdSize, hsMaximum)
    }.toSeq

    // We use the marigot area to fit a herd size as an integer (ex: 1.2 LSU -> 2 LSU while the available quantity in HERD_SIZE_FED_BY_MARIGOT is not null)te
    def marigotAddOn(requiredHSByKitchen: Seq[(Kitchen, Double)], completedKitchens: Seq[(KitchenID, Int)],marigotStock: Double): Seq[(KitchenID, Int)] =
      if (requiredHSByKitchen.isEmpty || marigotStock <= 0.0)
        completedKitchens ++ requiredHSByKitchen.map{case (k, hs)=> k.id-> hs.floor.toInt}
      else
        val (kitchen,requiredHS) = (requiredHSByKitchen.head._1,requiredHSByKitchen.head._2)
        val upgradedHS = requiredHS.ceil.toInt
        val diff = upgradedHS - requiredHS
        marigotAddOn(requiredHSByKitchen.tail, completedKitchens :+ (kitchen.id, upgradedHS), marigotStock - diff)

    marigotAddOn(requiredHSByKitchen, Seq(), data.HERD_SIZE_FED_BY_MARIGOT)

}
