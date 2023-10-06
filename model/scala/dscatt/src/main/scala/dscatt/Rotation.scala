package dscatt

import dscatt.Croping.{Crop, Fallow, Mil, NotAssigned, Peanut, Three}
import dscatt.Kitchen.FoodBalance
import dscatt.Loan.Loan
import dscatt.Simulation.SimulationState
import org.apache.commons.math3.random.MersenneTwister

object Rotation {
  def evolve(simulationState: SimulationState, soilQualityBasis: Double): (SimulationState, Seq[FoodBalance]) = {


    // Compute theoritical crops for coming year before we know if it is in culture or not
    val theoriticalCroping = simulationState.kitchens.map { k =>
      k -> World.ownedParcelsForKitchen(simulationState.world, k).map { p =>
        val newCropZone = Croping.evolveCropZone(p.cropZone, k.rotationCycle)
        p.copy(cropZone = newCropZone,
          crop = Croping.evolveCrop(p.crop, k.rotationCycle, newCropZone)
        )
      }
    }
    //theariticalAgronomicMetrics
    // Compute soil quality and available nitrogen for each parcel on the trearitical crop rotation
    implicit val theoreticalFertilityMetricsByParcel: Fertility.AgronomicMetricsByParcel = theoriticalCroping.flatMap {
      _._2
    }.map { p =>
      p.id -> Fertility.agronomicMetrics(p, soilQualityBasis)
    }.toMap

    val autonomousFoodBalance = theoriticalCroping.map { case (k, ps) => Kitchen.foodBalance(ps, k) }
    // Loaners process:
    // 1- have a positive foodBalance with all crops in culture
    // 2- collect all extra parcels
    // 3- Set not loaned parcels and not use for the kitchen to Not Assigned

    //Collect all unused parcels of potential loaners

    val eP = theoriticalCroping.map { case (k, parcels) =>
      k-> getParcelUsages(k, parcels)
    }
    val inexcessFromCultivatedParcelsByKitchen = eP.map{case(k,pu)=> k.id-> pu.inexcessFromCultivatedParcels}.toMap
    val allParcelUsages = ParcelUsages(eP.flatMap(_._2.cultivated), eP.flatMap(_._2.forLoan), eP.flatMap(_._2.notLoanable))

    //Collect all demanding kitchens except provisioning crops strategies (a kitchen provisioning food is not supposed to ask for a loan)
    val demandingKitchens = theoriticalCroping.filter(_._1.cropingStrategy match {
      case PeanutForInexcess(savingRate) if savingRate > 0 => false
      case _ => true
    }).map { case (k, parcels) =>
      Kitchen.foodBalance(parcels, k)
    }.filter(_.balance < 0)

    // Compute loans and store them in history sequence
    val (yearLoans, notUsedInLoanProcess) = dscatt.Loan.assign(simulationState.year, allParcelUsages.forLoan, demandingKitchens)
    val loanedParcels = yearLoans.map(l => l.parcel.copy(farmerID = l.to, crop = toMilIfFallow(l.parcel)))

    // The world parcels are a partition of needed parcels, not loanable parcels, loaned percels, and loanable (but not use in loan process) parcels
    val newParcels = allParcelUsages.cultivated ++ loanedParcels ++ allParcelUsages.notLoanable ++ notUsedInLoanProcess

    (simulationState.copy(
      world = simulationState.world.copy(parcels = newParcels),
      history = simulationState.history.updateLoans(simulationState.year, yearLoans, newParcels),
      kitchens = simulationState.kitchens.map(k=> k.copy(inexcessHistory = k.inexcessHistory :+ inexcessFromCultivatedParcelsByKitchen.getOrElse(k.id, 0.0)))
    ), autonomousFoodBalance)
  }

  case class ParcelUsages(cultivated: Seq[Parcel], forLoan: Seq[Parcel], notLoanable: Seq[Parcel], inexcessFromCultivatedParcels: Double = 0.0)

  // Extra is defined as everything except what the kitchen needs
  def getParcelUsages(kitchen: Kitchen, parcels: Seq[Parcel])(using Fertility.AgronomicMetricsByParcel): ParcelUsages =
    val (fallowsNotCultivated, parcelCandidatesForCulture) =
      val grouped = parcels.groupBy(_.crop)
      kitchen.ownFallowUse match {
        case NeverUseFallow => (grouped(Fallow), grouped(Mil) ++ grouped(Peanut))
        case UseFallowIfNeeded => (Seq(), grouped(Mil) ++ grouped(Peanut) ++ grouped(Fallow))
      }

    //FIXME: use the appropriate computation for exceedingProportion
    val cropNeeded: Kitchen.CropNeeded = kitchen.cropingStrategy match {
      case PeanutForInexcess(exceedingProportion: Double) => Kitchen.getCropNeeded(kitchen, parcelCandidatesForCulture, Some(Kitchen.foodNeeds(kitchen) * (1 + exceedingProportion)))
    }

    val notInCulture = fallowsNotCultivated ++ cropNeeded.candidatesNotUsed
    // val fallowFreeExtraLoanCandidates = parcelCandidatesForCulture diff parcelsNeeded

    val (notLoanable, loanable) = kitchen.loanStrategy match {
      case Selfish => (notInCulture, Seq())
      case AllExtraParcelsLoaner => (Seq(), notInCulture)
      case ExtraParcelsExceptFallowLoaner => (fallowsNotCultivated, cropNeeded.candidatesNotUsed)
    }

    ParcelUsages(cropNeeded.cultivatedParcels, loanable, notLoanable, cropNeeded.inexcessOnCultivatedParcels)


  def sortUncultivatedParcels(uncultivatedParcelsByKitchen: Map[Kitchen, Seq[Parcel]]) =
    uncultivatedParcelsByKitchen.map: u=>
      u._1.cropingStrategy match
        case PeanutForInexcess(exceedingProportion)=>



  def toMilIfFallow(parcel: Parcel): Crop = {
    parcel.crop match {
      case Fallow => Mil
      case c: Crop => c
    }
  }

}
