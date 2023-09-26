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
    implicit val theoreticalFertilityMetricsByParcel: Fertility.AgronomicMetricsByParcel = theoriticalCroping.flatMap{_._2}.map { p =>
      p.id -> Fertility.agronomicMetrics(p, soilQualityBasis)
    }.toMap

    val autonomousFoodBalance = theoriticalCroping.map { case (k, ps) => Kitchen.foodBalance(ps, k) }
    // Loaners process:
    // 1- have a positive foodBalance with all crops in culture
    // 2- collect all extra parcels
    // 3- Set not loaned parcels and not use for the kitchen to Not Assigned

    //Collect all unused parcels of potential loaners
    val parcelUsages = {
      val eP = theoriticalCroping.map { case (k, parcels) =>
        getParcelUsages(k, parcels)
      }
      ParcelUsages(eP.flatMap(_.cultivated), eP.flatMap(_.forLoan), eP.flatMap(_.notLoanable))
    }

    //Collect all demanding kitchens except provisioning crops strategies (a kitchen provisioning food is not supposed to ask for a loan)
    val demandingKitchens = theoriticalCroping.filter(_._1.cropingStrategy match {
      case Provisioning(_) => false
      case _ => true
    }).map { case (k, parcels) =>
      Kitchen.foodBalance(parcels, k)
    }.filter(_.balance < 0)

    val (yearLoans, notUsedInLoanProcess) = dscatt.Loan.assign(simulationState.year, parcelUsages.forLoan, demandingKitchens)
    val loanedParcels = yearLoans.map(l=> l.parcel.copy(farmerID = l.to, crop = toMilIfFallow(l.parcel)))
  
    // The world parcels are a partition of needed parcels, not loanable parcels, loaned percels, and loanable (but not use in loan process) parcels
    val newParcels = parcelUsages.cultivated ++
      parcelUsages.notLoanable.map(toNotAssignedIfNotFallow(_)) ++
      loanedParcels ++
      notUsedInLoanProcess.map(toNotAssignedIfNotFallow(_))
    
    (simulationState.copy(
      world = simulationState.world.copy(parcels = newParcels),
      history = simulationState.history.updateLoans(simulationState.year, yearLoans, newParcels)
    ), autonomousFoodBalance)
  }

  case class ParcelUsages(cultivated: Seq[Parcel], forLoan: Seq[Parcel], notLoanable: Seq[Parcel])

  // Extra is defined as everything except what the kitchen needs
  def getParcelUsages(kitchen: Kitchen, parcels: Seq[Parcel])(using Fertility.AgronomicMetricsByParcel): ParcelUsages = {

    //FIXME Use the OwnFallowUse if the kitchen is deficitaire
    val (fallowsNotCultivated, parcelCandidatesForCulture) = kitchen.ownFallowUse match {
      case NeverUseFallow=> parcels.partition(_.crop == Fallow)
      case UseFallowIfNeeded=> (Seq(),parcels)
    }

    //val parcelCandidatesForCulture = parcels.filterNot(_.crop == Fallow)

    val (parcelsCultivated, candidatesNotUsed) = kitchen.cropingStrategy match {
      case Parsimonious => Kitchen.getCropNeeded(kitchen, parcelCandidatesForCulture, Some(Kitchen.foodNeeds(kitchen)))
      case Provisioning(exceedingProportion) => Kitchen.getCropNeeded(kitchen, parcelCandidatesForCulture, Some(Kitchen.foodNeeds(kitchen) * (1 + exceedingProportion)))
      case AsMuchAsWeCan => parcelCandidatesForCulture
    }

    val notInCulture = fallowsNotCultivated ++ candidatesNotUsed
   // val fallowFreeExtraLoanCandidates = parcelCandidatesForCulture diff parcelsNeeded

    val (notLoanable, loanable) = kitchen.loanStrategy match {
      case Selfish => (notInCulture, Seq())
      case AllExtraParcelsLoaner => (Seq(), notInCulture)
      case ExtraParcelsExceptFallowLoaner => (fallowsNotCultivated, candidatesNotUsed)
    }

    ParcelUsages(parcelsCultivated, loanable, notLoanable)
  }

  def toMilIfFallow(parcel: Parcel): Crop = {
    parcel.crop match {
      case Fallow => Mil
      case c: Crop => c
    }
  }

  def toNotAssignedIfNotFallow(parcel: Parcel): Parcel = {
    val c = parcel.crop match {
      case Fallow => parcel.crop
      case _ => NotAssigned
    }
    parcel.copy(crop = c)
  }

}
