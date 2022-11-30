package dscatt

import dscatt.Croping.{Crop, Fallow, Mil, NotAssigned, Peanut, Three}
import dscatt.Kitchen.FoodBalance
import dscatt.Loan.Loan
import dscatt.Simulation.SimulationState
import org.apache.commons.math3.random.MersenneTwister

object Rotation {
  def evolve(simulationState: SimulationState)(using MersenneTwister): (SimulationState, Seq[FoodBalance]) = {


    // Compute theoritical crops for coming year before we know if it is in culture or not
    val theoriticalCroping = simulationState.kitchens.map { k =>
      k -> World.ownedParcelsForKitchen(simulationState.world, k).map { p =>
        val newCropZone = Croping.evolveCropZone(p.cropZone, k.rotationCycle)
        p.copy(cropZone = newCropZone,
          crop = Croping.evolveCrop(p.crop, k.rotationCycle, newCropZone)
        )
      }
    }
    
    val autonomousFoodBalance = theoriticalCroping.map { case (k, ps) => Kitchen.foodBalance(ps, k) }

    // Loaners process:
    // 1- have a positive foodBalance with all crops in culture
    // 2- collect all extra parcels
    // 3- Set not loaned parcels and not use for the kitchen to Not Assigned

    //Collect all unused parcels of potential loaners
    val (extraParcelsForLoan, notAssignedExtraParcels) = {
      val eP = theoriticalCroping.map { case (k, parcels) =>
        getExtraParcels(k, parcels)
      }
      (eP.flatMap(_.forLoan), eP.flatMap(_.notAssigned))
    }
    
    //Collect all demanding kitchens except provisioning crops strategies (a kitchen provisioning food is not supposed to ask for a loan)
    val demandingKitchens = theoriticalCroping.filter(_._1.cropingStrategy match {
      case Provisioning(_) => false
      case _ => true
    }).map { case (k, parcels) =>
      Kitchen.foodBalance(parcels, k)
    }.filter(_.balance < 0)

    val (yearLoans, notUsedInLoanProcess) = dscatt.Loan.assign(simulationState.year, extraParcelsForLoan, demandingKitchens)

    //Keep all parcels not involved in the loan process
    val unchangedParcels = theoriticalCroping.flatMap {
      _._2
    } diff (extraParcelsForLoan ++ notAssignedExtraParcels)

  
    //Process the effective loan (farmerID is changed)
    val changedParcels: Seq[Parcel] = {
      yearLoans.map(yl => yl.parcel.copy(farmerID = yl.to, crop = setMilIfFallow(yl.parcel)))
    } ++ (notUsedInLoanProcess ++ notAssignedExtraParcels).map { p => p.copy(crop = NotAssigned) }

    val newParcels = unchangedParcels ++ changedParcels

    (simulationState.copy(
      world = simulationState.world.copy(parcels = newParcels),
      history = simulationState.history.updateLoans(simulationState.year, yearLoans, newParcels)
    ), autonomousFoodBalance)
  }

  case class ExtraParcels(forLoan: Seq[Parcel], notAssigned: Seq[Parcel])

  // Extra is defined as everything except what the kitchen needs
  def getExtraParcels(kitchen: Kitchen, parcels: Seq[Parcel]): ExtraParcels = {

    val parcelCandidatesForCulture = parcels.filterNot(_.crop == Fallow)

    val parcelsNeeded = kitchen.cropingStrategy match {
      case Parsimonious => Kitchen.cropNeeds(kitchen, parcelCandidatesForCulture, Some(Kitchen.foodNeeds(kitchen)))
      case Provisioning(exceedingProportion) => Kitchen.cropNeeds(kitchen, parcelCandidatesForCulture, Some(Kitchen.foodNeeds(kitchen) * (1 + exceedingProportion)))
      case AsMuchAsWeCan => parcelCandidatesForCulture
    }

    val fallowFreeExtraCandidates = parcelCandidatesForCulture diff parcelsNeeded

    val (notLoanable, loanable) = kitchen.loanStrategy match {
      case Selfish => (fallowFreeExtraCandidates, Seq())
      case AllExtraParcelsLoaner => (Seq(), parcels diff parcelsNeeded)
      case ExtraParcelsExceptFallowLoaner => (Seq(), fallowFreeExtraCandidates)
    }

    ExtraParcels(loanable, notLoanable)
  }

  def setMilIfFallow(parcel: Parcel) = parcel.crop match {
    case Fallow => Mil
    case c: Crop => c
  }

}
