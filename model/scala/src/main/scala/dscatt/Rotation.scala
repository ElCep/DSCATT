package dscatt

import dscatt.Croping.NotAssigned
import dscatt.Loan.Loan
import dscatt.Simulation.SimulationState
import org.apache.commons.math3.random.MersenneTwister

object Rotation {
  def evolve(simulationState: SimulationState)(using MersenneTwister): SimulationState = {


    // Compute theoritical crops for coming year before we know if it is in culture or note
    val theoriticalCroping = simulationState.kitchens.map { k =>
      k -> World.ownedParcelsForKitchen(simulationState.world, k).map { p =>
        val newCropZone = Croping.evolveCropZone(p.cropZone, k.rotationCycle)
        p.copy(cropZone = newCropZone,
          crop = Croping.evolveCrop(p.crop, k.rotationCycle, newCropZone)
        )
      }
    }

    // theoriticalCroping.foreach { case (k, parcels) => println(Kitchen.foodBalance(parcels, k)) }

    // Loaners process:
    // 1- have a positive foodBalance with all crops in culture
    // 2- collect oall extra parcels
    // 3- Set not loaned parcels and not use for the kitchen to Not Assigned

    //Collect all unused parcels of potential loaners
    val extraParcels = theoriticalCroping.flatMap { case (k, parcels) =>
      extraParcelsForLoan(k, parcels)
    }

    //Collect all demanding kitchens
    val demandingKitchens = theoriticalCroping.map { case (k, parcels) => Kitchen.foodBalance(parcels, k) }.filter {
      _.balance < 0
    }

    //Collect loan list and not assigned parcels (not used in the loan process)
    val (loanHistory, notAssigned) = dscatt.Loan.assign(simulationState.year, extraParcels, demandingKitchens, simulationState.indicators.loanHistory)
    val loanHistoryOfTheYear = loanHistory.records.groupBy(_.year).get(simulationState.year).getOrElse(Seq())

    //Keep all parcels not involved in the loan process
    val unchangedParcels = theoriticalCroping.flatMap {
      _._2
    } diff extraParcels


    //Process the effective loan (farmerID is changed)
    val changedParcels: Seq[Parcel] = extraParcels.map { ep =>
      val loan = loanHistoryOfTheYear.find(l => l.parcel.id == ep.id)
      loan match {
        case Some(l: dscatt.Loan.Loan) => ep.copy(farmerID = l.to)
        case None => ep.copy(crop = NotAssigned)
      }
    }

    val newParcels = unchangedParcels ++ changedParcels

    simulationState.copy(
      world = simulationState.world.copy(parcels = newParcels),
      indicators = simulationState.indicators.copy(loanHistory = simulationState.indicators.loanHistory.append(loanHistory))
    )
  }

  def extraParcelsForLoan(kitchen: Kitchen, parcels: Seq[Parcel]) = {

    val (notCultivableCandidatesForKitchenK, cultivableCandidatesForKitchenK) = parcels.partition { p =>
      // Exclude fallow parcels
      p.crop == Croping.Fallow
    }

    // Cropping for good
    val parcelsNeeded = kitchen.cropingStrategy match {
      case Parsimonious => Kitchen.cropNeeds(kitchen, cultivableCandidatesForKitchenK, Some(Kitchen.foodNeeds(kitchen)))
      case Provisioning(exceedingProportion) => Kitchen.cropNeeds(kitchen, cultivableCandidatesForKitchenK, Some(Kitchen.foodNeeds(kitchen) * (1 + exceedingProportion)))
      case AsMuchAsWeCan =>
        // Needs are set on purpose to a big value so that it does not limit cultivation
        Kitchen.cropNeeds(kitchen, cultivableCandidatesForKitchenK, None) ++: notCultivableCandidatesForKitchenK
    }
    cultivableCandidatesForKitchenK diff parcelsNeeded
  }
}
