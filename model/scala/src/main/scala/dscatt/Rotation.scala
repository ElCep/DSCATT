package dscatt

import dscatt.Croping.NotAssigned
import dscatt.Loan.Loan
import dscatt.Simulation.SimulationState
import org.apache.commons.math3.random.MersenneTwister

object Rotation {
  def evolve(simulationState: SimulationState)(using MersenneTwister): SimulationState = {


    // Compute theoritical crops for coming year before we know if it is in culture or not
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
    // 2- collect all extra parcels
    // 3- Set not loaned parcels and not use for the kitchen to Not Assigned

    //Collect all unused parcels of potential loaners
    val (extraParcelsForLoan, notAssignedExtraParcels) = {
      val eP = theoriticalCroping.map { case (k, parcels) =>
        getExtraParcels(k, parcels)
      }
      (eP.flatMap(_.forLoan), eP.flatMap(_.notAssigned))
    }

    //Collect all demanding kitchens
    val demandingKitchens = theoriticalCroping.map { case (k, parcels) => Kitchen.foodBalance(parcels, k) }.filter {
      _.balance < 0
    }

    //Collect loan list and not assigned parcels (not used in the loan process)
    val (yearLoans, notUsedInLoanProcess) = dscatt.Loan.assign(simulationState.year, extraParcelsForLoan, demandingKitchens)
    //val loanHistoryOfTheYear = loanHistory.records.groupBy(_.year).get(simulationState.year).getOrElse(Seq())

    //Keep all parcels not involved in the loan process
    val unchangedParcels = theoriticalCroping.flatMap {
      _._2
    } diff (extraParcelsForLoan ++ notAssignedExtraParcels)


    //Process the effective loan (farmerID is changed)
    val changedParcels: Seq[Parcel] = {
      extraParcelsForLoan.map { ep =>
        val loan = yearLoans.find(l => l.parcel.id == ep.id)
        loan match {
          case Some(l: dscatt.Loan.Loan) => ep.copy(farmerID = l.to)
          case None => ep.copy(crop = NotAssigned)
        }
      } ++ (notAssignedExtraParcels ++ notUsedInLoanProcess).map { p => p.copy(crop = NotAssigned) }
    }

    val newParcels = unchangedParcels ++ changedParcels

    simulationState.copy(
      world = simulationState.world.copy(parcels = newParcels),
      history = simulationState.history.updateLoans(simulationState.year, yearLoans, newParcels)
    )
  }

  case class ExtraParcels(forLoan: Seq[Parcel], notAssigned: Seq[Parcel])

  def getExtraParcels(kitchen: Kitchen, parcels: Seq[Parcel]): ExtraParcels = {

    val (notCultivableCandidatesForKitchenK, cultivableCandidatesForKitchenK) = kitchen.loanStrategy match {
      //AllExtraParcelLoaner ne cultive pas a priori sur de la fallow
      case AllExtraParcelsLoaner => (Seq(), parcels)
      case ExtraParcelsExceptFallowLoaner | Selfish => parcels.partition { p =>
        // Exclude fallow parcels
        p.crop == Croping.Fallow
      }
    }

    val parcelsNeeded = kitchen.cropingStrategy match {
      case Parsimonious => Kitchen.cropNeeds(kitchen, cultivableCandidatesForKitchenK, Some(Kitchen.foodNeeds(kitchen)))
      case Provisioning(exceedingProportion) => Kitchen.cropNeeds(kitchen, cultivableCandidatesForKitchenK, Some(Kitchen.foodNeeds(kitchen) * (1 + exceedingProportion)))
      case AsMuchAsWeCan =>
        // Needs are set on purpose to a big value so that it does not limit cultivation
        Kitchen.cropNeeds(kitchen, cultivableCandidatesForKitchenK, None) ++: notCultivableCandidatesForKitchenK
    }

    val extra = cultivableCandidatesForKitchenK diff parcelsNeeded

    kitchen.loanStrategy match {
      case Selfish => ExtraParcels(Seq(), extra)
      case _ => ExtraParcels(extra, Seq())
    }
  }

}
