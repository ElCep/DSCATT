package dscatt

import Croping.{Crop, Fallow, Millet, Peanut}
import Kitchen.{Food, parcelsFoodProduction}
import Simulation.SimulationState
import Croping.*
import Data.*

object Rotation {
  def evolve(simulationState: SimulationState, initialFood: Seq[Food], data: Data): (SimulationState, Seq[Food], Int) = {

    // Compute theoritical crops for coming year before we know if it is in culture or not
    val theoriticalCroping = simulationState.kitchens.map { k =>
      k -> World.ownedParcelsForKitchen(simulationState.world, k).map { p =>
        val newCropZone = Croping.evolveCropZone(p.cropZone, k.rotationCycle)
        p.copy(cropZone = newCropZone,
          crop = Croping.evolveCrop(p.crop, k.rotationCycle, newCropZone)
        )
      }
    }

    val theoriticalFallowParcels = World.fallowParcels(theoriticalCroping.flatMap(_._2)).length

    // Loaners process:
    // 1- have a positive foodBalance with all crops in culture
    // 2- collect all extra parcels
    // 3- Set not loaned parcels and not use for the kitchen to Not Assigned
    //Collect all unused parcels of potential loaners
    val parcelUsageByKitchen = theoriticalCroping.map { case (k, parcels) =>
      k -> getParcelUsages(k, parcels, data, simulationState.year)
    }.toMap

    val inexcessFromCultivatedParcelsByKitchen = parcelUsageByKitchen.map { case (k, pu) => k -> pu.inexcessFromCultivatedParcels }
    val allParcelUsages = ParcelUsages(
      parcelUsageByKitchen.flatMap(_._2.cultivated).toSeq,
      parcelUsageByKitchen.flatMap(_._2.forLoan).toSeq,
      parcelUsageByKitchen.flatMap(_._2.notLoanable).toSeq
    )

    //Collect all demanding kitchens except provisioning crops strategies (a kitchen provisioning food is not supposed to ask for a loan)
    val demandingKitchens = parcelUsageByKitchen.filter(_._1.cropingStrategy match {
      case CropingStrategy.PeanutForInexcess(savingRate) if savingRate > 0 => false
      case _ => true
    }).map { case (k, parcelUsage) =>
      Kitchen.foodBalance(parcelUsage.cultivated, k, data, simulationState.year)
    }.filter(_.balance < 0).toSeq

    // Compute loans and store them in history sequence
    //Sort parcel for loan in crop priority order (Mil>Peanut>Fallow)
    val groupedForLoan = allParcelUsages.forLoan.groupBy(_.crop)
    val sortedForLoan =
      groupedForLoan.getOrElse(Millet, Seq()).sortBy(_.farmerID) ++
      groupedForLoan.getOrElse(Peanut, Seq()).sortBy(_.farmerID) ++
      groupedForLoan.getOrElse(Fallow, Seq()).sortBy(_.farmerID)
    val (yearLoans, notUsedInLoanProcess) = Loan.assign(sortedForLoan, demandingKitchens.sortBy(_.kitchenID), data, simulationState.year)
    val loanedParcels = yearLoans.map(_.parcel)

    val inCulture = allParcelUsages.cultivated ++ allParcelUsages.notLoanable ++ notUsedInLoanProcess
    val newParcels = inCulture ++ loanedParcels

    val loanedParcelsByK = loanedParcels.groupBy(_.farmerID)
    val cultivatedParcelsByK = inCulture.groupBy(_.farmerID)

    val milParcels = World.milParcels(newParcels).groupBy(_.farmerID)
    val peanutParcels = World.peanutParcels(newParcels).groupBy(_.farmerID)

    val food = initialFood.map { f =>
      val cultivatedK = cultivatedParcelsByK.getOrElse(f.kitchenID, Seq())
      val loanedK = loanedParcelsByK.getOrElse(f.kitchenID, Seq())
      f.copy(
        fromCulture = parcelsFoodProduction(cultivatedK, data, simulationState.year),
        fromLoan = parcelsFoodProduction(loanedK, data, simulationState.year),
        inexess = inexcessFromCultivatedParcelsByKitchen.getOrElse(Kitchen.kitchen(simulationState.kitchens, f.kitchenID).get, 0.0),
        fromMil = parcelsFoodProduction(milParcels.getOrElse(f.kitchenID, Seq()), data, simulationState.year),
        milInCultureArea = milParcels.getOrElse(f.kitchenID, Seq()).map(_.area).sum,
        fromPeanut = parcelsFoodProduction(peanutParcels.getOrElse(f.kitchenID, Seq()), data, simulationState.year),
        peanutInCultureArea = peanutParcels.getOrElse(f.kitchenID, Seq()).map(_.area).sum
      )
    }

    (simulationState.copy(
      world = simulationState.world.copy(parcels = newParcels),
      history = simulationState.history.updateLoans(simulationState.year, yearLoans, newParcels)
    ), food, theoriticalFallowParcels)
  }


  case class ParcelUsages(cultivated: Seq[Parcel], forLoan: Seq[Parcel], notLoanable: Seq[Parcel], inexcessFromCultivatedParcels: Double = 0.0)

  // Extra is defined as everything except what the kitchen needs
  def getParcelUsages(kitchen: Kitchen, parcels: Seq[Parcel], data: Data, year: Int): ParcelUsages =
    val (fallowsNotCultivated, parcelCandidatesForCulture) =
      val grouped = parcels.groupBy(_.crop)
//      kitchen.ownFallowUse match {
//        case OwnFallowUse.NeverUseFallow => (grouped.getOrElse(Fallow, Seq()).sortBy(_.farmerID), grouped.getOrElse(Mil, Seq()).sortBy(_.farmerID)  ++ grouped.getOrElse(Peanut, Seq()).sortBy(_.farmerID))
//        case OwnFallowUse.UseFallowIfNeeded => (Seq(), grouped.getOrElse(Mil, Seq()).sortBy(_.farmerID)  ++ grouped.getOrElse(Peanut, Seq()).sortBy(_.farmerID)  ++ grouped.getOrElse(Fallow, Seq()).sortBy(_.farmerID))
//      }
      kitchen.ownFallowUse match {
        case OwnFallowUse.NeverUseFallow => (grouped.getOrElse(Fallow, Seq()), grouped.getOrElse(Millet, Seq()) ++ grouped.getOrElse(Peanut, Seq()))
        case OwnFallowUse.UseFallowIfNeeded => (Seq(), grouped.getOrElse(Millet, Seq()) ++ grouped.getOrElse(Peanut, Seq()) ++ grouped.getOrElse(Fallow, Seq()))
      }

    val cropNeeded: Kitchen.CropNeeded = kitchen.cropingStrategy match {
      case CropingStrategy.PeanutForInexcess(savingRate: Double) =>
        val maxProducedFood = parcelsFoodProduction(parcelCandidatesForCulture, data, year)
        val minProducedfood = Kitchen.foodNeeds(kitchen, data)
        val needs = minProducedfood + savingRate * (maxProducedFood - minProducedfood)

        Kitchen.getCropNeeded(kitchen, parcelCandidatesForCulture, needs, data, year)
    }

    val notInCulture = cropNeeded.candidatesNotUsed ++ fallowsNotCultivated

    val (notLoanable, loanable) = kitchen.loanStrategy match {
      case LoanStrategy.Selfish => (notInCulture, Seq())
      case LoanStrategy.AllExtraParcelsLoaner => (Seq(), notInCulture)
      case LoanStrategy.ExtraParcelsExceptFallowLoaner => (fallowsNotCultivated, cropNeeded.candidatesNotUsed)
    }

    ParcelUsages(cropNeeded.cultivatedParcels, loanable, notLoanable, cropNeeded.inexcessOnCultivatedParcels)

}
