package dscatt

import Croping.{Crop, Fallow, Mil, Peanut, Three}
import Kitchen.{Food, FoodBalance, parcelsFoodProduction}
import Simulation.SimulationState
import org.apache.commons.math3.random.MersenneTwister
import Croping._
import Constants.*

object Rotation {
  def evolve(simulationState: SimulationState, soilQualityBasis: Double, initialFood: Seq[Food])(using MM): (SimulationState, Seq[Food], Int) = {


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
    
    //theariticalAgronomicMetrics
    // Compute soil quality and available nitrogen for each parcel on the trearitical crop rotation
    implicit val theoreticalFertilityMetricsByParcel: Fertility.AgronomicMetricsByParcel = theoriticalCroping.flatMap {
      _._2
    }.map { p =>
      p.id -> Fertility.agronomicMetrics(p, soilQualityBasis)
    }.toMap

    //val autonomousFoodBalance = theoriticalCroping.map { case (k, ps) => Kitchen.foodBalance(ps, k) }
    // Loaners process:
    // 1- have a positive foodBalance with all crops in culture
    // 2- collect all extra parcels
    // 3- Set not loaned parcels and not use for the kitchen to Not Assigned

    //Collect all unused parcels of potential loaners

    val parcelUsageByKitchen = theoriticalCroping.map { case (k, parcels) =>
      k.id -> getParcelUsages(k, parcels)
    }.toMap

    val inexcessFromCultivatedParcelsByKitchen = parcelUsageByKitchen.map { case (k, pu) => k -> pu.inexcessFromCultivatedParcels }.toMap
    val allParcelUsages = ParcelUsages(
      parcelUsageByKitchen.flatMap(_._2.cultivated).toSeq,
      parcelUsageByKitchen.flatMap(_._2.forLoan).toSeq,
      parcelUsageByKitchen.flatMap(_._2.notLoanable).toSeq
    )

    //Collect all demanding kitchens except provisioning crops strategies (a kitchen provisioning food is not supposed to ask for a loan)
    val demandingKitchens = theoriticalCroping.filter(_._1.cropingStrategy match {
      case CropingStrategy.PeanutForInexcess(savingRate) if savingRate > 0 => false
      case _ => true
    }).map { case (k, parcels) =>
      Kitchen.foodBalance(parcels, k)
    }.filter(_.balance < 0)

    // Compute loans and store them in history sequence
    val (yearLoans, notUsedInLoanProcess) = Loan.assign(simulationState.year, allParcelUsages.forLoan, demandingKitchens)
    val loanedParcels = yearLoans.map(l => l.parcel.copy(farmerID = l.to, crop = Mil))

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
        fromCulture = parcelsFoodProduction(cultivatedK),
        fromLoan = parcelsFoodProduction(loanedK),
        inexess = inexcessFromCultivatedParcelsByKitchen.getOrElse(f.kitchenID, 0.0),
        fromMil = parcelsFoodProduction(milParcels.getOrElse(f.kitchenID, Seq())),
        milInCultureArea = milParcels.getOrElse(f.kitchenID, Seq()).map(_.area).sum,
        fromPeanut = parcelsFoodProduction(peanutParcels.getOrElse(f.kitchenID, Seq())),
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
  def getParcelUsages(kitchen: Kitchen, parcels: Seq[Parcel])(using Fertility.AgronomicMetricsByParcel, MM): ParcelUsages =
    val (fallowsNotCultivated, parcelCandidatesForCulture) =
      val grouped = parcels.groupBy(_.crop)
      kitchen.ownFallowUse match {
        case OwnFallowUse.NeverUseFallow => (grouped.getOrElse(Fallow, Seq()), grouped.getOrElse(Mil, Seq()) ++ grouped.getOrElse(Peanut, Seq()))
        case OwnFallowUse.UseFallowIfNeeded => (Seq(), grouped.getOrElse(Mil, Seq()) ++ grouped.getOrElse(Peanut, Seq()) ++ grouped.getOrElse(Fallow, Seq()))
      }

    val cropNeeded: Kitchen.CropNeeded = kitchen.cropingStrategy match {
      case CropingStrategy.PeanutForInexcess(savingRate: Double) =>
        val maxProducedFood = parcelsFoodProduction(parcelCandidatesForCulture)
        val minProducedfood = Kitchen.foodNeeds(kitchen)
        val needs = minProducedfood + savingRate * (maxProducedFood - minProducedfood)

        Kitchen.getCropNeeded(kitchen, parcelCandidatesForCulture, needs)
    }

    val notInCulture = fallowsNotCultivated ++ cropNeeded.candidatesNotUsed
    // val fallowFreeExtraLoanCandidates = parcelCandidatesForCulture diff parcelsNeeded

    val (notLoanable, loanable) = kitchen.loanStrategy match {
      case LoanStrategy.Selfish => (notInCulture, Seq())
      case LoanStrategy.AllExtraParcelsLoaner => (Seq(), notInCulture)
      case LoanStrategy.ExtraParcelsExceptFallowLoaner => (fallowsNotCultivated, cropNeeded.candidatesNotUsed)
    }

    ParcelUsages(cropNeeded.cultivatedParcels, loanable, notLoanable, cropNeeded.inexcessOnCultivatedParcels)

  def toMilIfFallow(parcel: Parcel): Crop = {
    parcel.crop match {
      case Fallow => Mil
      case c: Crop => c
    }
  }

}
