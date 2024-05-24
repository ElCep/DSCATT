package dscatt

import dscatt.CropingStrategy.PeanutForInexcess
import dscatt.Simulation.SimulationState


enum SwitchType:
  case RainFall(r: Int) extends SwitchType
  case Faidherbia(nb: Int) extends SwitchType
  case Solidarity(l: LoanStrategy, fd: FoodDonationStrategy) extends SwitchType
  case OwnFallow(ofu: OwnFallowUse) extends SwitchType
  case Loan(strategy: LoanStrategy) extends SwitchType
  case FoodDonation(strategy: FoodDonationStrategy) extends SwitchType
  case Rotation(rC: RotationCycle) extends SwitchType
  case Grazing(dryHerdStrategy: HerdGrazingStrategy, wetHerdStrategy: HerdGrazingStrategy) extends SwitchType
  case DryGrazing(dryHerdStrategy: HerdGrazingStrategy) extends SwitchType
  case WetGrazing(wetHerdStrategy: HerdGrazingStrategy) extends SwitchType
  case HerdSize(herdSizeStrategy: HerdSizeStrategy) extends SwitchType
  case Mulching(mulchingStrategy: MulchingStrategy) extends SwitchType
  case Demography(populationGrowth: Double) extends SwitchType
  case PeanutSeedToFood(ratio: Double) extends SwitchType
  case PeanutInexcess(ratio: Double) extends SwitchType

import SwitchType._

case class Switcher(time: Int, switchType: SwitchType)

implicit class SimulationStateWrapper(simulationState: SimulationState) {
  private def kitchenToStateAndData(k: Seq[Kitchen], data: Data)=
    val switchedState = simulationState.copy(kitchens = k)
    (switchedState, data)
  
  def enventuallySwitch(switcher: Switcher, data: Data): (SimulationState, Data) =
    if (switcher.time == simulationState.year) {

      switcher.switchType match
        case RainFall(r) =>
          (simulationState, data.copyRainFall(r))
        case Faidherbia(nb) =>
          val newKitchens = simulationState.kitchens.map(_.copy(nbFaidherbiaByHa = nb))
          val newParcels = simulationState.world.parcels.map(p =>
            p.copy(faidherbiaTreesByHa = nb)
          )
          val switchedState = simulationState.copy(kitchens = newKitchens, world = simulationState.world.copy(parcels = newParcels))
          (switchedState, data)
        case Solidarity(l: LoanStrategy, fd: FoodDonationStrategy) =>
          val newKitchens = simulationState.kitchens.map(_.copy(loanStrategy = l, foodDonationStrategy = fd))
          kitchenToStateAndData(newKitchens, data)
        case OwnFallow(ofu: OwnFallowUse)=>
          val newKitchens = simulationState.kitchens.map(_.copy(ownFallowUse = ofu))
          kitchenToStateAndData(newKitchens, data)
        case Loan(ls: LoanStrategy)=>
          val newKitchens = simulationState.kitchens.map(_.copy(loanStrategy = ls))
          kitchenToStateAndData(newKitchens, data)
        case FoodDonation(fd: FoodDonationStrategy) =>
          val newKitchens = simulationState.kitchens.map(_.copy(foodDonationStrategy = fd))
          kitchenToStateAndData(newKitchens, data)
        case Rotation(rC: RotationCycle)=>
          val newKitchens = simulationState.kitchens.map(_.copy(rotationCycle = rC))
          kitchenToStateAndData(newKitchens, data)
        case Grazing(d: HerdGrazingStrategy, w: HerdGrazingStrategy)=>
          val newKitchens = simulationState.kitchens.map(_.copy(drySeasonHerdStrategy = d, wetSeasonHerdStrategy = w))
          kitchenToStateAndData(newKitchens, data)
        case DryGrazing(d: HerdGrazingStrategy)=>
          val newKitchens = simulationState.kitchens.map(_.copy(drySeasonHerdStrategy = d))
          kitchenToStateAndData(newKitchens, data)
        case WetGrazing(w: HerdGrazingStrategy)=>
          val newKitchens = simulationState.kitchens.map(_.copy(wetSeasonHerdStrategy = w))
          kitchenToStateAndData(newKitchens, data)
        case Mulching(mulchingStrategy: MulchingStrategy)=>
          val newKitchens = simulationState.kitchens.map(_.copy(mulchingStrategy = mulchingStrategy))
          kitchenToStateAndData(newKitchens, data)
        case HerdSize(hsStrategy)=>
          val newKitchens = simulationState.kitchens.map(_.copy(herdSizeStrategy = hsStrategy))
          kitchenToStateAndData(newKitchens, data)
        case Demography(populationGrowth)=>
          (simulationState, data.copyPopulationGrowth(populationGrowth))
        case PeanutSeedToFood(ratio: Double)=>
          (simulationState, data.copyPeanutSeedToFood(ratio))
        case PeanutInexcess(ratio: Double)=>
          val newKitchens = simulationState.kitchens.map(_.copy(cropingStrategy = CropingStrategy.PeanutForInexcess(ratio)))
          kitchenToStateAndData(newKitchens, data)
    }
    else (simulationState, data)

}