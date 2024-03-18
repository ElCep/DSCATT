package dscatt

import dscatt.CropingStrategy.PeanutForInexcess
import dscatt.Simulation.SimulationState


enum SwitchType:
  case RainFall(r: Int) extends SwitchType
  case Faidherbia(nb: Int) extends SwitchType
  case Solidarity(l: LoanStrategy, fd: FoodDonationStrategy) extends SwitchType
  case Rotation(rC: RotationCycle) extends SwitchType
  case PeanutInexess(savingRate: Double) extends SwitchType
  case Grazing(dryHerdStrategy: HerdGrazingStrategy, wetHerdStrategy: HerdGrazingStrategy) extends SwitchType
  case Mulching(leftOnTheGroundRatio: Double) extends SwitchType

import SwitchType._

case class Switcher(time: Int, swithType: SwitchType)

implicit class SimulationStateWrapper(simulationState: SimulationState) {
  def enventuallySwitch(switcher: Switcher, data: Data): (SimulationState, Data) =
    println("TIME " + switcher.time + " Yean " + simulationState.year)
    if (switcher.time == simulationState.year) {

      switcher.swithType match
        case RainFall(r) =>
          (simulationState, data.copy(r))
        case Faidherbia(nb) =>
          val newKitchens = simulationState.kitchens.map(_.copy(nbFaidherbia = nb))
          val newParcels = simulationState.world.parcels.map(p =>
            p.copy(faidherbiaTrees = nb * p.area)
          )
          val switchedState = simulationState.copy(kitchens = newKitchens, world = simulationState.world.copy(parcels = newParcels))
          (switchedState, data)
        case Solidarity(l: LoanStrategy, fd: FoodDonationStrategy) =>
          val newKitchens = simulationState.kitchens.map(_.copy(loanStrategy = l, foodDonationStrategy = fd))
          val switchedState = simulationState.copy(kitchens = newKitchens)
          (switchedState, data)
        case Rotation(rC: RotationCycle)=>
          val newKitchens = simulationState.kitchens.map(_.copy(rotationCycle = rC))
          val switchedState = simulationState.copy(kitchens = newKitchens)
          (switchedState, data)
        case PeanutInexess(savingRate)=>
          val newKitchens = simulationState.kitchens.map(_.copy(cropingStrategy = CropingStrategy.PeanutForInexcess(savingRate)))
          val switchedState = simulationState.copy(kitchens = newKitchens)
          (switchedState, data)
        case Grazing(d: HerdGrazingStrategy, w: HerdGrazingStrategy)=>
          val newKitchens = simulationState.kitchens.map(_.copy(drySeasonHerdStrategy = d, wetSeasonHerdStrategy = w))
          val switchedState = simulationState.copy(kitchens = newKitchens)
          (switchedState, data)
        case Mulching(leftOnTheGroundRatio)=>
          val newKitchens = simulationState.kitchens.map(_.copy(mulchingStrategy = MulchingStrategy.Mulching(leftOnTheGroundRatio)))
          val switchedState = simulationState.copy(kitchens = newKitchens)
          (switchedState, data)
    }
    else (simulationState, data)

}