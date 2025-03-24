package dscatt

import dscatt.HerdSizeStrategy.{FullCapacity, LSUByArea}
import dscatt.MulchingStrategy.{CropResidue, NoMulching}
import dscatt.SwitchType.Faidherbia

object Cost:

  object Faidherbia:
    def socialEffort(nbFaid: Double): Double =
      if (nbFaid <= 4)
        1.0
      else if ( 4 < nbFaid && nbFaid <= 8 )
        0.75 * nbFaid - 2
      else
        0.125 * nbFaid + 3

    def manpowerEffort(nbFaid: Double): Double =
        if (nbFaid <= 4) 0.0
        else if (4 < nbFaid && nbFaid <= 8) 0.75 * nbFaid - 3
        else (3.0 / 8) * nbFaid

  object PopulationGrowth:
    // Equivalence between population growth and nbChildPerWoman: nbChildPerWoman = 309 pG - 1.22
    def socialEffort(populationGrowth: Double) =
      if (populationGrowth <= 0.0104) 2
      else if (populationGrowth <= 0.0136) 1
      else 0

    def manpowerEffort(populationGrowth: Double): Double =
      if (populationGrowth <= 0.0104) 5
      else if (populationGrowth <= 0.0136) 7
      else 10

  object HerdGrazing:
    def drySeasonManPower(hg: HerdGrazingStrategy): Double =
        hg match
          case HerdGrazingStrategy.AnywhereAnyTime => 2
          case HerdGrazingStrategy.EverywhereByDayOwnerByNight => 3
          case HerdGrazingStrategy.OwnerOnly => 9
    
    def wetSeasonManPower(hg: HerdGrazingStrategy): Double =
        hg match
          case HerdGrazingStrategy.AnywhereAnyTime => 4
          case HerdGrazingStrategy.EverywhereByDayOwnerByNight => 3
          case HerdGrazingStrategy.OwnerOnly => 9
      
  implicit class ControlWrap(control: Control):
    def socialEffort: Double=
      control match
        case ls: LoanStrategy=>
          ls match
            case LoanStrategy.AllExtraParcelsLoaner => 9
            case LoanStrategy.ExtraParcelsExceptFallowLoaner => 2
            case LoanStrategy.Selfish => 10
        case ofu: OwnFallowUse=>
          ofu match
          case OwnFallowUse.NeverUseFallow => 2
          case OwnFallowUse.UseFallowIfNeeded=> 9
        case fd: FoodDonationStrategy=>
          fd match
            case FoodDonationStrategy.FoodForAllStrategy=> 1
            case FoodDonationStrategy.FoodForUsOnlyStrategy=> 10
        case hgs: HerdGrazingStrategy=>
          hgs match
            case HerdGrazingStrategy.AnywhereAnyTime=> 10
            case HerdGrazingStrategy.EverywhereByDayOwnerByNight=> 1
            case HerdGrazingStrategy.OwnerOnly=> 10
        case hss: HerdSizeStrategy=>
          hss match
            case LSUByArea(lsuByHa) => -( 90.0 / 7) * lsuByHa + 10 // SE = 1 for lsu = 0.7, SE = 10 for lsu = 0
        case ms: MulchingStrategy=>
          ms match
            case CropResidue=> 5
            case NoMulching=> 1


    def manpowerEffort: Double =
      control match
        case ls: LoanStrategy =>
          ls match
            case LoanStrategy.AllExtraParcelsLoaner => 0
            case LoanStrategy.ExtraParcelsExceptFallowLoaner => 0
            case LoanStrategy.Selfish => 0
        case ofu: OwnFallowUse =>
          ofu match
            case OwnFallowUse.NeverUseFallow => 0
            case OwnFallowUse.UseFallowIfNeeded => 9
        case fd: FoodDonationStrategy =>
          fd match
            case FoodDonationStrategy.FoodForAllStrategy => 1
            case FoodDonationStrategy.FoodForUsOnlyStrategy => 0
        case hss: HerdSizeStrategy =>
          hss match
            case LSUByArea(lsuByHa) => (100.0 / 7) * lsuByHa // ME = 0 for 0 cows, ME = 10 for lsu = 0.7
        case ms: MulchingStrategy =>
          ms match
            case CropResidue => 1
            case NoMulching => 8



