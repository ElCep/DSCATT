package dscatt

object Croping {

  sealed trait Crop

  object Mil extends Crop

  object Peanut extends Crop

  object Fallow extends Crop

  object NotAssigned extends Crop

  sealed trait CropZone

  object One extends CropZone

  object Two extends CropZone

  object Three extends CropZone

  def intToCropZone(cz: Int, rotationCycle: RotationCycle, threeIsOneProbabilityIf2Years: Boolean): CropZone = cz match
    case 1 => One
    case 2 => Two
    case 3 => rotationCycle match {
      case TwoYears if(threeIsOneProbabilityIf2Years)=> One
      case TwoYears=> Two
      case ThreeYears=> Three
    }

  def evolveCropZone(cropZone: CropZone, rotationCycle: RotationCycle): CropZone = {
    rotationCycle match {
      // In this case, reassign at the begiging cropZones into 2 cropZones only
      case TwoYears => cropZone match {
        case One => Two
        case _ => One
      }
      case ThreeYears => cropZone match {
        case One => Two
        case Two => Three
        case Three => One
      }
    }
  }

  def evolveCrop(crop: Crop, rotationCycle: RotationCycle, targetCropZone: CropZone) = {
    rotationCycle match {
      case TwoYears => targetCropZone match {
        case One=> Mil
        case _=> Peanut
      }
      case ThreeYears =>
        targetCropZone match {
          case Three => Fallow
          case _ =>
            crop match {
              case Peanut => Fallow
              case Mil => Peanut
              case Fallow => Mil
              case _ =>
                targetCropZone match {
                  case One => Mil
                  case Two => Peanut
                  case Three => Fallow
                }
            }
        }
    }
  }

  implicit class AParcel(parcel: Parcel) {
    def setFallowIfCropZoneThree = parcel.cropZone match {
      case Three => parcel.copy(crop = Fallow)
      case _ => parcel
    }
  }
  
  def foodFromPeanut(peanutWeight: Double) = Constants.PEANUT_FOOD_EQUIVALENCE * peanutWeight
}

