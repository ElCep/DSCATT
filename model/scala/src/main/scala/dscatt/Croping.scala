package dscatt

object Croping {

  sealed trait Crop
  object Mil extends Crop
  object Peanut extends Crop
  object Fallow extends Crop
  object NotAssigned extends Crop
  object HutField extends Crop

  sealed trait CropZone
  object One extends CropZone
  object Two extends CropZone
  object Three extends CropZone
  object Village extends CropZone

  sealed trait RotationCycle
  object ThreeYears extends RotationCycle
  object TwoYears extends RotationCycle

  sealed trait CropingStrategy
  object Intensive extends CropingStrategy // all parcels in culture all the time
  object Parsimonious extends CropingStrategy // no more than necessary (needs)

  sealed trait LoanStrategy
  object Loaner extends LoanStrategy // loans its extra parcels
  object Selfish extends LoanStrategy

  implicit def intToCropZone(cz: Int): CropZone = cz match
    case 1 => One
    case 2 => Two
    case 3 => Three

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
        case Village => Village
      }
    }
  }

  def evolveCrop(crop: Crop, rotationCycle: RotationCycle, targetCropZone: CropZone, inCulture: Boolean) = {
    rotationCycle match {
      case TwoYears => crop match {
        case Peanut => Mil
        case _ => Peanut
      }
      case ThreeYears =>
        targetCropZone match {
          case Three => Fallow
          case Village => HutField
          case _ =>
            if (inCulture) {
              crop match {
                case Peanut => Fallow
                case Mil => Peanut
                case Fallow => Mil
                case _ =>
                  targetCropZone match {
                    case One => Mil
                    case Two => Peanut
                    case Three => Fallow
                    case Village => HutField
                  }
              }
            } else NotAssigned
        }
    }
  }

  def foodFromPeanut(peanutWeight: Double) = Constants.PEANUT_FOOD_EQUIVALENCE * peanutWeight
}

