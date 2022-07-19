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

  object Village extends CropZone

  sealed trait CroppingStrategy

  object ThreeYears extends CroppingStrategy

  object TwoYears extends CroppingStrategy

  implicit def intToCropZone(cz: Int): CropZone = cz match
    case 1 => One
    case 2 => Two
    case 3 => Three

  def evolveCropZone(cropZone: CropZone, cropingStrategy: CroppingStrategy): CropZone = {
    cropingStrategy match {
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

  def evolveCrop(crop: Crop, cropingStrategy: CroppingStrategy, cropZone: CropZone) = {
    cropingStrategy match {
      case TwoYears => crop match {
        case Peanut => Mil
        case _ => Peanut
      }
      case ThreeYears => crop match {
        case Peanut => Fallow
        case Mil => Peanut
        case Fallow => Mil
        case NotAssigned =>
        //  println("NOT ASSIGNED " + cropZone)
          cropZone match {
          case One => Mil
          case Two => Peanut
          case Three => Fallow
          case Village => NotAssigned
        }
      }
    }
  }

  def foodFromPeanut(peanutWeight: Double) = Constants.PEANUT_FOOD_EQUIVALENCE * peanutWeight
}

