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

  sealed trait CropingStrategy

  object ThreeYears extends CropingStrategy

  object TwoYears extends CropingStrategy

  case class Rotation(crop: Crop, cropZone: CropZone)

  implicit def intToCropZone(cz: Int): CropZone = cz match
    case 1 => One
    case 2 => Two
    case 3 => Three

  def evolve(rotation: Rotation, cropingStrategy: CropingStrategy) = {
    cropingStrategy match {
      // In this case, reassign at the begiging cropZones into 2 cropZones only
      case TwoYears => rotation.copy(
        crop = rotation.crop match {
          case Peanut => Mil
          case Mil => Peanut
          case NotAssigned => rotation.cropZone match {
            case One => Mil
            case Two => Peanut
            case x: Crop=> x
          }
        }
      )
      case ThreeYears => rotation.copy(
        rotation.crop match {
          case Peanut => Fallow
          case Mil => Peanut
          case Fallow => Mil
          case NotAssigned => rotation.cropZone match {
            case One => Mil
            case Two => Peanut
            case Three => Fallow
            case Village=> NotAssigned
          }
        }
      )
    }
  }
}

