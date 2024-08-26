package dscatt

import dscatt.RotationCycle.MilletOnly

object Croping {

  sealed trait Crop

  object Millet extends Crop

  object Peanut extends Crop

  object Fallow extends Crop

  implicit class ACrop(c: Crop) {
    def display = c match {
      case Millet => "Millet"
      case Fallow => "Fallow"
      case Peanut => "Peanut"
    }
  }

  sealed trait CropZone

  object One extends CropZone

  object Two extends CropZone

  object Three extends CropZone

  def intToCropZone(cz: Int, rotationCycle: RotationCycle, parcelID: Int): CropZone = cz match
    case 1 => One
    case 2 => Two
    case 3 => rotationCycle match
      case RotationCycle.MilletPeanut | RotationCycle.MilletFallow if (parcelID % 2 == 0) => One
      case RotationCycle.MilletPeanut | RotationCycle.MilletFallow=> Two
      case RotationCycle.FallowMilletPeanut => Three
      case RotationCycle.MilletOnly=> One
    

  def evolveCropZone(cropZone: CropZone, rotationCycle: RotationCycle): CropZone =
    rotationCycle match
      // In this case, reassign at the begiging cropZones into 2 cropZones only
      case RotationCycle.MilletPeanut | RotationCycle.MilletFallow => cropZone match
        case One => Two
        case _ => One
      case RotationCycle.FallowMilletPeanut => cropZone match
        case One => Two
        case Two => Three
        case Three => One
      case RotationCycle.MilletOnly=> One

  def evolveCrop(crop: Crop, rotationCycle: RotationCycle, targetCropZone: CropZone) =
    rotationCycle match 
      case RotationCycle.MilletPeanut => targetCropZone match
        case One => Millet
        case _ => Peanut
      case RotationCycle.MilletFallow => targetCropZone match
        case One=> Millet
        case _=> Fallow
      case RotationCycle.FallowMilletPeanut =>
        targetCropZone match
          case One => Millet
          case Two => Peanut
          case Three => Fallow
      case RotationCycle.MilletOnly=> Millet
    

  implicit class AParcel(parcel: Parcel):
    def setFallowIfCropZoneThree = parcel.cropZone match
      case Three => parcel.copy(crop = Fallow)
      case _ => parcel

    def updateCrops = parcel.cropZone match
      case Two => parcel.copy(crop = Peanut)
      case Three=> parcel.copy(crop = Fallow)
      case _ => parcel
}

