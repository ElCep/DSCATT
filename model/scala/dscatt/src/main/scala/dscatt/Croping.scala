package dscatt

import dscatt.RotationCycle.MilletOnly

object Croping {


  enum Crop:
    case Millet, Peanut, Fallow, NotAssignedYet

  import Crop.*

  implicit class ACrop(c: Crop) {
    def display = c match {
      case Millet => "Millet"
      case Fallow => "Fallow"
      case Peanut => "Peanut"
    }
  }

  //  sealed trait CropZone
  //
  //  object One extends CropZone
  //
  //  object Two extends CropZone
  //
  //  object Three extends CropZone


  def nextCrop(rotationCycle: RotationCycle, actual: Crop) = 
    rotationCycle match
      case RotationCycle.MilletPeanut =>
        actual match
          case Millet => Some(Peanut)
          case Peanut => Some(Millet)
          case _=> None
      case RotationCycle.FallowMilletPeanut =>
        actual match
          case Millet => Some(Peanut)
          case Peanut => Some(Fallow)
          case Fallow => Some(Millet)
          case _=> None
      case RotationCycle.MilletFallow =>
        actual match
          case Millet => Some(Fallow)
          case Fallow => Some(Millet)
          case _=> None
      case RotationCycle.MilletOnly => Some(Millet)
      case _=> None


  def length(rotationCycle: RotationCycle): Int =
    rotationCycle match
      case RotationCycle.FallowMilletPeanut=> 3
      case RotationCycle.MilletPeanut | RotationCycle.MilletFallow=> 2
      case RotationCycle.MilletOnly=> 1

  def firstIn(rotationCycle: RotationCycle): Crop =
    rotationCycle match
      case RotationCycle.MilletPeanut | RotationCycle.MilletFallow | RotationCycle.MilletOnly => Millet
      case RotationCycle.FallowMilletPeanut => Fallow

  def secondIn(rotationCycle: RotationCycle): Crop =
    rotationCycle match
      case RotationCycle.MilletPeanut => Peanut
      case RotationCycle.MilletFallow => Fallow
      case RotationCycle.FallowMilletPeanut | RotationCycle.MilletOnly => Millet

  def thirdIn(rotationCycle: RotationCycle): Crop =
    rotationCycle match
      case RotationCycle.MilletOnly => Millet
      case RotationCycle.FallowMilletPeanut => Peanut
      case RotationCycle.MilletPeanut | RotationCycle.MilletFallow => NotAssignedYet

  def intToCrop(cz: Int, rotationCycle: RotationCycle): Crop =
    cz match
      case 1 => firstIn(rotationCycle)
      case 2 => secondIn(rotationCycle)
      case 3 => thirdIn(rotationCycle)


  //  def intToCropZone(cz: Int, rotationCycle: RotationCycle, parcelID: Int): CropZone = cz match
  //    case 1 => One
  //    case 2 => Two
  //    case 3 => rotationCycle match
  //      case RotationCycle.MilletPeanut | RotationCycle.MilletFallow if (parcelID % 2 == 0) => One
  //      case RotationCycle.MilletPeanut | RotationCycle.MilletFallow=> Two
  //      case RotationCycle.FallowMilletPeanut => Three
  //      case RotationCycle.MilletOnly=> One
  //
  //
  //  def evolveCropZone(cropZone: CropZone, rotationCycle: RotationCycle): CropZone =
  //    cropZone match
  //      case One => Two
  //      case Two => Three
  //      case Three => One
  //    rotationCycle match
  //      // In this case, reassign at the begiging cropZones into 2 cropZones only
  //      case RotationCycle.MilletPeanut | RotationCycle.MilletFallow => cropZone match
  //        case One => Two
  //        case _ => One
  //      case RotationCycle.FallowMilletPeanut => cropZone match
  //        case One => Two
  //        case Two => Three
  //        case Three => One
  //      case RotationCycle.MilletOnly=> One

  //  def evolveCrop(crop: Crop, rotationCycle: RotationCycle, targetCropZone: CropZone) =
  //    rotationCycle match
  //      case RotationCycle.MilletPeanut => targetCropZone match
  //        case One => Millet
  //        case _ => Peanut
  //      case RotationCycle.MilletFallow => targetCropZone match
  //        case One=> Millet
  //        case _=> Fallow
  //      case RotationCycle.FallowMilletPeanut =>
  //        targetCropZone match
  //          case One => Millet
  //          case Two => Peanut
  //          case Three => Fallow
  //      case RotationCycle.MilletOnly=> Millet


  implicit class AParcel(parcel: Parcel):
    //    def setFallowIfCropZoneThree = parcel.cropZone match
    //      case Three => parcel.copy(crop = Fallow)
    //      case _ => parcel

    def updateCrops = {
      parcel.initiallyPlannedCrop match
      case Some(crop)=> parcel.copy(crop = crop, initiallyPlannedCrop = None)
      case _ => parcel
    }
}

