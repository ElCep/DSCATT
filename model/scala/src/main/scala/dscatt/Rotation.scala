package dscatt

object Rotation {
  def evolve(world: World, kitchens: Seq[Kitchen]): World = {

    val newParcelsWithCrops = kitchens.flatMap { k =>

      // Compute theoritical crops for coming year before we know if it is in culture or note
      val parcelCandidatesForKitchenK = World.parcelsForKitchen(world, k).map { p =>
        val newCropZone = Croping.evolveCropZone(p.cropZone, k.rotationCycle)
        p.copy(cropZone = newCropZone,
          crop = Croping.evolveCrop(p.crop, k.rotationCycle, newCropZone)
        )
      }

      val (notCultivableCandidatesForKitchenK, cultivableCandidatesForKitchenK) = parcelCandidatesForKitchenK.partition { p =>
        // Exclude fallow parcels
        p.crop == Croping.Fallow
      }

      k.cropingStrategy match {
        case Parsimonious =>
          val parcels = Kitchen.cropNeeds(k, cultivableCandidatesForKitchenK, Some(Kitchen.foodNeeds(k)))
          parcels ++: notCultivableCandidatesForKitchenK
        case Provisioning(exceedingProportion) =>
          val parcels = Kitchen.cropNeeds(k, cultivableCandidatesForKitchenK, Some(Kitchen.foodNeeds(k) * (1 + exceedingProportion)))
          parcels ++: notCultivableCandidatesForKitchenK
        case AsMuchAsWeCan =>
          // Needs are set on purpose to a big value so that it does not limit cultivation
          Kitchen.cropNeeds(k, cultivableCandidatesForKitchenK, None) ++: notCultivableCandidatesForKitchenK
      }
    }

    world.copy(parcels = newParcelsWithCrops)

  }
}
