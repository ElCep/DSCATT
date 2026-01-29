package dscatt.launchers

import dscatt.{Croping, KitchenProfiler, MeanStd, Parcel, RotationCycle}
import Croping.*

package object commonSettings:
  case class HookFile(outputPath: String, parcels: Boolean, kitchens: Boolean, dynamics: Boolean)

  case class HookParameters(displayParcels: Boolean = true, displayKitchens: Boolean = false, hookFile: Option[HookFile])

  val seed = 7770

  val hookFile = HookFile(
    outputPath = "/tmp",
    parcels = false,
    kitchens = true,
    dynamics = true
  )

  def hooks = HookParameters(
    displayParcels = false,
    displayKitchens = false,
    hookFile = None
  )


  val defaultKitchenProfiler =
    val manureDepositStategyMilNextYear = { (p: Parcel, r: RotationCycle) =>
      Croping.evolveCrop(p.crop, r, Croping.evolveCropZone(p.cropZone, r)) == Millet
    }

    val distributionBuilder =
      MeanStd(
        kitchenMean = 13.0,
        kitchenStd = 8.0,
        solidarityMean = 5.0,
        solidarityStd = 1.0,
        soilCareMean = 10.0,
        soilCareStd = 5.0,
        mutualizedHerdGrazingMean = 8.0,
        mutualizedHerdGrazingStd = 8.0,
        faidherbiaMean = 12.0,
        faidherbiaStd = 2.0,
        maxFaidherbia = 12,
        breederMean = 0.5,
        breederStd = 0.3,
        maxBreeder = 0.7
      )


    val kp: KitchenProfiler = KitchenProfiler.build(
      nbKitchenProfile = 2,
      initialTotalNumberOfKitchen = 31,
      initialKitchenSize = 16,
      drySeasonManureCriteria = manureDepositStategyMilNextYear,
      distributionBuilder = distributionBuilder,
      seed = seed
    )

    kp