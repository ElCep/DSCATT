package dscatt

object Constants {

  type KG = Double
  type HA = Double
  type KG_BY_HA = Double

  // FOOD
  val DAILY_FOOD_NEED_PER_PERSON = 0.75

  // Actor word yields. Used the first year (when field boosts are unknown)
  val MIL_YIELD_PER_M2 = 600.0 / 10000
  val PEANUT_YIELD_PER_M2 = 400.0 / 10000

  val PEANUT_FOOD_EQUIVALENCE = 0.5 // quantity of food / quantity of peanuts
  // Considering that  0.66 workers per kitchen is enough and that they need 273*15=4095kg / year,
  // and a yield of 500kg / ha in mean, it comes 0.8ha / worker
  val CULTIVATED_AREA_PER_WORKER: HA = 15
  val WORKERS_RATIO_PER_KITCHEN = 0.66 // Source: Robert and co

  //KITCHEN
  val KITCHEN_MINIMUM_SIZE = 3
  val KITCHEN_MAXIMUM_SIZE = 16
  val SPLIT_KITCHEN_OFFSPRING_SIZE = 4
  val KITCHEN_SIZE_THRESHOLD_FOR_ABSORPTION = KITCHEN_MAXIMUM_SIZE - SPLIT_KITCHEN_OFFSPRING_SIZE

  //SPACE // to adjust the generated area to a 200 people village in ha
  val AREA_FACTOR = {
    val foodFor200 = 200 * DAILY_FOOD_NEED_PER_PERSON * 365
    val surfaceFor200 = foodFor200.toDouble / (0.5 * (MIL_YIELD_PER_M2 + PEANUT_YIELD_PER_M2 * PEANUT_FOOD_EQUIVALENCE))
    surfaceFor200 / (500000 * 2.0 / 3) / 10000 // 500000 = generated shp cultivated surface
  }

  //FERTILITY
  type FertilityBoost = Double

  val FERTILITY_BOOST_PER_MANURE_KG_PER_HA = 0.000075 // Converted from the data: "A boost of +15% with 2T per ha"
  val KG_OF_MANURE_PER_COW_PER_YEAR: KG = 2000

  // Nitrogen
  val ATMOSPHERIC_NITROGEN: KG_BY_HA = 27.5 // kg/ha
  val NITROGEN_MINERALIZATION: KG_BY_HA = 12 // kg/ha
  val NITROGEN_PROPORTION_PER_MANURE_KG = 0.0238 // no dimension

  val MIL_FULL_POTENTIAL_YIELD: KG_BY_HA = 3775 // kg DM / ha
  val PEANUT_FULL_POTENTIAL_YIELD: KG_BY_HA = 450 // kg DM / ha
  val FALLOW_FULL_POTENTIAL_YIELD: KG_BY_HA = 1498 // kg DM / ha

  val MIL_STRAW_RATIO = 0.7
  val PEANUT_STRAW_RATIO = 0.666

  val MIL_SEED_RATIO = 0.3
  val PEANUT_SEED_RATIO = 0.333

  //Herd
  val KG_OF_STRAW_PER_COW_PER_YEAR: KG = 3650

}