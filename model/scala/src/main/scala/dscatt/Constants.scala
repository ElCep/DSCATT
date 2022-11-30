package dscatt

object Constants {

  // FOOD
  val DAILY_FOOD_NEED_PER_PERSON = 0.75
  val MIL_YIELD_PER_M2 = 600.0 / 10000
  val PEANUT_YIELD_PER_M2 = 400.0 / 10000
  val PEANUT_FOOD_EQUIVALENCE = 0.5 // quantity of food / quantity of peanuts
  // Considering that  0.66 workers per kitchen is enough and that they need 273*15=4095kg / year,
  // and a yield of 500kg / ha in mean, it comes 0.8ha / worker
  val CULTIVATED_AREA_PER_WORKER = 15000
  val WORKERS_RATIO_PER_KITCHEN = 0.66 // Source: Robert and co

  //KITCHEN
  val KITCHEN_MINIMUM_SIZE = 3
  val KITCHEN_MAXIMUM_SIZE = 16
  val SPLIT_KITCHEN_OFFSPRING_SIZE = 4
  val KITCHEN_SIZE_THRESHOLD_FOR_ABSORPTION = KITCHEN_MAXIMUM_SIZE - SPLIT_KITCHEN_OFFSPRING_SIZE

  //SPACE // to adjust the generated area to a 200 people village
  val AREA_FACTOR = {
    val foodFor200 = 200 * DAILY_FOOD_NEED_PER_PERSON * 365
    val surfaceFor200 = foodFor200.toDouble / (0.5 * (MIL_YIELD_PER_M2 + PEANUT_YIELD_PER_M2 * PEANUT_FOOD_EQUIVALENCE))
    surfaceFor200 / (500000 * 2.0 / 3) // 500000 = generated shp cultivated surface
  }

  //FERTILITY
  type FertilityBoost = Double
  val INITIAL_FERTILITY_PER_PARCEL = 1.0

  val FERTILITY_BOOST_PER_MANURE_KG_PER_HA = 0.000075  // Converted from the data: "A boost of +15% with 2T per ha"
  val KG_OF_MANURE_PER_COW_PER_YEAR = 400

  val FERTILITY_BOOST_PER_FERTILIZER_KG = 0.012

  val MIL_EFFECT_ON_FERTILITY_BOOST = 0.98  // drop of 2% per year
  val PEANUT_EFFECT_ON_FERTILITY_BOOST = 0.98 // drop of 2% per year
  val FALLOW_EFFECT_FERTILITY_BOOST = 0.99 // FIXME: to be confirmed

}