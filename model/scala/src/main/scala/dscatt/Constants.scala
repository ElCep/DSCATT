package dscatt

object Constants {

  // FOOD
  val DAILY_FOOD_NEED_PER_PERSON = 0.75
  val MIL_YIELD_PER_M2 = 600.0 / 10000
  val PEANUT_YIELD_PER_M2 = 400.0 / 10000
  val PEANUT_FOOD_EQUIVALENCE = 0.5 // quantity of food / quantity of peanuts

  //KITCHEN
  val KITCHEN_MINIMUM_SIZE = 3
  
  //SPACE // to adjust the generated area to a 200 people village
  val AREA_FACTOR = {
    val foodFor200 = 200 * DAILY_FOOD_NEED_PER_PERSON * 365
    val surfaceFor200 = foodFor200.toDouble / (0.5 * (MIL_YIELD_PER_M2 + PEANUT_YIELD_PER_M2 * PEANUT_FOOD_EQUIVALENCE))
    surfaceFor200 / (238050 * 2.0 / 3) // 238050 = generated shp cultivated surface
  }
  val VILLAGE_ZONE_DISTANCE = 50.0 // in meters

}
