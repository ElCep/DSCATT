package dscatt

object Constants {

  type KG = Double
  type HA = Double
  type KG_BY_HA = Double
  type NB_BY_HA = Double
  type MM = Int

  // FOOD
  // 0.825: ratio of people eating a 100% meal
  // Age distribution: 0-4y: 8%, 5-19: 19%, 19+: 73%
  // we consider that 0-4y do not eat millet, 5-19y a 50% quantity and 19+ a full quantity
  // 0 * 0.06 + 0,5 * 0,4 + 0,54 = 0,74
  val DAILY_FOOD_NEED_PER_PERSON = 0.75 * 0.74 // 0.555

  // Actor word yields. Used the first year (when field boosts are unknown)
  val MIL_YIELD_PER_M2 = 600.0 / 10000
  val PEANUT_YIELD_PER_M2 = 400.0 / 10000

  // FIXME "var" for calibration only
  var PEANUT_FOOD_EQUIVALENCE = 1.2443 // quantity of food / quantity of peanuts
  // Considering that  0.66 workers per kitchen is enough and that they need 273*15=4095kg / year,
  // and a yield of 500kg / ha in mean, it comes 0.8ha / worker
  val CULTIVATED_AREA_PER_WORKER: HA = 15
  val WORKERS_RATIO_PER_KITCHEN = 0.66 // Source: Robert and co

  //FIXME "var" for calibration only
  val KITCHEN_MINIMUM_SIZE = 8
  val KITCHEN_MAXIMUM_SIZE = 31
  val SPLIT_KITCHEN_OFFSPRING_SIZE = 13
  //val KITCHEN_SIZE_THRESHOLD_FOR_ABSORPTION = KITCHEN_MAXIMUM_SIZE - SPLIT_KITCHEN_OFFSPRING_SIZE

  //SPACE // to adjust the generated area to a 180 ha
  val AREA_FACTOR = 180.0 / 500000 // (Mariana Odru, 2013 (177.1 ha))

  //FERTILITY
  type FertilityBoost = Double

  val FERTILITY_BOOST_PER_MANURE_KG_PER_HA = 0.000075 // Converted from the data: "A boost of +15% with 2T per ha"
  val KG_OF_MANURE_PER_COW_PER_YEAR: KG = 2000
  val EXPANDING_HERD_SIZE = 1.0 // Expanding herd size due to outside village grazing

  // Nitrogen
  val ATMOSPHERIC_NITROGEN: KG_BY_HA = 27.5 // kg/ha
  val NITROGEN_MINERALIZATION: KG_BY_HA = 12 // kg/ha
  val NITROGEN_PROPORTION_PER_MANURE_KG = 0.0238 // no dimension

//  val MIL_FULL_POTENTIAL_YIELD: KG_BY_HA = 3775 // kg DM / ha
//  val PEANUT_FULL_POTENTIAL_YIELD: KG_BY_HA = 1300 // kg DM / ha
//  val FALLOW_FULL_POTENTIAL_YIELD: KG_BY_HA = 1498 // kg DM / ha

  val MIL_STRAW_RATIO = 0.7
  val PEANUT_STRAW_RATIO = 0.666
  val MIL_SEED_RATIO = 0.3
  val PEANUT_SEED_RATIO = 0.333

  //val MIL_SEED_FULL_POTENTIAL_YIELD: KG_BY_HA = Constants.MIL_FULL_POTENTIAL_YIELD * Constants.MIL_SEED_RATIO
  //val PEANUT_SEED_FULL_POTENTIAL_YIELD: KG_BY_HA = Constants.PEANUT_FULL_POTENTIAL_YIELD * Constants.PEANUT_SEED_RATIO


  //Herd
  val KG_OF_STRAW_PER_COW_PER_YEAR: KG = 3650

  // Fertility
  var FALLOW_BOOST = 0.3549
  var SOIL_QUALITY_BASIS = 0.1

}