package dscatt

object Data {
  type KG = Double
  type HA = Double
  type KG_BY_HA = Double
  type NB_BY_HA = Double
  type MM = Int
  type SOIL_QUALITY = Double
  type SOIL_QUALITY_BY_HA = Double
  type SOIL_QUALITY_BY_KG = Double
}

import Data._

class Data(
            soilQualityBasis: Double, // exposed for calibration
            fallowBoost: Double, // exposed for calibration 
            erosion: Double, //exposed for calibratino
            peanutSeedToFood: Double, // exposed for calibration
            dailyFoodNeedPerPerson: Double,
            rainFall: MM
          ) {

  def copy(rFall: MM) = new Data(
    soilQualityBasis: Double,
    fallowBoost: Double,
    erosion: Double,
    peanutSeedToFood: Double,
    dailyFoodNeedPerPerson: Double,
    rFall
  )


  // FOOD
  // 0.825: ratio of people eating a 100% meal
  // Age distribution: 0-4y: 8%, 5-19: 19%, 19+: 73%
  // we consider that 0-4y do not eat millet, 5-19y a 50% quantity and 19+ a full quantity
  // 0 * 0.06 + 0,5 * 0,4 + 0,54 = 0,74
  // val DAILY_FOOD_NEED_PER_PERSON = 0.75 * 0.74 // 0.555
  val DAILY_FOOD_NEED_PER_PERSON = dailyFoodNeedPerPerson

  // Actor word yields. Used the first year (when field boosts are unknown)
  val MIL_YIELD_PER_M2 = 600.0 / 10000
  val PEANUT_YIELD_PER_M2 = 400.0 / 10000

  val PEANUT_FOOD_EQUIVALENCE = peanutSeedToFood // quantity of food / quantity of peanuts
  // Considering that  0.66 workers per kitchen is enough and that they need 273*15=4095kg / year,
  // and a yield of 500kg / ha in mean, it comes 0.8ha / worker
  val CULTIVATED_AREA_PER_WORKER: HA = 15
  val WORKERS_RATIO_PER_KITCHEN = 0.66 // Source: Robert and co

  val KITCHEN_MINIMUM_SIZE = 8
  val KITCHEN_MAXIMUM_SIZE = 31
  val SPLIT_KITCHEN_OFFSPRING_SIZE = 13
  //val KITCHEN_SIZE_THRESHOLD_FOR_ABSORPTION = KITCHEN_MAXIMUM_SIZE - SPLIT_KITCHEN_OFFSPRING_SIZE

  //SPACE // to adjust the generated area to a 180 ha
  val AREA_FACTOR = 180.0 / 500000 // (Mariana Odru, 2013 (177.1 ha))

  //FERTILITY

  val FERTILITY_BOOST_PER_MANURE_KG_PER_HA = 0.000075 // Converted from the data: "A boost of +15% with 2T per ha"

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
  val RAIN_FALL = rainFall


  //val MIL_SEED_FULL_POTENTIAL_YIELD: KG_BY_HA = Constants.MIL_FULL_POTENTIAL_YIELD * Constants.MIL_SEED_RATIO
  //val PEANUT_SEED_FULL_POTENTIAL_YIELD: KG_BY_HA = Constants.PEANUT_FULL_POTENTIAL_YIELD * Constants.PEANUT_SEED_RATIO

  //Herd
  //Assouma: the quantity ingered daily is equivalent to 2,5% of the cattle weight daily.
  // The quantity of grass is 65% of this quantity. For 1 year 250*0,025*365*0,65
  val KG_OF_STRAW_PER_COW_PER_YEAR: KG = 1480
  val KG_OF_MANURE_PER_COW_PER_YEAR: KG = 1140 //1250//1370 // Assouma between 40% and 55% of the ingested DM: 250 * 0.025 * 365 * 0,5

  // Fertility
  val SOIL_QUALITY_BASIS: SOIL_QUALITY_BY_HA = soilQualityBasis // soil quality by hectare
  val FALLOW_BOOST: SOIL_QUALITY_BY_HA = fallowBoost
  val EROSION: Double = erosion
  val MULCHING_BOOST: SOIL_QUALITY_BY_KG = 0.001 // FIXME: QS/KG ?
  val MANURE_BOOST_1_YEAR_AGO: SOIL_QUALITY_BY_KG = 0.00012 //FIXME: QS/KG ?
  val MANURE_BOOST_2_YEARS_AGO: SOIL_QUALITY_BY_KG = 0.00008 //FIXME: QS/KG ?
  val FAIDHERBIA_BOOST_PER_TREE = 0.06 // QS / TREE
  val MARIGOT_SURFACE_FOR_EXTRA_GRAZING = 67 * 0.7 // 67ha is the marigot surface described by Odru et al.This surface is available only during dry season: 47ha
  val MARIGOT_ANNUAL_FOOD: KG_BY_HA = 475 * MARIGOT_SURFACE_FOR_EXTRA_GRAZING // Grillot 2018 p.94 and Scriban p.15
  val HERD_SIZE_FEEDED_BY_MARIGOT_DURING_DRY_SEASON = (MARIGOT_ANNUAL_FOOD / KG_OF_STRAW_PER_COW_PER_YEAR).ceil.toInt // around 15
  println("AAANN " + HERD_SIZE_FEEDED_BY_MARIGOT_DURING_DRY_SEASON)
}