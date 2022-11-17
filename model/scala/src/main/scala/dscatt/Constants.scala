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
  val INITIAL_FERTILITY_PER_PARCEL = 1000
  // Between 12% and 20%, sources: https://www.researchgate.net/publication/248850078_Influence_d%27apports_de_matiere_organique_sur_la_culture_de_mil_et_d%27arachide_sur_un_sol_sableux_du_Nord-Senegal_I_-_Bilans_de_consommation_production_et_developpement_racinaire
  // Effects are very close for mil and peanut
  val MANURE_EFFECT_ON_YIELD = 0.15
  val FALLOW_EFFECT_ON_YIELD = 0.1
  val FERTILIZER_EFFECT_ON_MIL_YIELD = 0.2
  val FERTILIZER_EFFECT_ON_PEANUT_YIELD = 0.8
}