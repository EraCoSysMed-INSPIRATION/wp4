create_human_individual <- function(population = unname(unlist(ospsuite::HumanPopulation)),
                                    gender = unname(unlist(ospsuite::Gender)),
                                    weight,
                                    age,
                                    height = NULL,
                                    gestational_age = 40,
                                    molecule_ontogenies = NULL, 
                                    weight_unit = c("kg", "g"),
                                    height_unit = c("m", "dm", "cm"),
                                    age_unit = c("year(s)", "day(s)"),
                                    gestational_age_unit = c("week(s)", "day(s)")) {
  
  population <- match.arg(population)
  gender <- match.arg(gender)
  weight_unit <- match.arg(weight_unit)
  height_unit <- match.arg(height_unit)
  age_unit <- match.arg(age_unit)
  gestational_age_unit <- match.arg(gestational_age_unit)
  
  chars <- createIndividualCharacteristics(species = "Human",
                                           population = population,
                                           gender = gender,
                                           weight = weight,
                                           height = height,
                                           age = age,
                                           gestationalAge = gestational_age,
                                           weightUnit  = weight_unit,
                                           heightUnit  = height_unit,
                                           ageUnit = age_unit,
                                           gestationalAgeUnit = gestational_age_unit,
                                           moleculeOntogenies = molecule_ontogenies)
  
  individual <- createIndividual(individualCharacteristics = chars)
  return(individual)
}