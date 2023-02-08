library(ospsuite)
library(saemix)
library(dplyr)
library(jsonlite)
library(tidyr)

source("../PI_R/esq_pkg/R/create_individuals.R")
source("SAEM_helper.R")

# Helper ----
# *******************************************************************
get_effect_list <- function(paths) {
  results <- c()
  names <- c()
  for (p in paths) {
    names <- c(names, p$path)
    results <- c(results, p$value)
  }
  names(results) <- names
  return(results)
}

# -- INPUT -----------------------------------------------------------------------------
data_dir <- file.path("../../data")
data_file <- file.path(data_dir, "DATASET_DEXTROMETHORPHAN_V05.csv")
individuals_file <- file.path(data_dir, "individuals_V01.json")
model_file <- file.path(data_dir, "model_dextromethorphan_individual.pkml")
processed_data_file <- "dextromethorphan_test.csv"

# -- PREPROCESS ------------------------------------------------------------------------

# Time in hours
# DV (CMT = 2) => Dextromethorphan - Peripheral Venous Blood-Plasma-Concentration in ng/ml
# DV (CMT = 3) => Dextrorphan - Peripheral Venous Blood-Plasma-Concentration in ng/ml
# DV (CMT = 4) => Dextrorphan-total - Peripheral Venous Blood-Plasma-Concentration in ng/ml
compound_info <- list(
  "Dextromethorphan" = list(
    "CMT" = 2,
    "MW" = 271.4
  ),
  "Dextrorphan" = list(
    "CMT" = 3,
    "MW" = 257.4
  ),
  "Dextrorphan-total" = list(
    "CMT" = 4,
    "MW" = 257.4
  )
)
# -> convert to µmol/l as in the simulation output
# ng/ml -> ug/l
# MW = 271.4 g/mol
# TIME in h -> min
observed_data <- read.table(data_file, sep = ",", header = TRUE) %>%
  #filter(ID %in% unique(ID)[1:15]) %>%
  mutate(SEX = if_else(SEX == 1, "MALE", "FEMALE")) %>%
  mutate(POPULATION = if_else(ETHN == 1, "WhiteAmerican_NHANES_1997",
                                         "BlackAmerican_NHANES_1997") ) %>%
  rename(INDIVIDUALID = 1) %>%
  filter(EVID == 0) %>% filter(CMT >= 2) %>%
  mutate(DV = ifelse(CMT == 2, DV / 271.4, DV / 257.4)) %>%
  mutate(TIME = TIME * 60) %>%
  mutate(ID = INDIVIDUALID)

write.table(observed_data, processed_data_file,
            row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE, fileEncoding = "UTF-8")


# -- SIMULATIONS ----------------------------------------------------------------------
individuals <- fromJSON(individuals_file)
simulations <- create_simulations(model_file, individuals, processed_data_file, simulation_resolution = 0.1)

# -- FIT DEFINITIONS ------------------------------------------------------------------
# kcat_paths = unnamed list of kcat paths to vary containing
# path = kcat_paths[[x]]$path
# prior = kcat_paths[[x]]$value

kcat_paths <- getAllParametersMatching("*|kcat", simulations[[1]])
output_paths <- list("Organism|PeripheralVenousBlood|Dextromethorphan|Plasma (Peripheral Venous Blood)" = 2,
                     "Organism|PeripheralVenousBlood|Dextrorphan|Plasma (Peripheral Venous Blood)" = 3,
                     "Organism|PeripheralVenousBlood|Dextrorphan|Dextrorphan-total Plasma (Peripheral Venous Blood)" = 4)

# add kcat paths to list of parameters to vary
mixed_effects <- get_effect_list(kcat_paths)

# SAEM
saemix_data <- saemixData(name.data       = observed_data,
                          name.group      = "ID",
                          name.predictors = c("TIME", "CMT", "ID"),
                          name.response   = "DV")

model <- function(psi, id, x) {

  unique_ids <- unique(x$ID)
  chunks <- length(unique(id)) / length(unique_ids)
  sims <- simulations[as.character(unique_ids)]

  model_output <- c()
  for (chunk in seq_len(chunks)) {

    psi_from_idx <- (chunk - 1) * length(sims) + 1
    psi_to_idx <- chunk * length(sims)

    sampled_parameters_raw <- psi[psi_from_idx:psi_to_idx,] %>%
      as.data.frame()
    colnames(sampled_parameters_raw) <- names(mixed_effects)
    parameters <- sampled_parameters_raw %>% mutate(ID = names(sims))

    set_simulation_parameters(sims, parameters)
    results <- run_simulations(sims, ncores = 16) %>%
      fetch_simulation_results(names(output_paths))

    results_long <- pivot_longer(results, names(output_paths)) %>%
      mutate(CMT = sapply(name, function(x) output_paths[[x]])) %>%
      mutate(DV = value) %>%
      select(ID, TIME, CMT, DV) %>%
      mutate(ID = as.numeric(ID))


    compartments <- unique(results_long$CMT)
    results_long_approx <- data.frame()
    for (comp in compartments) {
      for (id in unique_ids) {
        obs_tmp <- observed_data %>%
          filter(INDIVIDUALID == id) %>%
          filter(CMT == comp)

        sim_tmp <- results_long %>%
          filter(ID == id) %>%
          filter(CMT == comp)

        print(sim_tmp)

        interpolated <- approx(sim_tmp$TIME, sim_tmp$DV, obs_tmp$TIME)$y
        tmp_df <- data.frame(ID = id,
                             CMT = comp,
                             TIME = obs_tmp$TIME,
                             DV = interpolated)

        results_long_approx <- rbind(results_long_approx, tmp_df)
      }
    }




    for (id in unique_ids) {
      obs_tmp <- observed_data %>% filter(ID == id)
      sim_tmp <- results %>% filter(ID == id)
      interpolated <- approx(sim_tmp$TIME, sim_tmp[[output_path]], obs_tmp$TIME)$y
      model_output <- c(model_output, interpolated)
    }

  }

  return(model_output)
}


saemix_model <- saemixModel(model = model,
                            psi0  = mixed_effects,
                            error.model = "combined",
                            transform.par = rep(1, length(mixed_effects)))


saemix_options <- list(map = TRUE, fim = TRUE, ll.is = FALSE,
                       displayProgress = TRUE, seed = 123,
                       nbiter.saemix = c(50, 200),
                       ll.gq = TRUE,
                       ll.is = FALSE,
                       nb.sim = 5,
                       nb.simpred = 10,
                       ipar.lmcmc = 5,
                       nbdisplay = 3,
                       save = TRUE, save.graphs = TRUE)

saemix.fit    <- saemix(saemix_model, saemix_data, saemix_options)
