library(ospsuite)
library(saemix)
library(dplyr)

source("models/dextromethorphan/model-2/R/PI_R/esq_pkg/R/create_individuals.R")
source("models/dextromethorphan/model-2/R/SAEM/SAEM_helper.R")

# -- INPUT -----------------------------------------------------------------------------
data_dir <- file.path("models", "dextromethorphan", "model-2", "data")
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
  filter(EVID == 0) %>% filter(CMT == 2) %>%
  mutate(DV = DV / 271.4) %>%
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
output_paths <- list("Organism|PeripheralVenousBlood|Dextromethorphan|Plasma (Peripheral Venous Blood)",
                     "Organism|PeripheralVenousBlood|Dextrorphan|Plasma (Peripheral Venous Blood)",
                     "Organism|PeripheralVenousBlood|Dextrorphan|Dextrorphan-total Plasma (Peripheral Venous Blood)")

# add kcat paths to list of parameters to vary
mixed_effects <- list("Dextromethorphan-CYP2D6-Lutz 2012 (dextrorphan)|kcat" = 90)


# SAEM
saemix_data <- saemixData(name.data       = observed_data,
                          name.group      = "ID",
                          name.predictors = c("TIME", "ID"),
                          name.response   = "DV")


model <- function(psi, id, x) {

  unique_ids <- unique(x$ID)
  chunks <- length(unique(id)) / length(unique_ids)

  sims <- simulations[as.character(unique_ids)]
  model_output <- c()
  for (chunk in seq_len(chunks)) {
    print(chunk)

    psi_from_idx <- (chunk - 1) * length(sims) + 1
    psi_to_idx <- chunk * length(sims)
    kcats <- psi[psi_from_idx:psi_to_idx, 1]
    print(kcats)
    parameters <- data.frame(ID = names(sims),
                             "Dextromethorphan-CYP2D6-Lutz 2012 (dextrorphan)|kcat" = kcats,
                             check.names = FALSE)
    set_simulation_parameters(sims, parameters)

    results <- run_simulations(sims, ncores = 16) %>%
      fetch_simulation_results(output_path)


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
                            psi0  = c(kcat = 200),
                            error.model="combined",
                            transform.par = c(1))


saemix_options <- list(map = TRUE, fim = TRUE, ll.is = FALSE,
                       displayProgress = TRUE, seed = 123,
                       nbiter.saemix = c(30, 30),
                       ll.gq = TRUE,
                       ll.is = FALSE,
                       nb.sim = 5,
                       nb.simpred = 10,
                       ipar.lmcmc = 5,
                       nbdisplay = 3,
                       save = TRUE, save.graphs = TRUE)

saemix.fit    <- saemix(saemix_model, saemix_data, saemix_options)
