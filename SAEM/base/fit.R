# *******************************************************************
# Project: Dextromethorphan SAEM Fit
# Script purpose: Fit the Propofol model to the observed data
# Date: 03-04-2024
# Author: Dominik Selzer (dominik.selzer@uni-saarland.de)
# *******************************************************************

library(ospsuite)
library(saemix)
library(dplyr)
library(tidyr)
library(purrr)
library(jsonlite)
library(parallel)
library(tictoc)

source("create_simulations.R")
source("create_individuals.R")
source("create_human_individuals.R")
source("saem_helper.R")

# Settings ----
# *******************************************************************
cpu_cores <- 120
seed <- 123
k1k2 <- c(20, 150)

project_name <- "Dextromethorphan"
model_file <- "data/model_dextromethorphan_individual.pkml"
individuals_file <- "data/individuals_V01.json"

# Time in hours / conc in ng/ml = ug/l
obs_file <- "data/DATASET_DEXTROMETHORPHAN_V05.csv"
compounds <- list(
  compound("Dextromethorphan", 2, 271.4, "Organism|PeripheralVenousBlood|Dextromethorphan|Plasma (Peripheral Venous Blood)")
  #compound("Dextrorphan", 4, 257.4, "Organism|PeripheralVenousBlood|Dextrorphan|Plasma (Peripheral Venous Blood)"),
  #compound("Dextrorphan-total", 3, 257.4, "Organism|PeripheralVenousBlood|Dextrorphan|Dextrorphan-total Plasma (Peripheral Venous Blood)")
)
id_filter <- NULL

# Preprocess observed data ----
# *******************************************************************
comps <- compounds |> map_vec(pluck("cmt"))
observed_data <- read.table(obs_file, sep = ",", header = TRUE) |>
  mutate(SEX = if_else(SEX == 1, "MALE", "FEMALE")) |>
  mutate(POPULATION = if_else(ETHN == 1, "WhiteAmerican_NHANES_1997",
                              "BlackAmerican_NHANES_1997")) |>
  rename(INDIVIDUALID = 1) |>
  filter(CMT %in% comps) |>
  filter(EVID == 0) |>
  mutate(ID = INDIVIDUALID) |>
  arrange(ID, CMT, TIME) |>
  map_cmt(compounds) |>
  correct_dv_units(compounds) |>
  mutate(TIME = TIME * 60) # base unit in PK-Sim is minutes from h

# Filter step ----
# *******************************************************************
observed_data <- filter_observed(observed_data, id_filter)

# Create Individuals ----
# *******************************************************************
individuals <- list()
for (id in unique(observed_data$ID)) {
  tmp <- observed_data |>
    filter(ID == id) |>
    slice(1)

  individuals[[as.character(id)]] <- create_human_individual(
    population = tmp$POPULATION,
    weight = as.numeric(tmp$WEIGHT),
    gender = tmp$SEX,
    height = as.numeric(tmp$HEIGHT),
    age = as.numeric(tmp$AGE),
    height_unit = "cm"
  )
}

# Create Simulations ----
# *******************************************************************
tic("Create Simulations")
simulations <- create_simulations(
  pkml_file = model_file,
  individuals,
  simulation_resolution = NULL
) |>
  set_names(names(individuals)) |>
  patch_sim_times(observed_data) |>
  set_sim_outputs(compounds)
toc()


# Setup model ----
# *******************************************************************
parameter_path <- getAllParametersMatching("*|kcat", simulations[[1]])[c(2)]
parameter <- model_parameters(simulations, parameter_path)
sim_batches <- create_sim_batches(simulations, parameter_path)

saemix_data <- create_saem_data(observed_data)
saemix_model <- saem_model(parameter, sim_batches, cores = cpu_cores)

# Fit ----
# *******************************************************************
saemix_res <- saem_fit(project_name, saemix_model, saemix_data, seed, k1k2)
