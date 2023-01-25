library(ospsuite)
library(dplyr)
library(jsonlite)
source("models/dextromethorphan/model-2/R/PI_R/esq_pkg/R/create_individuals.R")

# -- INPUT -----------------------------------------------------------------------------
data_dir <- file.path("models", "dextromethorphan", "model-2", "data")
data_file <- file.path(data_dir, "DATASET_DEXTROMETHORPHAN_V05.csv")

# -- PREPROCESS ------------------------------------------------------------------------

individual_data <- read.table(data_file, sep = ",", header = TRUE) %>%
  mutate(SEX = if_else(SEX == 1, "MALE", "FEMALE")) %>%
  mutate(POPULATION = if_else(ETHN == 1, "WhiteAmerican_NHANES_1997",
                                         "BlackAmerican_NHANES_1997") ) %>%
  rename(INDIVIDUALID = 1) %>%
  filter(EVID == 1)

individuals <- list()
for (i in seq_len(nrow(individual_data))) {
    row <- individual_data[i, ]
    individual <- create_human_individual(
        population = row$POPULATION,
        gender = row$SEX,
        weight = as.numeric(row$WEIGHT),
        height = as.numeric(row$HEIGHT) / 100,
        age = as.numeric(row$AGE)
    )
    individuals[[as.character(row$INDIVIDUALID)]] <- individual
}
write_json(individuals, file.path(data_dir, "individuals_V01.json"), auto_unbox = TRUE, pretty = TRUE)