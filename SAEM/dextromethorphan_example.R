library(ospsuite)
library(saemix)
library(dplyr)

source("../PI_R/esq_pkg/R/create_individuals.R")

#-------------------------------------------
data_file <- "../data/dextromethorphan/DATASET_DEXTROMETHORPHAN_V02.csv"
out_data_file <- "dextromethorphan_test.csv"
#-------------------------------------------

population_characteristics <- read.table(data_file, sep = ";", header = TRUE) %>%
  select(ID, AGE, WEIGHT, HEIGHT) %>%
  mutate(SEX = "MALE") %>%
  mutate(POPULATION = "European_ICRP_2002") %>%
  rename(INDIVIDUALID = 1)

write.table(population_characteristics, out_data_file,
          row.names = FALSE,
          col.names = TRUE,
          sep = ",",
          quote = FALSE,
          fileEncoding = "UTF-8")


subjects <- create_human_individuals(out_data_file,
                                     weight_unit = "kg",
                                     height_unit = "cm",
                                     age_unit = "year(s)",
                                     progress = FALSE)
