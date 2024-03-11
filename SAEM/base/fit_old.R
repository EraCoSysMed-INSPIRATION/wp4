library(ospsuite)
library(saemix)
library(dplyr)
library(tidyr)
library(purrr)
library(jsonlite)
library(parallel) 
cores <-  detectCores() - 1

source("create_simulations.r")
source("create_individuals.R")
source("create_human_individuals.R")


# if # ID < 50 duplicate
dup <- 1
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



observed_data <- read.table("DATASET_DEXTROMETHORPHAN_V05.csv", sep = ",", header = TRUE) %>%
  #filter(ID %in% unique(ID)[1:15]) %>%
  mutate(SEX = if_else(SEX == 1, "MALE", "FEMALE")) %>%
  mutate(POPULATION = if_else(ETHN == 1, "WhiteAmerican_NHANES_1997",
                              "BlackAmerican_NHANES_1997") ) %>%
  rename(INDIVIDUALID = 1) %>%
  filter(EVID == 0) %>% filter(CMT >= 2) %>%
  mutate(DV = ifelse(CMT == 2, DV / 271.4, DV / 257.4)) %>%
  mutate(TIME = TIME * 60) %>%
  mutate(ID = INDIVIDUALID) %>% 
  arrange(ID, CMT, TIME)

  # observed_data <-  
  #           map(seq_len(dup),~observed_data) %>% 
  #           bind_rows(.id="rep") %>% 
  #   mutate(SIM = cumsum(c(1,diff(ID)!=0)))
  

########### time points by ID & CMT   #############
observed_data_CMT <- observed_data
observed_data_CMT["CMT"][observed_data_CMT["CMT"] == 2] <- "Organism|PeripheralVenousBlood|Dextromethorphan|Plasma (Peripheral Venous Blood)"
observed_data_CMT["CMT"][observed_data_CMT["CMT"] == 3] <-   "Organism|PeripheralVenousBlood|Dextrorphan|Plasma (Peripheral Venous Blood)"
observed_data_CMT["CMT"][observed_data_CMT["CMT"] == 4] <- "Organism|PeripheralVenousBlood|Dextrorphan|Dextrorphan-total Plasma (Peripheral Venous Blood)"

time_points <- split(observed_data_CMT[, c("TIME", "CMT")], observed_data_CMT$ID) %>% 
lapply(., function(x) as.vector(split(x$TIME, x$CMT)))
time_points <- rep(time_points, dup)
names(time_points) <- seq_along(time_points)



observed_data_ALL <- observed_data

observed_data <- observed_data[observed_data$CMT==2,]
###############################################################
  
  

# -- SIMULATIONS ----------------------------------------------------------------------
individuals <- fromJSON("individuals_V01.json")
simulations <- create_simulations(pkml_file= "model_dextromethorphan_individual.pkml",
                                  individuals,
                                  weight_unit = "kg",
                                  height_unit = "cm",
                                  age_unit = "year(s)",
                                  simulation_resolution = NULL) #0.1

names(simulations) <- names(individuals)



# check TIME *60 already done in observed data (base unit = min in PKSim)???
for (i in names(simulations)){
  simulations[[i]]$outputSchema$clear()
  myTimes <- sort(unique(observed_data$TIME[observed_data$INDIVIDUALID== as.numeric(i)]))
  # myTimes <- myTimes*60 
  simulations[[i]]$outputSchema$addTimePoints(myTimes)
}

# -- FIT DEFINITIONS ------------------------------------------------------------------

kcat_paths <- getAllParametersMatching("*|kcat", simulations[[1]])

# ! order output_paths by CMT
output_paths <- c("Organism|PeripheralVenousBlood|Dextromethorphan|Plasma (Peripheral Venous Blood)",
                  "Organism|PeripheralVenousBlood|Dextrorphan|Plasma (Peripheral Venous Blood)",
                  "Organism|PeripheralVenousBlood|Dextrorphan|Dextrorphan-total Plasma (Peripheral Venous Blood)")

# add kcat paths to list of parameters to vary
# mixed_effects <- list("Dextromethorphan-CYP2D6-Lutz 2012 (dextrorphan)|kcat" = 90)
mixed_effects <-  unlist(lapply(kcat_paths, function(x) x[["value"]]))
names(mixed_effects) <- unlist(lapply(kcat_paths, function(x) x[["path"]]))

batches <- list()
for (s in simulations) {
  simBatch <- createSimulationBatch(simulation = s, parametersOrPaths = kcat_paths)
  batches <- c(batches, simBatch)
}


# SAEM
saemix_data <- saemixData(name.data = observed_data, header = TRUE,
                         sep = ",", na = NA, name.group = c("ID"),
                         name.predictors = c("CMT", "TIME", "ID"), name.response = c("DV"),
                         #name.covariates = c("Weight", "Sex"),
                         units = list(x = "hr", y = "µmol/l")
                         )

opts <- SimulationRunOptions$new(
  numberOfCores = 66,
  checkForNegativeValues = FALSE,
  showProgress = FALSE)

model <- function(psi, id, x) {
  
  # upate all parameters for all simimulations
  for(i in seq_along(batches)){
    batches[[i]]$addRunValues(parameterValues = psi[i,])
  }
 
  out <- runSimulationBatches(batches, simulationRunOptions = opts)
  out <- lapply(out, function(x) x[[1]])

  pred <- NULL
  for(i in seq_along(simulations)){
    resultsData <- (getOutputValues(out[[i]], quantitiesOrPaths = output_paths))$data
    predictions <- lapply(output_paths, function(x) resultsData[round(resultsData$Time,3) %in% round(time_points[[i]][[x]],3), x]) %>% unlist()
    pred<- c(pred,predictions)
  }
  
run <<- run + 1
print(run)
return(pred)
}


saemix_model <- saemixModel(model = model,
                            psi0 = mixed_effects,
                            transform.par = rep(1, length(mixed_effects)),
                            error.model="combined"
                            )


saemix_options <- list(map = TRUE, fim = TRUE, ll.is = FALSE,
                       displayProgress = TRUE, seed = 123,
                       nbiter.saemix = c(300, 100),
                       ll.gq = TRUE,
                       ll.is = FALSE,
                       nb.sim = 5,
                       nb.simpred = 10,
                       ipar.lmcmc = 5,
                       nbdisplay = 3,
                       save = TRUE, 
                       save.graphs = TRUE)


start <- Sys.time()
# dev.new()
run <<- 0
saemix.fit    <- saemix(saemix_model, saemix_data, saemix_options)
end <- Sys.time()
duration <- difftime(end, start, units = "hours")

saveRDS(saemix.fit, file = "output_saem-dextro.rds")
saveRDS(duration, file = "duration.rds")
