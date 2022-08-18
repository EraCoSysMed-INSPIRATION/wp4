library(ospsuite)
library(R6)
library(FME)
library(openxlsx)
library(dplyr)
library(parallel)
library(progress)
library(readr)
library(ggplot2)
library(tidyr)

#############  load functions ####################

source(file.path(getwd(),"PKSim_PI_fun.R"))

############ VARIABLE DEFINITION ##################

# Path to the folder where the model file is located.
modelFolder <- file.path(getwd(), "../models")
# Path to the folder where experimental data files are located
dataFolder = file.path(getwd(), "../data")
# Matrix
data_structure <- c(ID = 1, TIME = 2, Values = 3, matrix = 4 )

# Name of the pkml model file(s)
simNames <- c("Kaumeier oral solution 185mg.pkml")
# Name of the excel file with experimental data
# dataFile_Obs = "finalDataset Theophylline 100subj.csv"
dataFile_Obs = "obs_Theophylline 100subj_reformat.csv"
# Name of the csv file with covariate data
dataFileCov = "Kaumeier oral solution 185mg - pop - for parameter estimation-Population.csv"

# Define parameters to optimize with initial values
parameterPaths <- c("Theophylline-CYP1A2-adjusted|Specific clearance" = 8.43e-3*1.2,
                       "Theophylline|Intestinal permeability (transcellular)" = 8.76e-7*1.2,
                        "CYP1A2|Reference concentration" = 1.8*1.2,
                        "Neighborhoods|Kidney_pls_Kidney_ur|Theophylline|Glomerular Filtration-GFR|GFR fraction" = 0.15*1.2
                       )

bounds_factor <- 10



  
#  Define outputs path (same name as in Obs$matrix)
outputPaths <- c(Plasma = "Organism|PeripheralVenousBlood|Theophylline|Plasma (Peripheral Venous Blood)",
                 Urine = "Organism|Kidney|Urine|Theophylline|Fraction excreted to urine")

plots <- T

#### path of all outputs:
# sim <- ospsuite::loadSimulation(file.path(modelFolder, simNames))
# sim[["outputSelections"]][["allOutputs"]]
######################## observed data ###################

Observed_Data<- read.csv(file.path(dataFolder, dataFile_Obs)) %>% select(all_of(data_structure)) 
  

Observed_Data$Values <- as.numeric(Observed_Data$Values)
Observed_Data$TIME <- as.numeric(Observed_Data$TIME)
Observed_Data$ID <- as.numeric(Observed_Data$ID)


splitID <- split(Observed_Data, Observed_Data$ID)
Obs <- lapply(splitID, function(x) split(x, x$matrix))

Obs[1:2]

#######################  indiv param & covariates  ####################################

pop <- loadPopulation(file.path(dataFolder, dataFileCov))
# pop$remove("CYP1A2|Reference concentration")


##################### run parameters estimation ##############################
results_fit <- list()
start_time <- Sys.time()
for(i in names(Obs)){
  st <- Sys.time()
 print(i)
# i <-  names(Obs)[1]
Obs_ind <- Obs[[i]][order(names(Obs[[i]]), decreasing = F)]
sim <- sim_update(ID = i)

 
# ObjFun(simulation = sim, Obs = Obs_ind, ID = i)



 fit <- FME::modFit(f = iterate, p = as.vector(parameterPaths) , lower = as.vector(parameterPaths)/bounds_factor, upper = bounds_factor*as.vector(parameterPaths), method = "Nelder-Mead")
# fit <- nloptr::neldermead(fn = iterate, x0 = as.vector(parameterPaths) , lower = as.vector(parameterPaths)/bounds_factor, upper = bounds_factor*as.vector(parameterPaths))


graphics.off()
results_fit[[i]] <- fit
et <- Sys.time()
print(round(difftime(et, st, units = "mins")))
}
end_time <- Sys.time()
duration <- round(difftime(end_time, start_time, units = "mins"))



saveRDS(results_fit, file = "FME-results_NM.rds")
saveRDS(duration, file = "FME-results_NM_duration.rds")




##############  ModCost  ######################

fit_old <- read_rds(file.path(getwd(),"nloptr-results_NM.rds"))
fit_old[[1]][1]


