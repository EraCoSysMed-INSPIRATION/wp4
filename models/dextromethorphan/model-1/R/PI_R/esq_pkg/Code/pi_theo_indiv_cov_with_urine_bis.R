library(ospsuite)
# library(esqlabsR)
library(R6)
library(FME)
library(openxlsx)
library(dplyr)
library(parallel)
library(progress)
library(readr)
library(ggplot2)


file.sources = list.files(file.path(getwd(),"../R"), 
                          pattern="*.R$", full.names=TRUE, 
                          ignore.case=TRUE) 

for (f in file.sources) {
  source(f)
}


#####VARIABLE DEFINITION#####
# Path to the folder where the model file is located.
modelFolder <- file.path(getwd(), "../Models/Simulations")
# Path to the folder where experimental data files are located
dataFolder = file.path(getwd(), "../Data")
# Name of the excel file with experimental data
dataFile_obs = "theo_all_obs.xlsx"
dataFile_urine = "theo_all_urine.xlsx"
# Name of the csv file with covariate data
dataFileCov = "Kaumeier oral solution 185mg - pop - for parameter estimation-Population.csv"


####### create 1 csv/ID from population parameters #################

df <- read_lines_raw(file.path(dataFolder, dataFileCov))
for(i in 4:length(df)){
  ind <- df[c(1:3,i)]
  write_lines(ind, paste0("../Data/IndivParam_fromPop/ID ",i-4,".csv"))
}

###### create 1 sheet/ ID for obs ################

#plasma
data_obs <- read.xlsx(xlsxFile =  file.path(dataFolder, dataFile_obs), sheet = "Sheet1") %>% mutate_at(.vars = c(2,10,11), as.numeric)

ds_obs <-split(data_obs,data_obs$Patient.Id )


indiv_theo_obs <- createWorkbook()

for (i in 1:length(ds_obs)) { 
  addWorksheet(indiv_theo_obs, sheetName=paste0("ID ",names(ds_obs[i])))
  writeData(indiv_theo_obs, sheet=i, x=ds_obs[[i]]) # Note [[]]
} 

saveWorkbook(indiv_theo_obs, file.path(dataFolder,"indiv_obs.xlsx"), overwrite = TRUE)






##########Create PIConfiguration#############
piConfiguration <- PIConfiguration$new()
#If TRUE, the error is printed after each iteration. May be useful for assessing if the algorithm converges.
piConfiguration$printIterationFeedback <- FALSE

#########Define parameters to optimize#######
# 
parameterPaths <- c("Theophylline-CYP1A2-adjusted|Specific clearance",
                     "Theophylline|Intestinal permeability (transcellular)",
                      "CYP1A2|Reference concentration",
                     "Neighborhoods|Kidney_pls_Kidney_ur|Theophylline|Glomerular Filtration-GFR|GFR fraction")
# 




 # parameterPaths <- c("Neighborhoods|Kidney_pls_Kidney_ur|Theophylline|Glomerular Filtration-GFR|GFR fraction")


# getParameter("Theophylline-CYP1A2-adjusted|Specific clearance", simulations[[1]])

#  check rhobeg for bobyqa
# rhobeg_<-c()
# par_ <- c()
# for(t in seq_along(parameters)){
#   
#   rhobeg <- parameters[[t]]$maxValue- parameters[[t]]$minValue
#   rhobeg_ <- c(rhobeg_,rhobeg)
#   par <-  parameters[[t]]$startValue
#   par_ <- c(par_,par)
# }
# 
# default <- min(0.95, 0.2 * max(abs(par_)))
# constraint <- min(rhobeg_)/2


output <- matrix(ncol=5 + length(parameterPaths), nrow=0) %>% as.data.frame()
# for(i in names(indiv_theo)[-c(null_ind)]){
for(i in names(indiv_theo_plasma)[1:2]){
  
  #######LOAD SIMULATIONS and put them in a named list######
  simNames <- c("Kaumeier oral solution 185mg.pkml")
  simulations <- lapply(simNames, function(x){ospsuite::loadSimulation(file.path(modelFolder, x))})
  names(simulations) <- simNames
  
  
  parameters <- list()                 
  for (parameterPath in parameterPaths){
    modelParams <- list()
    for (simulation in simulations){
      modelParams <- c(modelParams, ospsuite::getParameter(path = parameterPath, container = simulation))
      #change initial values
      setParameterValues(parameters = modelParams, values = modelParams[[1]]$value*1.2)
    }
    
    piParameter <- PIParameters$new(parameters = modelParams)
    parameters <- c(parameters, piParameter)
  }
  
  
  
 
  
  
  # for(i in c("ID 0")){ 
  single_ind <- paste0("../Data/IndivParam_fromPop/",i,".csv") 
  myPopulation <- loadPopulation(csvPopulationFile = single_ind)
  id <- myPopulation$allIndividualIds
  param <- myPopulation$getParameterValuesForIndividual(id)
  
  
  sup <- which(param$paths == "CYP1A2|Reference concentration")
  
  setParameterValuesByPath(param$paths[-sup], param$values[-sup], simulations$`Kaumeier oral solution 185mg.pkml`)
 
  # !!! pop take over sim, "CYP1A2|Reference concentration" changed previously in sim returns to pop value 
  
 
  
  # getParameter("Theophylline-CYP1A2-adjusted|Specific clearance", simulations[[1]])
  # getParameter(path = "CYP1A2|Reference concentration", container = simulations$`Kaumeier oral solution 185mg.pkml`)
  
 
  
  # DataConfiguration is an object that describes how to read observed data from an excel file
  dataConfiguration_plasma <- DataConfiguration$new(dataFolder = dataFolder,
                                             dataFile = "indiv_obs.xlsx",
                                             compoundPropertiesFile = NULL,
                                             dataSheets = c(i)
  )
  
  observedData_plasma <- readOSPSTimeValues(dataConfiguration_plasma)

  
  dataConfiguration_urine <- DataConfiguration$new(dataFolder = dataFolder,
                                                    dataFile = "indiv_urine.xlsx",
                                                    compoundPropertiesFile = NULL,
                                                    dataSheets = c(i)
  )

  observedData_urine <- readOSPSTimeValues(dataConfiguration_urine)
  
  
  
  
  piOutputMappings <- list()
 
  piOutputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Theophylline|Plasma (Peripheral Venous Blood)",
                                                                container = simulations$`Kaumeier oral solution 185mg.pkml`))
  
  piOutputMapping$addObservedData(observedData_plasma[[i]]$plasma)
  piOutputMappings <- append(piOutputMappings, piOutputMapping)
  
  
  
  
  piOutputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|Kidney|Urine|Theophylline|Fraction excreted to urine",
                                                                container = simulations$`Kaumeier oral solution 185mg urine.pkml`))
  piOutputMapping$addObservedData(observedData_urine[[i]]$urine)
  piOutputMappings <- append(piOutputMappings, piOutputMapping)
  

  
  
  # file.sources = list.files(file.path(getwd(),"../R"), 
  #                           pattern="*.R$", full.names=TRUE, 
  #                           ignore.case=TRUE) 
  # 
  # for (f in file.sources) {
  #   source(f)
  # }
  # 
  
  print(getAllParametersMatching(parameterPaths, simulations[[1]]))

  pi <- ParameterIdentification$new(simulations = simulations, parameters = parameters, outputMappings = piOutputMappings,
                                    configuration = piConfiguration)
  start_time <- Sys.time()
  results <- pi$run()
  end_time <- Sys.time()
  duration <- round(difftime(end_time, start_time, units = "mins"))
  print(i)
  print(results$msg)
  # bobyqa
  # ind_res <- data.frame(t(c(i, results$ssr, results$feval, results$par, results$msg,  duration)))
  # marq
  # ind_res <- data.frame(t(c(i, results$ssr, results$iterations, results$par, results$message,  duration)))
  # Nelder-Mead
    ind_res <- data.frame(t(c(i, results$ssr, results$counts[[1]], results$par, results$message,  duration)))
  output <- rbind(output,ind_res)
  
}   
# View(pi)
# View(simulations)
# 
# simulations[[1]]$root$id
# simulations[[2]]$root$id


 # colnames(output) <- c("ID","ssr","feval", parameterPaths, "msg",  "duration (min)")
#NELDER-MEAD:
 colnames(output) <- c("ID","ssr","feval", parameterPaths,  "duration (min)")

 
write.xlsx(output, file.path(dataFolder,"/Results/individual/indiv_from_pop_U-P_GFR fraction_NM_init20_2ID.xlsx"), overwrite = TRUE)





write.xlsx(output, file.path("O:/Programming/outputs_pksim/indiv_from_pop_withUrine_Reference concentration_Marq_init20_all.xlsx"), overwrite = TRUE)







