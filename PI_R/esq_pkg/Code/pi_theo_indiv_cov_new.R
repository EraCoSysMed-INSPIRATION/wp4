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
dataFile = "theo_all.xlsx"
# Name of the csv file with covariate data
dataFileCov = "Kaumeier oral solution 185mg - pop - for parameter estimation-Population.csv"


####### create 1 csv/ID from population parameters #################

df <- read_lines_raw(file.path(dataFolder, dataFileCov))
for(i in 4:length(df)){
  ind <- df[c(1:3,i)]
  write_lines(ind, paste0("../Data/IndivParam_fromPop/ID ",i-4,".csv"))
}

###### create 1 sheet/ ID for obs ################


data <- read.xlsx(xlsxFile =  file.path(dataFolder, dataFile), sheet = "Sheet1") %>% mutate_if(is.character, as.numeric)

ds <-split(data,data$Group.Id )


indiv_theo <- createWorkbook()

for (i in 1:length(ds)) { 
  addWorksheet(indiv_theo, sheetName=paste0("ID ",names(ds[i])))
  writeData(indiv_theo, sheet=i, x=ds[[i]]) # Note [[]]
} 

saveWorkbook(indiv_theo, file.path(dataFolder,"indiv.xlsx"), overwrite = TRUE)




#######LOAD SIMULATIONS and put them in a named list######
simNames <- c("Kaumeier oral solution 185mg.pkml")
simulations <- lapply(simNames, function(x){ospsuite::loadSimulation(file.path(modelFolder, x))})
names(simulations) <- simNames



##########Create PIConfiguration#############
piConfiguration <- PIConfiguration$new()
#If TRUE, the error is printed after each iteration. May be useful for assessing if the algorithm converges.
piConfiguration$printIterationFeedback <- FALSE

#########Define parameters to optimize#######
parameters <- list()
# parameterPaths <- c("Theophylline-CYP1A2-adjusted|Specific clearance",
#                      "Theophylline|Intestinal permeability (transcellular)",
#                       "CYP1A2|Reference concentration",
#                      "Neighborhoods|Kidney_pls_Kidney_ur|Theophylline|Glomerular Filtration-GFR|GFR fraction")





parameterPaths <- c("CYP1A2|Reference concentration")
                  
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

i <- "ID 0"
output <- matrix(ncol=5 + length(parameterPaths), nrow=0) %>% as.data.frame()
# for(i in names(indiv_theo)[-c(null_ind)]){
for(i in names(indiv_theo)){
  
  single_ind <- paste0("../Data/IndivParam_fromPop/",i,".csv") 
  myPopulation <- loadPopulation(csvPopulationFile = single_ind)
  id <- myPopulation$allIndividualIds
  param <- myPopulation$getParameterValuesForIndividual(id)
  
  
  sup <- which(param$paths == "CYP1A2|Reference concentration")
  
  setParameterValuesByPath(param$paths[-sup], param$values[-sup], simulations$`Kaumeier oral solution 185mg.pkml`)
 
  # achtung!!! pop take over sim, "CYP1A2|Reference concentration" changed previously in sim return to pop value 
  
  
 
  getParameter(path = "CYP1A2|Reference concentration", container = simulations$`Kaumeier oral solution 185mg.pkml`)
  
  #########Define otput mappings#######
  piOutputMappings <- list()
  
  
  
  #Create a PIOutputMapping setting the quantity of a model
  piOutputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Theophylline|Plasma (Peripheral Venous Blood)",
                                                                container = simulations$`Kaumeier oral solution 185mg.pkml`))
  
  # DataConfiguration is an object that describes how to read observed data from an excel file
  dataConfiguration <- DataConfiguration$new(dataFolder = dataFolder,
                                             dataFile = "indiv.xlsx",
                                             compoundPropertiesFile = NULL,
                                             dataSheets = c(i)
  )
  
  # observedData <- esqlabsR::readOSPSTimeValues(dataConfiguration)
  
  
  observedData <- readOSPSTimeValues(dataConfiguration)
  
  
  
  #Add observed data. Multiple data can be added to the same mapping
  piOutputMapping$addObservedData(observedData[[i]][[1]])
  
  
  #Add the mapping to the list of all mappings
  piOutputMappings <- append(piOutputMappings, piOutputMapping)
  pi <- ParameterIdentification$new(simulations = simulations$`Kaumeier oral solution 185mg.pkml`, parameters = parameters, outputMappings = piOutputMappings[[1]],
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


# colnames(output) <- c("ID","ssr","feval", parameterPaths, "msg",  "duration (min)")
#NELDER-MEAD:
colnames(output) <- c("ID","ssr","feval", parameterPaths,  "duration (min)")

write.xlsx(output, file.path(dataFolder,"/Results/individual/ind_from_pop/Nelder/indiv_from_pop_Nelder_init20_Specific clearance.xlsx"), overwrite = TRUE)
write.xlsx(output, file.path("O:/Programming/indiv_from_pop_Nelder_init20_Specific clearance.xlsx"), overwrite = TRUE)







