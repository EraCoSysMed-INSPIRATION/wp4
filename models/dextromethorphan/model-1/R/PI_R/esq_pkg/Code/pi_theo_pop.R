library(ospsuite)
# library(esqlabsR)
library(R6)
library(FME)
library(openxlsx)
library(dplyr)
library(parallel)
library(progress)




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
dataFile = "theo_all_rich.xlsx"





# DataConfiguration is an object that describes how to read observed data from an excel file
  dataConfiguration <- DataConfiguration$new(dataFolder = dataFolder,
                                             dataFile = dataFile,
                                             compoundPropertiesFile = NULL,
                                             dataSheets = c(
                                               "Sheet1"
                                             ))
  # observedData <- esqlabsR::readOSPSTimeValues(dataConfiguration)
  observedData <- readOSPSTimeValues(dataConfiguration)
  
  #######LOAD SIMULATIONS and put them in a named list######
  simNames <- c("Kaumeier oral solution 185mg.pkml")
  simulations <- lapply(simNames, function(x){ospsuite::loadSimulation(file.path(modelFolder, x))})
  names(simulations) <- simNames
  
  # initial parameters values
  simulations_init <- lapply(simNames, function(x){ospsuite::loadSimulation(file.path(modelFolder, x))})
  names(simulations_init) <- simNames
  
  
  ##########Create PIConfiguration#############
  piConfiguration <- PIConfiguration$new()
  #If TRUE, the error is printed after each iteration. May be useful for assessing if the algorithm converges.
  piConfiguration$printIterationFeedback <- FALSE
  
  #########Define parameters to optimize#######
  parameters <- list()
  # parameterPaths <- c("Theophylline-CYP1A2-adjusted|Specific clearance",
  #                       "Theophylline|Intestinal permeability (transcellular)",
  #                      "CYP1A2|Reference concentration",
  #                      "Neighborhoods|Kidney_pls_Kidney_ur|Theophylline|Glomerular Filtration-GFR|GFR fraction")
  # 
  # 
  #                    

  parameterPaths <- c("Neighborhoods|Kidney_pls_Kidney_ur|Theophylline|Glomerular Filtration-GFR|GFR fraction")
                    
  

  

  
  #########Define otput mappings#######
  piOutputMappings <- list()

  #Create a PIOutputMapping setting the quantity of a model
  piOutputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Theophylline|Plasma (Peripheral Venous Blood)",
                                                                container = simulations$`Kaumeier oral solution 185mg.pkml`))
  
 
             
  #Add observed data. Multiple data can be added to the same mapping
  piOutputMapping$addObservedData(observedData$Sheet1$`0`)
  #Add the mapping to the list of all mappings
  piOutputMappings <- append(piOutputMappings, piOutputMapping)
  
 
  #Create new parameter identification. This PI would optimize all three simulations.
  # pi <- ParameterIdentification$new(simulations = simulations, parameters = parameters, outputMappings = piOutputMappings,
  #                                   configuration = piConfiguration)
  # #Plot results before optimization
  # pi$plotCurrentResults()
  
  #FOR PERFORMANCE REASONS, OPTIMIZE WITH ONE SIMULATION ONLY.
  #IF YOU WANT TO USE ALL THREE SIMULATIONS, USE THE `parameters` LIST
  #CREATED ABOVE AND PASS ALL THREE SIMULATIONS AND MAPPINGS
  # parameters1 <- lapply(parameterPaths, function(x){
  #   modelParams <- getParameter(path = x, container = simulations$`Kaumeier oral solution 185mg.pkml`)
  #   piParameters <- PIParameters$new(parameters = modelParams)
  # })
 
  parameters_init <- list()
  for (parameterPath in parameterPaths){
    modelParams_init <- list()
    for (simulation in simulations_init){
      modelParams_init <- c(modelParams_init, ospsuite::getParameter(path = parameterPath, container = simulation))
    }
    piParameter_init <- PIParameters$new(parameters = modelParams_init)
    parameters_init <- c(parameters_init, piParameter_init)
  
  }
  
  
  
  output <- matrix(ncol=5 + length(parameterPaths), nrow=0) %>% as.data.frame()
  
  for(i in  seq(0.8, 1.2, 0.02)){
  
  parameters <- list()
  
  for (parameterPath in parameterPaths){
    modelParams <- list()
    for (simulation in simulations){
      modelParams <- c(modelParams, ospsuite::getParameter(path = parameterPath, container = simulation))
      #change initial values
      setParameterValues(parameters = modelParams, values = parameters_init[[which(parameterPaths==parameterPath)]]$startValue*i)
    }
    piParameter <- PIParameters$new(parameters = modelParams)
    parameters <- c(parameters, piParameter)
  }
  
  
  # # # check contol for bobyqa:
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
  # min(0.95, 0.2 * max(abs(par_)))
  # min(rhobeg_)/2
  # #############################
  
  
  
  

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
   ind_res <- data.frame(t(c(i, results$ssr, results$iterations, results$par, results$message,  duration)))
  # Nelder-Mead
  # ind_res <- data.frame(t(c(i, results$ssr, results$counts[[1]], results$par, results$message,  duration)))
  output <- rbind(output,ind_res)
  
  }
     colnames(output) <- c("var","ssr","feval", parameterPaths, "msg", "duration (min)")
   # Nelder-Mead:
   # colnames(output) <- c("var","ssr","feval", parameterPaths,  "duration (min)")
   
   write.xlsx(output, file.path(dataFolder,"Results/pop_results_cov_Marq_init20_GFR fraction_rich.xlsx"), overwrite = TRUE)
   write.xlsx(output, file.path("O:/Programming/pop_results_cov_Nelder-Mead _init20.xlsx"), overwrite = TRUE)
  
  
  
  

 
 # getAllMoleculesMatching("Organism|VenousBlood|**", simulations[[1]] )
 
   # getAllParametersMatching("Neighborhoods|**|Theophylline|Glomerular Filtration-GFR|**",simulations[[1]])

 