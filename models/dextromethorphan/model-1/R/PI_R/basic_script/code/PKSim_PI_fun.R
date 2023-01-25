

##################  Upadate simulation timepoints & ID param-cov ##################### 


sim_update <- function(ID){
  
  # sim only at obs timepoints
  Obs_ind <- Obs[ID] 
  time <- lapply(names(outputPaths), function(x) (Obs_ind[[ID]][[x]]$TIME)) %>% unlist() %>% unique()
  simulation <- ospsuite::loadSimulation(file.path(modelFolder, simNames))
  simulation$outputSchema$clear()
  simulation$outputSchema$addTimePoints(timePoints =  time)
  
  # upadate parameters from csv file
  param <- pop$getParameterValuesForIndividual(ID)
  setParameterValuesByPath(param$paths, param$values, simulation)
  
  return(simulation)
}

############ Function to output predictions at observed time points, given a set of parameters ################
iterate <- function(currVals){
  
  for (i in seq_along(parameterPaths)){
    modelParams <- list()
    modelParams <- c(modelParams, getParameter(names(parameterPaths)[i], sim))
    setParameterValues(modelParams,currVals[[i]])
  }
  
  
  # print(getAllParametersMatching(names(parameterPaths), sim))
  ObjFun()
}



##############  function to output predictions at observed time points, given a set of parameters, and model ###################
pred <- function(sim, ID){
 
  simulationResults <- ospsuite::runSimulation(sim)
  resultsData <- lapply(outputPaths,function(x){getOutputValues(simulationResults, quantitiesOrPaths =  x)$data})
  
  # resultsData_f <- lapply(names(outputPaths), function(x) {(filter(resultsData[[x]], Time %in% Obs[[ID]][[x]]$TIME))} %>% select(outputPaths[x])) 

  # list(as.vector(unlist(resultsData_f)))
  return(resultsData)
}

################# define Objective Function - SSR #######################
ObjFun <- function(){

  pred_curr<- pred(sim, ID = i)
  #keep only timepoints in obs data for each outputs
  pred_curr_f <- sapply(names(outputPaths), function(x) {(filter(pred_curr[[x]], Time %in% Obs_ind[[x]]$TIME))}, simplify = FALSE,USE.NAMES = TRUE)
  pred_curr_f <-pred_curr_f[order(names(pred_curr_f), decreasing = F)]
  
  p <- as.vector(unlist(lapply(pred_curr_f, function(x) x[[3]])))
  o <- as.vector(unlist(lapply(Obs_ind, function(x) x[["Values"]])))
  
  if (length(p) != length(o)) {stop("n obs != n pred")} 
  
  #output for FME
   res <- p - o
  
  # output for nloptr
  # sum_res <- sum(res^2)
 
  
 #  #modCost
 #  modelDF <- data.frame("Time" = pred_curr_f$Plasma$Time, 
 #                        "Values" = pred_curr_f$Plasma[[3]])
 #  
 #  
 #  
 #  obsDF <- data.frame("Time" = Obs_ind$Plasma$TIME, 
 #                      "Values" = Obs_ind$Plasma$Values)
 #  
 # mc <- modCost(model = modelDF, obs = obsDF,  x = "Time" )
 #  
 # 
 # modelDF1 <- data.frame("Time" = pred_curr_f$Urine$Time, 
 #                       "Values" = pred_curr_f$Urine[[3]])
 # 
 # 
 # 
 # obsDF1 <- data.frame("Time" = Obs_ind$Urine$TIME, 
 #                     "Values" = Obs_ind$Urine$Values)
 # 
 # mc1 <- modCost(model = modelDF1, obs = obsDF1,  x = "Time" )
 # mc2 <- modCost(model = modelDF1, obs = obsDF1,  x = "Time", cost = mc )
 # mc$model
 # mc1$model
 # mc2$model

  if(plots){

    par(mfrow = c(length(outputPaths), 1))
    for(p in 1:length(outputPaths)){
    plot(Obs_ind[[p]]$TIME, Obs_ind[[p]]$Values, pch = 16, col = "red",
       main = names(outputPaths[p]), xlab = "time", ylab = "Conc", cex.main = 1)
    lines(pred_curr_f[[p]]$Time,pred_curr_f[[p]][[3]], lwd = 2, col = "blue")
    # title(paste0("ID", i), outer = TRUE, cex = 1.5)
    mtext(paste0("ID", i),                   # Add main title
          side = 3,
          line = -1,
          outer = TRUE)
    }

  }
  #FME
   return(res)
  #nloptr
  # return(sum_res)
}


