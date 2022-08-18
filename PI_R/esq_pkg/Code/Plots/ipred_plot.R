library(ospsuite)
# library(esqlabsR)
# library(R6)
# library(FME)
 library(openxlsx)
 library(dplyr)
 library(parallel)
 library(progress)
library(ggplot2)
library(gridExtra)


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
resultsFolder = file.path(getwd(), "../Data/Results/individual")
# Name of the excel file with experimental data
dataFile = "theo_all.xlsx"
# Name of the csv file with covariate data
dataFileCov = "finalDataset Theophylline 100subj.csv"


# estimated parameters
estimatedParam <- read.xlsx(xlsxFile =  file.path(resultsFolder,"Nelder-Mead/indiv_results_cov_Nelder-Mead_init20.xlsx"))

PKSim_data <- read.xlsx(xlsxFile =  file.path(dataFolder, dataFile), sheet = "Sheet1") %>% 
  mutate_if(is.character, as.numeric)

# Create individuals based on input datasets
individuals <- create_human_individuals(file.path(dataFolder, dataFileCov), 
                                        sep = ",", height_unit = "dm")
# null individuals
null_ind <- which(sapply(individuals, is.null))
individuals <-individuals[-null_ind]

# Load simulation
simNames <- c("Kaumeier oral solution 185mg.pkml")
sim_R <- loadSimulation(file.path(modelFolder, simNames))
sim_pred <- loadSimulation(file.path(modelFolder, simNames))

parameterPaths <- c("Theophylline-CYP1A2-adjusted|Specific clearance",
                    "Theophylline|Intestinal permeability (transcellular)",
                    "CYP1A2|Reference concentration",
                    "Neighborhoods|Kidney_pls_Kidney_ur|Theophylline|Glomerular Filtration-GFR|GFR fraction")




# sim obs R initial

clearOutputIntervals(sim_R)
setOutputInterval(simulation = sim_R, startTime = 0, endTime = 4320, resolution = 1/60)
obs_R <- data.frame()
for(i in seq_along(individuals)){

# Apply individual parameters
setParameterValuesByPath(
  parameterPaths = individuals[[i]]$distributedParameters$paths,
  values         = individuals[[i]]$distributedParameters$values,
  simulation     = sim_R
)

simulationResults <-  ospsuite::runSimulation(sim_R)
resultsPath <- simulationResults$allQuantityPaths[[1]]
resultsData <- getOutputValues(simulationResults, quantitiesOrPaths = resultsPath)
resultsData$data$ID <- names(individuals[i])
resultsData$data$Value <- "obs_R"
# print(simulationResults)
obs_R <- rbind(obs_R,resultsData$data)
}




# sim pred
clearOutputIntervals(sim_pred)
setOutputInterval(simulation = sim_pred, startTime = 0, endTime = 4320, resolution = 1/60)
pred_R <- data.frame()
for(i in seq_along(individuals)){
  # Apply individual parameters
  setParameterValuesByPath(
    parameterPaths = individuals[[i]]$distributedParameters$paths,
    values         = individuals[[i]]$distributedParameters$values,
    simulation     = sim_pred
  )
  
  est <- estimatedParam[i,4:7]
  
  modelParam <- lapply(parameterPaths,ospsuite::getParameter, container = sim_pred)
  setParameterValues(parameters = modelParam[[1]], values =as.numeric(est[1]))
  setParameterValues(parameters = modelParam[[2]], values =as.numeric(est[2]))
  setParameterValues(parameters = modelParam[[3]], values =as.numeric(est[3]))
  setParameterValues(parameters = modelParam[[4]], values =as.numeric(est[4]))
  
  simulationResults <-  ospsuite::runSimulation(sim_pred)
  resultsPath <- simulationResults$allQuantityPaths[[1]]
  resultsData <- getOutputValues(simulationResults, quantitiesOrPaths = resultsPath)
  resultsData$data$ID <- names(individuals[i])
  resultsData$data$Value <- "pred_Nelder"
  # print(simulationResults)
  pred_R <- rbind(pred_R,resultsData$data)
}


# Obs PKSim

obs_PKSim <- PKSim_data %>% mutate(ID = paste0("ID ", Group.Id ), Value = "obs_PKSim") %>% rename(Time = `Time.[min]`, `Organism|PeripheralVenousBlood|Theophylline|Plasma (Peripheral Venous Blood)` = `Concentration.(molar).[µmol/l]`) %>% 
                select(c(10,11,15,16)) %>% filter(!ID %in% names(null_ind))



# obs <- merge(obs_R, Obs_PKSim, by = c("ID", "Time"))
# all <- merge(obs, pred_R, by = c("ID", "Time")) %>% rename(conc = 4)           
             
obs_R1 <- obs_R[,-1]
pred_R1 <- pred_R[,-1]

all <- rbind(obs_R1, obs_PKSim, pred_R1 ) %>% rename(conc = 2) 
indiv_df <- split(all, all$ID)
 plots <- lapply(indiv_df,
                 function(n){
                   ggplot(n,aes(x=Time, y=conc, color = Value, linetype = Value)) +
                   geom_line(size = 1)+
                   scale_color_manual(values=c("red", "blue", "black"))+
                   scale_linetype_manual(values=c("solid","solid", "dotted"))+
                   ggtitle(unique(n$ID))+
                   theme(
                     legend.title = element_blank(),
                     legend.position = c(0.75, 0.75),
                    plot.title=element_text(hjust = 0.5,margin=margin(b=-30)))
                 }
                 )

                   

 ml <- marrangeGrob(plots, nrow=2, ncol=3) 
 ggsave("Nelder-mead.pdf", ml, width = 10, height = 10, dpi = 300)
 
 

 individuals['ID 44']$`ID 44`$derivedParameters$values[c(1,2,3)]
 individuals['ID 44']$`ID 44`$distributedParameters$values[c(1,3,9,10,12,13,15,30,32,34,61,64,67,69,71,73,90,93,95)]
 