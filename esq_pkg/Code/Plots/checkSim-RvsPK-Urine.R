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
library(tidyr)

file.sources = list.files(file.path(getwd(),"../R"),
                          pattern="*.R$", full.names=TRUE,
                          ignore.case=TRUE)

for (f in file.sources) {
  source(f)
}


#####VARIABLE DEFINITION#####
# Path to the folder where the model file is located.
modelFolder <- file.path(getwd(), "../../Models/Simulations")
# Path to the folder where experimental data files are located
dataFolder = file.path(getwd(), "../../Data")
resultsFolder = file.path(getwd(), "../../Data/Results/individual/ind_with_cov/Bob/With_Urine")
# Name of the excel file with experimental data
dataFile = "theo_all_urine.xlsx"
# Name of the csv file with covariate data
pksim_pop = "Kaumeier oral solution 185mg - pop - for parameter estimation-Population.csv"


outputs <-2 #1 for Plasma, 2 for Urine
leg <- "est_Bob" #legend for estimations
pdfName <- "4Param_Urine_Bob_AllID.pdf"
estimated_param <- "indiv_from_pop_Withrine2-4P_init20_Bob-allID.xlsx"
 plot_title <- "Urine excreted fraction - Bobyqa"
# plot_title <- "Plasma - Bobyqa"
# estimated parameters
# estimatedParam <- read.xlsx(xlsxFile =  file.path(resultsFolder,"Nelder-Mead/indiv_results_cov_Nelder-Mead_init20.xlsx"))

PKSim_data <- read.xlsx(xlsxFile =  file.path(dataFolder, dataFile), sheet = "Sheet1") %>% mutate_at(11, as.numeric) 
# Create individuals based on input datasets
myPopulation <- loadPopulation(csvPopulationFile = file.path(dataFolder, pksim_pop))


# Load simulation
simNames <- c("Kaumeier oral solution 185mg.pkml")
sim_R <- loadSimulation(file.path(modelFolder, simNames))


# Obs R

populationResults_ObsR <- runSimulation(simulation = sim_R, population = myPopulation)
resultsData_ObsR   <- getOutputValues(populationResults_ObsR, quantitiesOrPaths = populationResults_ObsR$allQuantityPaths[[outputs]])
obs_R <- resultsData_ObsR$data %>% mutate(Value = "obs_R_true") %>% mutate(ID = paste0("ID ", IndividualId)) %>% select(-IndividualId)


# Obs PKSim

# obs_PKSim <- PKSim_data %>% mutate(ID = paste0("ID ", Patient.Id ), Value = "obs_PKSim") %>% rename(Time = `Time.[min]`, `Organism|PeripheralVenousBlood|Theophylline|Plasma (Peripheral Venous Blood)` = `Concentration.(molar).[µmol/l]`) %>% 
                 # select(c(10,11,15,16))

 obs_PKSim <- PKSim_data %>% mutate(ID = paste0("ID ", Patient.Id ), Value = "obs_PKSim") %>% rename(Time = `Time.[min]`, `Organism|Kidney|Urine|Theophylline|Fraction excreted to urine` = `Fraction.[]`) %>% 
   select(c(10,11,15,16))
# Obs R_initial
# "CYP1A2|Reference concentration" both in population and sim. seems population take over
myPopulation <- loadPopulation(csvPopulationFile = file.path(dataFolder, pksim_pop))
sim_R <- loadSimulation(file.path(modelFolder, simNames))


myPopulation$getParameterValues("CYP1A2|Reference concentration")
myPopulation$setParameterValues("CYP1A2|Reference concentration", 1.2*myPopulation$getParameterValues("CYP1A2|Reference concentration"))
myPopulation$getParameterValues("CYP1A2|Reference concentration")

getParameter("Theophylline-CYP1A2-adjusted|Specific clearance", sim_R)
setParameterValuesByPath(
  "Theophylline-CYP1A2-adjusted|Specific clearance",
  getParameter("Theophylline-CYP1A2-adjusted|Specific clearance", sim_R)$value*1.2,
  sim_R
  )
getParameter("Theophylline-CYP1A2-adjusted|Specific clearance", sim_R)

getParameter("Theophylline|Intestinal permeability (transcellular)", sim_R)
setParameterValuesByPath(
  "Theophylline|Intestinal permeability (transcellular)",
  getParameter("Theophylline|Intestinal permeability (transcellular)", sim_R)$value*1.2,
  sim_R
)
getParameter("Theophylline|Intestinal permeability (transcellular)", sim_R)

getParameter("Neighborhoods|Kidney_pls_Kidney_ur|Theophylline|Glomerular Filtration-GFR|GFR fraction", sim_R)
setParameterValuesByPath(
  "Neighborhoods|Kidney_pls_Kidney_ur|Theophylline|Glomerular Filtration-GFR|GFR fraction",
  getParameter("Neighborhoods|Kidney_pls_Kidney_ur|Theophylline|Glomerular Filtration-GFR|GFR fraction", sim_R)$value*1.2,
  sim_R
)
getParameter("Neighborhoods|Kidney_pls_Kidney_ur|Theophylline|Glomerular Filtration-GFR|GFR fraction", sim_R)



populationResults_ObsR_init <- runSimulation(simulation = sim_R, population = myPopulation)
resultsData_ObsR_init   <- getOutputValues(populationResults_ObsR_init, quantitiesOrPaths = populationResults_ObsR_init$allQuantityPaths[[outputs]])
obs_Rinit <- resultsData_ObsR_init$data %>% mutate(Value = "obs_R_init") %>% mutate(ID = paste0("ID ", IndividualId)) %>% select(-IndividualId)



#ObsR estimated

myPopulation <- loadPopulation(csvPopulationFile = file.path(dataFolder, pksim_pop))
sim_R <- loadSimulation(file.path(modelFolder, simNames))
estimatedParam <- read.xlsx(xlsxFile =  file.path(resultsFolder,estimated_param), sep.names =" ")


parameterPaths <- c("Theophylline-CYP1A2-adjusted|Specific clearance",
                     "Theophylline|Intestinal permeability (transcellular)",
                      "CYP1A2|Reference concentration",
                    "Neighborhoods|Kidney_pls_Kidney_ur|Theophylline|Glomerular Filtration-GFR|GFR fraction")



est <- data.frame()

# for(i in unique(obs_PKSim$ID)){
for(i in unique(estimatedParam$ID)){
  single_ind <- paste0("../../Data/IndivParam_fromPop/",i,".csv") 
  myPopulation <- loadPopulation(csvPopulationFile = single_ind)
  id <- myPopulation$allIndividualIds
  param <- myPopulation$getParameterValuesForIndividual(id)
  
  setParameterValuesByPath(param$paths, param$values, sim_R)
 
 
  estimatedParam_ind <- estimatedParam %>% filter(ID == i)  %>% select(4:7) %>% gather(key, value) %>% 
  mutate_at(vars(value), as.numeric) %>%   as.list()
   
 
  # check <- lapply(parameterPaths,ospsuite::getParameter, container = sim_R)
 
 setParameterValuesByPath(
   parameterPaths = estimatedParam_ind$key,
   values         = estimatedParam_ind$value,
   simulation     = sim_R
 )

 # check <- lapply(parameterPaths,ospsuite::getParameter, container = sim_R)

 simulationResults <-  ospsuite::runSimulation(sim_R)
 resultsPath <- simulationResults$allQuantityPaths[[outputs]]
 resultsData <- getOutputValues(simulationResults, quantitiesOrPaths = resultsPath)
 resultsData$data$ID <- i
 resultsData$data$Value <- leg
 resultsData$data$IndividualId  <- NULL
 # print(simulationResults)
 est <- rbind(est,resultsData$data)
 
}




all <- rbind(obs_R, obs_PKSim, obs_Rinit, est ) %>% rename(conc = 2) 
indiv_df <- split(all, all$ID)
plots <- lapply(indiv_df,
                function(n){
                  ggplot(n,aes(x=Time, y=conc, color = Value, linetype = Value)) +
                    geom_line(size = 1)+
                    scale_color_manual(values=c("green","black", "red", "lightblue"))+
                    scale_linetype_manual(values=c("solid", "dotted","dotted", "dotted"))+
                    ggtitle(unique(n$ID))+
                    xlab("EF %")+
                    theme(
                      legend.title = element_blank(),
                      legend.position = c(0.75, 0.25),
                      plot.title=element_text(hjust = 0.5,margin=margin(b=-30)))
                }
)



 ml <- marrangeGrob(plots, nrow=2, ncol=3, top = plot_title) 
 ggsave(pdfName, ml, width = 10, height = 10, dpi = 300)
 
 


 