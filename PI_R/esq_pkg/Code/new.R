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
dataFile_plasma = "theo_all_plasma.xlsx"
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


df <- read.csv(file.path(datafolder,"finalDataset Theophylline 100subj.csv"))




data_plasma <- read.xlsx(xlsxFile =  file.path(dataFolder, dataFile_plasma), sheet = "Sheet1") %>% mutate_at(.vars = c(2,10,11), as.numeric)

ds_plasma <-split(data_plasma,data_plasma$Patient.Id )


indiv_theo_plasma <- createWorkbook()

for (i in 1:length(ds_plasma)) { 
  addWorksheet(indiv_theo_plasma, sheetName=paste0("ID ",names(ds_plasma[i])))
  writeData(indiv_theo_plasma, sheet=i, x=ds_plasma[[i]]) # Note [[]]
} 

saveWorkbook(indiv_theo_plasma, file.path(dataFolder,"indiv_plasma.xlsx"), overwrite = TRUE)


