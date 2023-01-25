
setwd("C:/Users/I0393817/Sanofi(1)/EracoSysMed - Inspiration - General/Modelling/PK-Sim/Simulations for estimation tests")


pop <- read.csv("Kaumeier oral solution 185mg - pop - for parameter estimation-global dataset.csv")


names(pop)
names(pop) <- c("IndividualId",
			"Gender",
			"Population",
			"Weight [kg]",
			"BMI [kg/dm²]",
			"BSA [dm²]",
               	"Age[years]",
             	"Height [dm]",
              	"VenousBlood.Volume [l]",    
			"ArterialBlood.Volume [l]",
		 	"Bone.Volume [l]",
	           	"Brain.Volume [l]",
         		"Fat.Volume [l]",
	            "Gonads.Volume [l]",
        		"Heart.Volume [l]",
	          	"Kidney.Volume [l]",
        		"Stomach.Volume [l]",        
			"SmallIntestine.Volume [l]",
			"LargeIntestine.Volume [l]", 
			"Liver.Volume [l]",
         		"Lung.Volume [l]",           
			"Muscle.Volume [l]",
        		"Pancreas.Volume [l]",       
			"Skin.Volume [l]",          
			"Spleen.Volume [l]") 



PKdata <- read.csv("Kaumeier oral solution 185mg - pop - for parameter estimation-Results.csv")

head(PKdata)

names(PKdata)

names(PKdata) <-c("IndividualId",                                                                       
			"Time [min]",                                                                            
			"PeripheralVenousBlood.Plasma [µmol/l]",
			"Kidney.Urine.Fraction.excreted.to.urine") 


finalDataset <- merge(pop,PKdata,by="IndividualId")

head(finalDataset)

write.csv(finalDataset, "finalDataset Theophylline 100subj.csv", row.names = FALSE)

