library(ggplot2)
library(openxlsx)
library(scales)
library(gridExtra)
library(grid)
library(dplyr)

# all parameters 
# inputFile <- "indiv_from_pop_withUrine-4Param_NM_init20_all.xlsx"
# path <- file.path(getwd(),"../../Data/Results/individual/ind_with_cov/NM/With_Urine")
# estimations <- read.xlsx(file.path(path,inputFile))


################ one by one #######################
# sc <- read.xlsx("../../Data/Results/individual/ind_with_cov/Bob/With_Urine/indiv_from_pop_withUrine_Specific clearance_Bobyqa_init20_all.xlsx") %>% select(c(1,4))
# ip <-  read.xlsx("../../Data/Results/individual/ind_with_cov/Bob/With_Urine/indiv_from_pop_withUrine_Intestinal permeability_Bobyqa_init20_all.xlsx") %>% select(4)
# rc <-  read.xlsx("../../Data/Results/individual/ind_with_cov/Bob/With_Urine/indiv_from_pop_withUrine_Reference concentration_Bobyqa_init20_all.xlsx") %>% select(4)
# gfr <-  read.xlsx("../../Data/Results/individual/ind_with_cov/Bob/With_Urine/indiv_from_pop_withUrine_GFR fraction_Bobyqa_init20_all.xlsx") %>% select(4)
# 
# 
# estimations <- cbind(sc,ip,rc,gfr) %>% mutate_at(c(-1),as.numeric)
# outputname <- "Bob_CovUrine_init20_oneParam"

################ one by one #######################
sc <- read.xlsx("../../Data/Results/individual/ind_with_cov/Bob/With_Urine/indiv_from_pop_withUrine_Specific clearance_Bobyqa_init20_all.xlsx") %>% select(c(1,4))
ip <-  read.xlsx("../../Data/Results/individual/ind_with_cov/Bob/With_Urine/indiv_from_pop_withUrine_Intestinal permeability_Bobyqa_init20_all.xlsx") %>% select(4)
rc <-  read.xlsx("../../Data/Results/individual/ind_with_cov/Bob/With_Urine/indiv_from_pop_withUrine_Reference concentration_Bobyqa_init20_all.xlsx") %>% select(4)
gfr <-  read.xlsx("../../Data/Results/individual/ind_with_cov/Bob/With_Urine/indiv_from_pop_withUrine_GFR fraction_Bobyqa_init20_all.xlsx") %>% select(4)

# ################ one by one #######################
# sc <- read.xlsx("../../Data/Results/individual/ind_with_cov/Bob/With_Urine/indiv_from_pop_withUrine_Specific clearance_Bobyqa_init20_all.xlsx") %>% select(c(1,4))
# ip <-  read.xlsx("../../Data/Results/individual/ind_with_cov/Bob/With_Urine/indiv_from_pop_withUrine_Intestinal permeability_Bobyqa_init20_all.xlsx") %>% select(4)
# rc <-  read.xlsx("../../Data/Results/individual/ind_with_cov/Bob/With_Urine/indiv_from_pop_withUrine_Reference concentration_Bobyqa_init20_all.xlsx") %>% select(4)
# gfr <-  read.xlsx("../../Data/Results/individual/ind_with_cov/Bob/With_Urine/indiv_from_pop_withUrine_GFR fraction_Bobyqa_init20_all.xlsx") %>% select(4)
# 
# 
# estimations <- cbind(sc,ip,rc,gfr) %>% mutate_at(c(-1),as.numeric)
# outputname <- "Bob_CovUrine_init20_oneParam"




estimations <- cbind(sc,ip,rc,gfr) %>% mutate_at(c(-1),as.numeric)
outputname <- "Bob_CovUrine_init20_oneParam"



############### all parameters ####################

estimations <- read.xlsx("../../Data/Results/individual/ind_with_cov/NM/With_Urine/indiv_from_pop_withUrine-4Param_NM_init20_all.xlsx") %>%  select(c(-8, -9))
outputname <- "NM_CovUrine_init20_allParambis"


############### all parameters ####################
# 
estimations <- read.xlsx("../../Data/Results/individual/ind_with_cov/Bob/With_Urine/indiv_from_pop_Withrine2-4P_init20_Bob-allID.xlsx") %>%  select(c(-8,-9))
outputname <- "Bob_CovUrine_init20_allParam"

############### all parameters ####################
# 
# estimations <- read.xlsx("../../Data/Results/individual/ind_with_cov/Bob/With_Urine/indiv_from_pop_withUrine_Bobyqa_init20_all.xlsx") %>%  select(c(-8, -9))
# outputname <- "Bob_CovUrine_init20_allParam"


myEst <- list(

myEst_CYP1A2_specific_clearance = data.frame(ID=estimations[[1]],
                                              estimatedValue=as.numeric(estimations[[4]]),
                                              realValue = 0.00843,
                                              parameter="CYP1A2 specific clearance (L/min)"),


myEst_Intestinal_permeability = data.frame(ID=estimations[[1]],
                                            estimatedValue=as.numeric(estimations[[5]]),
                                            realValue = 8.76e-7,
                                            parameter="Intestinal permeability (dm/min)"),


myEst_CYP1A2_Reference_concentration = data.frame(ID=estimations[[1]],
                                                   estimatedValue=as.numeric(estimations[[6]]),
                                                   realValue = 1.8,
                                                   parameter="CYP1A2 Reference concentration (µmol/L)"),




myEst_GFR_fraction_estimation = data.frame(ID=estimations[[1]],
                                            estimatedValue=as.numeric(estimations[[7]]),
                                            realValue = 0.15,
                                            parameter="GFR fraction estimation")

)

myColours <- scales::hue_pal()(length(myEst))
plots <- vector('list', length(myEst))
for (i in seq_along(myEst)){
  rv <- unique(myEst[[i]]$realValue)
  ev <- median(myEst[[i]]$estimatedValue)
  sv <- unique(myEst[[i]]$realValue) * 1.2
 
  plot <- ggplot(myEst[[i]], aes(x=estimatedValue)) + geom_histogram(alpha=0.5, fill=myColours[i], bins = 100) 
  lim <- max(ggplot_build(plot)$data[[1]]$count)*1.4
   plot <- plot + 
    geom_vline(aes_(xintercept=rv), colour="black", linetype="dashed", size=1) +
    geom_vline(aes_(xintercept=ev), colour=myColours[i], linetype="dashed", size=1) +
    geom_vline(aes_(xintercept=sv), colour="darkgrey", linetype="dashed", size=1) +
    annotate("text", x = rv*1.02, y = lim * 0.9, label=paste0("true:" ,scientific(rv,3)), fontface=2, size = 3) +
    annotate("text", x = ev*1.02, y = lim * 0.8, label=scientific(ev,3), color = myColours[i], fontface=2, size = 3) +
    annotate("text", x = sv, y = lim * 0.6, label="start", color = "darkgrey", fontface=2, size = 3) +
    # ylim(0, lim ) +
    ggtitle(myEst[[i]]$parameter) +
    xlab("Estimated values") +
    theme(legend.position = "none",
          plot.title = element_text(size = 12, face = "bold"),
          axis.text.x = element_text(size = 10),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size=10))
  
  plots[[i]] <-plot
}                                      

tg <- textGrob("Individual parameters estimation (All) - Nelder-Mead\nstart value :+20% from true value", gp = gpar(fontsize = 13, fontface = 'bold'))
grided <- grid.arrange(grobs = plots, ncol = 2)
p <- grid.arrange(grobs = plots, ncol = floor(sqrt(length(plots))), grided, top = tg )
    
plot.save <- function(plot, 
                      width = 800, 
                      height = 500, 
                      text.factor = 1, 
                      # filename = paste0("../Data/", sub('\\..*$', '', inputFile),".png")
                      filename = paste0(outputname,".png")
) {
  
  dpi <- text.factor * 100
  width.calc <- width / dpi
  height.calc <- height / dpi
  
  ggsave(filename = filename,
         dpi = dpi,
         width = width.calc,
         height = height.calc,
         units = 'in',
         plot = plot)
}

plot.save(p, width = 1000, height = 535, text.factor = 1)

summary(myEst[[1]]$estimatedValue)
sd(myEst[[1]]$estimatedValue)
