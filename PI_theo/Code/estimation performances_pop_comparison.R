library(ggplot2)
library(openxlsx)
library(scales)
library(gridExtra)
library(grid)

inputFile_bobyqa <- "pop_results_bobyqa_init20_4parameters.xlsx"
inputFile_Marq <- "pop_results_Marq_init20_4parameters.xlsx"
inputFile_NM <- "pop_results_Nelder-Mead_init20_4parameters.xlsx"

# path <- file.path(getwd(),"../Data")

path <- file.path(getwd())

estimations_bob <- read.xlsx(file.path(path,paste0("../Data/Results/",inputFile_bobyqa)))
estimations_bob[] <- lapply(estimations_bob,type.convert,as.is=TRUE)
estimations_bob$algo <- "Bobyqa"

estimations_Marq <- read.xlsx(file.path(path,paste0("../Data/Results/",inputFile_Marq)))
estimations_Marq[] <- lapply(estimations_Marq,type.convert,as.is=TRUE)
estimations_Marq$algo <- "Levenberg-Marquardt"

estimations_NM <- read.xlsx(file.path(path,paste0("../Data/Results/",inputFile_NM)))
estimations_NM[] <- lapply(estimations_NM,type.convert,as.is=TRUE)
estimations_NM$algo <- "Nelder-Mead"

estimations <- do.call("rbind", list(estimations_bob, estimations_Marq, estimations_NM))

myEst <- list(

myEst_CYP1A2_specific_clearance = data.frame(var=estimations[[1]],
                                              estimatedValue=as.numeric(estimations[[4]]),
                                              realValue = 0.00843,
                                              parameter="CYP1A2 specific clearance (L/min)",
                                              Method = estimations[[10]]),
                                              

myEst_Intestinal_permeability = data.frame(var=estimations[[1]],
                                            estimatedValue=as.numeric(estimations[[5]]),
                                            realValue = 8.76e-7,
                                            parameter="Intestinal permeability (dm/min)",
                                            Method = estimations[[10]]),
                                            

myEst_CYP1A2_Reference_concentration = data.frame(var=estimations[[1]],
                                                   estimatedValue=as.numeric(estimations[[6]]),
                                                   realValue = 1.8,
                                                   parameter="CYP1A2 Reference concentration (µmol/L)",
                                                   Method = estimations[[10]]),
                                                  


myEst_GFR_fraction_estimation = data.frame(var=estimations[[1]],
                                            estimatedValue=as.numeric(estimations[[7]]),
                                            realValue = 0.15,
                                            parameter="GFR fraction estimation",
                                           Method = estimations[[10]])
                                            
)


plots <- vector('list', length(myEst))


for (i in seq_along(myEst)){
  rv <- unique(myEst[[i]]$realValue) 
 
 
  
  
  
  plot <- ggplot(myEst[[i]], aes(x = var, y=estimatedValue, color = Method)) + geom_point(alpha=0.5,   size = 3)
  lim <- max(max(ggplot_build(plot)$data[[1]]$y), rv)*1.4
  asp <- (1.2-0.8)/lim
  
   plot <- plot +
     geom_hline(aes_(yintercept=rv), colour="black", linetype="dashed", size=1) +
     geom_vline(aes(xintercept=1), colour="black", linetype="dashed", size=1) +
    annotate("text", x = 0.7, vjust = 1, hjust = 0.25,y = rv, label=paste0("true:" ,scientific(rv,3)), fontface=2, size = 3) +
     geom_abline(slope = rv, colour="grey") +
     annotate("text", x = 1.3, y = 1.3 * rv , label = "start value",  angle = atan(rv * asp) * 180 /pi, color = "grey", vjust = 1, hjust = 0.7,  fontface=2, size = 3) +
    ylim(0, lim ) + xlim(0.7, 1.3) +
    ggtitle(myEst[[i]]$parameter) +
    ylab("Estimated values") +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 12, face = "bold"),
          axis.text.x = element_text(size = 10),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size=10))
   
  plots[[i]] <-plot
}                                      


  # tmp <- ggplot_gtable(ggplot_build(plots[[1]]))
  # leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  # legend <- tmp$grobs[[leg]]


tg <- textGrob("Pooled data - 4 parameters estimation\nstart value :[-20% - +20%] from true value", gp = gpar(fontsize = 13, fontface = 'bold'))
grided <- grid.arrange(grobs = plots, ncol = 2)#, legend)
p <- grid.arrange(grobs = plots, ncol = floor(sqrt(length(plots))), grided, top = tg )
    
plot.save <- function(plot, 
                      width = 800, 
                      height = 800, 
                      text.factor = 1, 
                      filename = paste0("../Data/Results/pop_results_init20_4Paramer.png")
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

plot.save(p, width = 1000, height = 1000, text.factor = 1)
