library(saemix)
library(tidyverse)
library(tidyverse)
library(ggplot2)


# Data
###########################################################
tp_data <- read.csv("theophylline_original.txt")
tp_data <- tp_data %>% filter(TIME > 0)
tp_data$DV <- tp_data$DV * (min(tp_data$AMT)/tp_data$AMT)
tp_data$IDD <- tp_data$ID
tp_data$CMP <- 1

tp_data_2 <- tp_data
tp_data_2$CMP <- 2
tp_data <- rbind(tp_data, tp_data_2)

# Plot data for test
###########################################################
pl <- ggplot(data=tp_data, aes(x=TIME, y=DV)) + geom_point(color="#993399", size=2) +
  xlab("time (h)") + ylab("concentration (mg/l)")
pl +geom_line(color="#993399", aes(group=ID))

pl + geom_line() + facet_wrap(~ID)


# Simple 1-comp PK
###########################################################
model <- function(psi, id, x) {

  browser()

  D   <- 3.1
  t   <-x[,1]
  ka  <-psi[id,1]
  V   <-psi[id,2]
  ke  <-psi[id,3]

  pred <- D*ka / (V*(ka-ke)) * (exp(-ke*t)-exp(-ka*t))

  return(pred)
}


# SAEM
saemix_data <- saemixData(name.data       = tp_data,
                          name.group      = "ID",
                          name.predictors = c("TIME", "CMP"),
                          name.covariates = c("IDD"),
                          name.response   = "DV")

# fixed effects without random effects <- 0 in diag
cov_matrix <- diag(3)
diag(cov_matrix)[2] <- 0


saemix_model <- saemixModel(model = model,
                            psi0  = c(ka = 1, V = 20, ke = 0.5),
                            error.model="combined",
                            fixed.estim = c(1,1,1),
                            covariance.model = cov_matrix,
                            # 1= log-normal, 0 = normal
                            transform.par = c(1,0,1))

saemix_options <- list(map = TRUE, fim = TRUE, ll.is = FALSE,
                       displayProgress = FALSE, seed = 123,
                       save = FALSE, save.graphs = FALSE)


saemix.fit    <- saemix(saemix_model, saemix_data, saemix_options)
saemix_pred <- saemix.predict(saemix.fit)

saemix.plot.fits(saemix_pred)


