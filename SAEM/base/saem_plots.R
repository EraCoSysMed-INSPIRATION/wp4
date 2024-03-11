library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(GGally)
library(ggforce)
library(purrr)

plot_convergence <- function(file = "convergence.csv",
                             theme = ggplot2::theme_light(),
                             color = "coral",
                             nrow = NULL,
                             ncol = NULL,
                             mapping = NULL) {
  data <- read.csv("convergence.csv", check.names = FALSE)
  data <- data |> pivot_longer(-c(k1, k2, iter))
  data <- data |> mutate(color = color)


  if (!is.null(mapping)) {
    for (old_name in names(mapping)) {
      data$name[data$name == old_name] <- mapping[old_name]
    }
  }

  ggplot(data, aes(x = iter, y = value, color = color)) +
    geom_line(size = 1, color = color) +
    facet_wrap(.~name, scales = "free_y", nrow = nrow, ncol = ncol) +
    geom_vline(xintercept = data$k1[1], size = 1, linetype = "dashed", col = "darkgrey") +
    xlab("Iteration") +
    ylab("Value") +
    theme +
    theme(legend.position = "none") +
    scale_x_continuous(breaks= pretty_breaks())

}


plot_gof <- function(file = "sdtab.csv",
                     type = c("ppred", "ipred"),
                     color = c("darkgrey", "red", "darkgreen"),
                     smooth = TRUE,
                     theme = ggplot2::theme_light()) {

  type <- match.arg(type)
  data <- read.csv(file, check.names = FALSE)
  if (type == "ppred") {
    data <- data |> mutate(value = pred)
    x_lab <- "Population Predictions"
  } else {
    data <- data |> mutate(value = ipred)
    x_lab <- "Individual Predictions"
  }

  combined_range <- range(c(data$value, data$DV), na.rm = TRUE)
  log_limits <- log10(combined_range)

  plot <- ggplot(data, aes(x = value, y = DV)) +
    geom_point(size = 2, color = color[1]) +
    geom_abline(intercept = 0, slope = 1, color = color[2], size = 1) +
    scale_x_log10(limits = 10^log_limits, name = x_lab) +
    scale_y_log10(limits = 10^log_limits, name = "Observations") +
    theme

  if (smooth) {
    plot <- plot +
      geom_smooth(color = color[3], linetype = "dashed", size = 1, se = FALSE)
  }

  plot
}

plot_res <- function(file = "sdtab.csv",
                     type = c("ires", "iwres"),
                     color = c("darkgrey", "red", "darkgreen"),
                     smooth = TRUE,
                     theme = ggplot2::theme_light()) {

  type <- match.arg(type)
  data <- read.csv(file, check.names = FALSE)
  if (type == "ires") {
    data <- data |> mutate(value = ires)
    y_lab <- "Individual Residuals (IRes)"
  } else {
    data <- data |> mutate(value = iwres)
    y_lab <- "Individual Weighted Residuals (IWRes)"
  }

  plot <- ggplot(data, aes(x = ipred, y = value)) +
    geom_point(size = 2, color = color[1]) +
    geom_hline(yintercept = 0, linetype = "dashed", color = color[2], size = 1) +
    theme +
    xlab("Predicted (IPRED)") +
    ylab(y_lab)

  if (smooth) {
    plot <- plot +
      geom_smooth(color = color[3], linetype = "dashed", size = 1, se = FALSE)
  }

  plot
}


plot_param_dist <- function(file = "indiv_parameters.txt",
                            res_file = "results.rds",
                            theme = ggplot2::theme_light(),
                            colors = c("darkgrey", "red"),
                            nrow = NULL,
                            ncol = NULL,
                            param_names) {

  data <- read.delim(file, check.names = FALSE, sep = " ", skip = 1, header = FALSE)
  colnames(data) <- c("ID", param_names)

  res_data <- readRDS(res_file)
  population_estimates <- res_data@fixed.effects |>
    t() |>
    as.data.frame() |>
    set_names(param_names)


  data <- data |> pivot_longer(-c(ID))
  population_estimates <- population_estimates |> pivot_longer(everything())


  ggplot(data, aes(x = value)) +
    geom_density(size = 1, color = colors[1]) +
    geom_vline(aes(xintercept = value),
               data = population_estimates,
               col = colors[2], size = 1, linetype = "dashed") +
    facet_wrap(.~name, scale = "free", nrow = nrow, ncol = ncol) +
    ylab("Density") +
    xlab("Parameter Value") +
    theme +
    scale_x_log10()
}

plot_param_correlation <- function(file = "indiv_parameters.txt",
                                   param_names) {
  data <- read.delim(file, check.names = FALSE, sep = " ", skip = 1, header = FALSE)
  colnames(data) <- c("ID", param_names)
  data <- data |> select(-ID)
  ggpairs(data)

}


plot_profiles <- function(file = "sdtab.csv",
                          nrow = 4,
                          ncol = 4,
                          log = TRUE) {

  data <- read.csv(file)
  data <- data |> pivot_longer(c("DV", "ipred", "pred"))
  data <- data |> mutate(name = case_when(name == "DV" ~ "Observed",
                                          name == "pred" ~ "PRED",
                                          name == "ipred" ~ "IPRED"))

  n_ids <- length(unique(data$ID))
  pages <- ceiling(n_ids/(nrow * ncol))
  for (page in seq(pages)) {
    plot <- ggplot(data) +
      geom_point(aes(x = TIME, y = value, color = name)) +
      geom_line(aes(x = TIME, y = value, color = name)) +
      facet_wrap_paginate(.~ID, scale = "free", nrow = nrow, ncol = ncol, page = page) +
      theme_light() +
      xlab("Time [min]") +
      ylab("Concentration [micromol/l]") +
      theme(legend.position = "top") +
      guides(color = guide_legend(title = NULL))

    if (log) {
      plot <- plot +
        scale_y_log10()
    }

    print(plot)
  }
}
