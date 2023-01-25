
##############################################################################################

# times in minutes
# resolution in pts/min
set_simulation_times <- function(simulations, observed_data, resolution = 1) {

  times <- observed_data %>%
    group_by(INDIVIDUALID) %>%
    summarise(min = min(TIME), max = max(TIME))


  sim_names <-  names(simulations)
  for (idx in seq_along(simulations)) {
    sim <- simulations[[idx]]

    tmp <- times %>% filter(INDIVIDUALID == sim_names[idx])
    start_time <- tmp$min
    end_time <- tmp$max

    setOutputInterval(simulation = sim, startTime = start_time,
                      endTime = end_time, resolution = resolution)
  }
}

# parameter_df is a data_frame with ID
# other columns are names of the parameters
set_simulation_parameters <- function(simulations, parameter_df) {

  for (idx in seq_along(simulations)) {
    id <- names(simulations)[idx]

    tmp <- parameter_df %>% filter(ID == id) %>% select(-ID)
    parameter_pathes <- colnames(tmp)

    sim <- simulations[[idx]]
    for (p in parameter_pathes) {
      parameter <- getParameter(p, sim)
      setParameterValues(parameter, tmp[[p]])
    }
  }
}


# Create Simulations
# from
# - list of individuals
# - model definition from pkml file
create_simulations <- function(pkml_file,
                               obs_data_file,
                               weight_unit = "kg",
                               height_unit = "cm",
                               age_unit = "year(s)",
                               simulation_resolution = NULL) {

  individuals <- create_human_individuals(obs_data_file,
                                          weight_unit = weight_unit,
                                          height_unit = height_unit,
                                          age_unit = age_unit,
                                          progress = FALSE)

  simulations <- list()
  for (i in individuals) {
    sim <- loadSimulation(pkml_file, loadFromCache = FALSE)
    set_individual_parameters(i, sim)
    simulations <- c(simulations, sim)
  }

  data <- read.table(obs_data_file, sep = ",", header = TRUE)
  names(simulations) <- unique(data$INDIVIDUALID)

  if (!is.null(simulation_resolution)) {
    set_simulation_times(simulations, data, simulation_resolution)
  }

  return(simulations)
}

# Run Simulations
# - (list of simulations) in parallel and fetch results
run_simulations <- function(simulations,
                            ncores = NULL,
                            progress = FALSE) {

  if (is.null(ncores))
    ncores <- parallel::detectCores(logical = FALSE)

  options <- SimulationRunOptions$new()
  options$numberOfCores <- ncores
  options$showProgress <- progress

  runs <- runSimulations(simulations, simulationRunOptions = options)
  res <- lapply(runs, getOutputValues)
  names(res) <- names(simulations)
  return(res)
}

# Fetch Simulation Results
fetch_simulation_results <- function(sim_results, paths) {
  results <- list()

  for (i in seq_along(sim_results)) {
    res <- sim_results[[i]]
    id <- names(sim_results)[i]

    tmp <- data.frame(ID = id, TIME = res$data$Time)

    for (p in paths) {
      tmp <- cbind(tmp, data.frame(res$data[[p]]))
    }

    colnames(tmp) <- c("ID", "TIME", paths)
    results <- c(results, list(tmp))
  }

  bind_rows(results)
}


##############################################################################################







create_SAEM_model <- function(simulations, parameter_paths) {

  paths <- parameter_paths
  sims <- simulations

  # psi is a matrix of parameters
  fn <- function(psi, id, x) {

    set_simulation_parameters()


  }


  return(fn)
}


