set_individual_parameters <- function(individual, simulation) {

  # lazy tests if input is valid
  if (!all(c("distributedParameters", "derivedParameters") %in% names(individual)))
    stop("individual must be a valid Individual", call. = FALSE)

  if (!"Simulation" %in% class(simulation))
    stop("simulation must be a OSP Simulation", call. = FALSE)

  setParameterValuesByPath(parameterPaths = individual$distributedParameters$paths,
                           values = individual$distributedParameters$values,
                           simulation = simulation)
}

create_simulations <- function(pkml_file,
                               individuals,
                               simulation_resolution = NULL) {

  simulations <- list()
  for (i in individuals) {
    sim <- loadSimulation(pkml_file, loadFromCache = FALSE)
    set_individual_parameters(i, sim)
    simulations <- c(simulations, sim)
  }

  if (!is.null(simulation_resolution)) {
    set_simulation_times(simulations, data, simulation_resolution)
  }

  return(simulations)
}




# LEGACY CODE ----
# *******************************************************************
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


