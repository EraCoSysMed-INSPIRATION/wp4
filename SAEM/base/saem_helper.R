# *******************************************************************
# Project: SAEM Misc Functions
# Script purpose: Define some helper functions for the SAEM fit
# Date: 03-01-2024
# Author: Dominik Selzer (dominik.selzer@uni-saarland.de)
# *******************************************************************

# create a compound
compound <- function(name, cmt, mw, path) {
  list(
    "name" = name,
    "cmt" = cmt,
    "mw" = mw,
    "path" = path
  )
}

# maps the CMTs in the observed data to the paths of the compounds
map_cmt <- function(data, compounds) {
  cmts <- compounds |> map_vec(pluck("cmt"))
  data <- data |> filter(CMT %in% cmts)

  data$NEW_CMT <- NA
  for (i in 1:length(cmts)) {
    data$NEW_CMT[data$CMT == cmts[i]] <- compounds[[i]]$path
  }
  data$CMT <- data$NEW_CMT
  data <- data |> select(-NEW_CMT)
  return(data)
}


# corrects the DV units in the observed data (ng/ml to micromol/l)
correct_dv_units <- function(data, compounds) {
  for (i in 1:length(compounds)) {
    data$DV[data$CMT == compounds[[i]]$path] <- data$DV[data$CMT == compounds[[i]]$path] / compounds[[i]]$mw
  }
  return(data)
}

# filter observed data by individual IDs
filter_observed <- function(data, ids = NULL) {
  if (!is.null(ids)) {
    data <- data |> filter(INDIVIDUALID %in% ids)
  }

  return(data)
}

# filter individuals by individual IDs
filter_individuals <- function(individuals, ids = NULL) {
  if (!is.null(ids)) {
    ids <- as.character(ids)
    individuals <- individuals[ids]
  }

  return(individuals)
}

# Patch simulation times (must be minutes/PK-Sim base unit in observed data)
patch_sim_times <- function(simulations, observed_data) {
  for (i in names(simulations)) {
    simulations[[i]]$outputSchema$clear()
    unique_obs_times <- observed_data |>
      filter(INDIVIDUALID == as.numeric(i)) |>
      pull(TIME) |>
      unique() |>
      sort()

    simulations[[i]]$outputSchema$addTimePoints(unique_obs_times)
  }
  return(simulations)
}

model_parameters <- function(simulations, paths) {
  names <- paths |> map_vec(pluck("path"))
  values <- paths |> map_vec(pluck("value"))
  names(values) <- names
  return(values)
}


create_sim_batches <- function(simulations, parameter_paths) {
  batches <- list()
  for (s in simulations) {
    sim_batch <- createSimulationBatch(simulation = s, parametersOrPaths = parameter_paths)
    batches <- c(batches, sim_batch)
  }

  names(batches) <- names(simulations)
  return(batches)
}

# set the simulation output paths
set_sim_outputs <- function(simulations, compounds) {
  paths <- compounds |> map_vec(pluck("path"))
  for (id in names(simulations)) {
    addOutputs(paths, simulations[[id]])
  }
  return(simulations)
}

# create the saemix data object
create_saem_data <- function(observed_data, time_unit = "min", conc_unit = "µmol/l",
                             log_data = FALSE) {
  if ("EVID" %in% colnames(observed_data)) {
    observed_data <- observed_data |> filter(EVID == 0)
  }

  if (log_data) {
    observed_data$DV <- log(observed_data$DV)
    if (any(is.infinite(observed_data$DV)) || any(is.na(observed_data$DV)))
      stop("Log of DV produced non-finite or NA values produced ")
  }

  saemix_data <- saemixData(
    name.data = observed_data, header = TRUE,
    sep = ",", na = NA, name.group = c("ID"),
    name.predictors = c("CMT", "TIME", "ID", "AS"),
    name.response = c("DV"),
    units = list(x = time_unit, y = conc_unit)
  )
  return(saemix_data)
}


# psi is a vector of the probe parameters
# data is the data set of just just the id
# return a new psi vector
example_param_fn <- function(psi, data) {
  AS <- data$AS[1]
  params <- case_when(AS == 1  ~ psi[1],
                      AS == 1.5 ~ psi[2],
                      AS == 2 ~ psi[3],
                      AS == 3 ~ psi[4])
  return(params)
}

saem_covar_model <- function(parameter, fixed_effects = c()) {

  n_params <- length(parameter)
  idx_params <- seq(n_params)

  covMat <- matrix(0, nrow=n_params, ncol=n_params)
  for (idx in idx_params) {
    covMat[idx, idx] <- ifelse(idx %in% fixed_effects, 0, 1)
  }
  covMat
}

saem_fixed_parameters <- function(parameter, fixed = c()) {
  res <- rep(1, times = length(parameter))

  res[fixed] <- 0
  res
}

saem_model <- function(parameter,
                       simulation_batch,
                       error_model = "proportional",
                       transform_par = NULL,
                       cores = NULL,
                       param_fn = NULL,
                       log_fit = FALSE,
                       covariance_model = NULL,
                       fixed_estim = NULL) {
  if (is.null(transform_par)) {
    transform_par <- rep(1, length(parameter))
  }

  if (is.null(cores)) {
    cores <- detectCores() - 1
  }

  message(paste("Set", cores, "cores"))
  sim_opts <- SimulationRunOptions$new(
    numberOfCores = cores,
    checkForNegativeValues = FALSE,
    showProgress = FALSE
  )

  internal_model <- function(psi, id, x) {
    id_idx <- match(unique(id), id)
    sim_ids <- x$ID[id_idx]

    batch_ids <- c()
    for (i in seq_along(sim_ids)) {
      tmp_id <- sim_ids[i]
      values <- psi[i, ] |> unlist() |> unname()

      if (!is.null(param_fn)) {
        tmp_data <- x |> filter(ID == tmp_id)
        values <- param_fn(values, tmp_data)
      }

      b_tmp <- simulation_batch[[as.character(tmp_id)]]
      batch_ids[[i]] <- b_tmp$addRunValues(values)
    }

    message("Run Batch - Start")
    batches <- simulation_batch[as.character(unique(sim_ids))]
    batch_res <- runSimulationBatches(batches, simulationRunOptions = sim_opts)
    message("Run Batch - Done")

    # collect results
    simulations <- vector("list", length(batch_ids))
    names(simulations) <- batch_ids
    for (result in batch_res) {
      result_ids <- names(result)
      for (res_id in result_ids) {
        simulations[[res_id]] <- result[[res_id]]
      }
    }

    ypred <- c()
    for (i in seq_along(sim_ids)) {
      saem_id <- unique(id)[i]
      x_temp <- x |>
        filter(ID == sim_ids[i]) |>
        slice_head(n = sum(id == saem_id)) |>
        select(CMT, TIME)
      paths <- unique(x_temp$CMT)

      sim <- simulations[[i]]
      output <- getOutputValues(sim, paths)
      output <- output$data |>
        select(-IndividualId) |>
        pivot_longer(cols = paths)

      x_temp$TIME <- round(x_temp$TIME, digits = 2)
      output$Time <- round(output$Time, digits = 2)
      y <- left_join(x_temp, output, by = c("TIME" = "Time", "CMT" = "name")) |> pull(value)

      ypred <- c(ypred, y)
    }

    if (log_fit) {
      ypred <- log(ypred)
    }

    if (anyNA(ypred)) {
      stop("NA values in ypred")
    }

    if (length(ypred) != nrow(x)) {
      stop("Length of ypred does not match length of x")
    }

    return(ypred)
  }

  if (is.null(covariance_model)) covariance_model <- saem_covar_model(parameter)
  if(nrow(covariance_model) != length(parameter) && ncol(covariance_model) != length(parameter))
    stop("covariance_model does not fit parameters")

  if (is.null(fixed_estim)) fixed_estim <- saem_fixed_parameters(parameter)
  if (length(fixed_estim) != length(parameter))
    stop("fixed_estim does not fit parameters")

  if (length(transform_par) != length(parameter))
    stop("Parameter transformation does not fit parameters")

  saemix_model <- saemixModel(
    model = internal_model,
    psi0 = parameter,
    transform.par = transform_par,
    error.model = error_model,
    covariance.model = covariance_model,
    fixed.estim = fixed_estim
  )
  return(saemix_model)
}

create_folder <- function(name) {
  date <- Sys.Date()
  time <- format(Sys.time(), "%Hh%Mm")
  folder_name <- paste0(name, "_", date, "__", time)
  return(folder_name)
}

saem_fit <- function(project_name, saemix_model, saemix_data, seed, k1k2 = c(50, 200)) {

  folder_name <- create_folder(project_name)
  start_time <- Sys.time()
  data_df <- saemix_data@data


  saemix_options <- list(
    map = TRUE, # MAP
    fim = TRUE, # FIM for error estimates
    ll.is = FALSE, # Log-likelihood
    ll.gq = TRUE,
    displayProgress = TRUE,
    nbdisplay = 1,
    seed = seed,
    nbiter.saemix = k1k2,
    nbiter.burn = 5,
    nbiter.map = 5,
    nb.sim = 5,
    nb.simpred = 10,
    ipar.lmcmc = 50,
    save = TRUE,
    save.graphs = FALSE,
    directory = folder_name
  )

  tic("Run")
  saemix_fit <- saemix(saemix_model, saemix_data, saemix_options)
  toc()
  end_time <- Sys.time()

  # runtime file
  runtime_text <- c(sprintf("Start time: %s", start_time),
                    sprintf("End time: %s", end_time))
  write(runtime_text, file.path(folder_name, "runtime.txt"), sep = "\n")

  # input file
  write.table(data_df, file.path(folder_name, "data.txt"), sep = "\n", row.names = FALSE)

  # convergence file
  conv_data <- get_convergence_data(saemix_fit)
  write.table(conv_data, file.path(folder_name, "convergence.csv"), sep = ",", row.names = FALSE)

  # sdtab
  sdtab_data <- get_pred_data(saemix_fit)
  write.table(sdtab_data, file.path(folder_name, "sdtab.csv"), sep = ",", row.names = FALSE)

  # rds results
  saveRDS(saemix_fit@results, file = file.path(folder_name, "results.rds"))

  return(saemix_fit)
}


get_convergence_data <- function(fit_object) {
  df <- fit_object["results"]["allpar"] |> as.data.frame()
  df <- df |>
    mutate(k1 = fit_object["options"]$nbiter.saemix[1]) |>
    mutate(k2 = fit_object["options"]$nbiter.saemix[2]) |>
    mutate(iter = (1:nrow(df)) - 1)

  return(df)
}

get_pred_data <- function(fit_object) {
  data <- fit_object["data"]["data"]
  names(data) <- make.unique(names(data))
  data <- data |>
    mutate(pred = fit_object["results"]["ppred"]) |>
    mutate(ipred = fit_object["results"]["ipred"]) |>
    mutate(ires = fit_object["results"]["ires"]) |>
    mutate(iwres = fit_object["results"]["iwres"])

  return(data)
}
