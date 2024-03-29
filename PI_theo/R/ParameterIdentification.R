#' @title ParameterIdentification
#' @docType class
#' @description A task to identify optimal parameter values based on simulation outputs and observed data
#' @import ospsuite R6
#' @export
#' @import FME hash esqlabsR
#' @format NULL
ParameterIdentification <- R6::R6Class(
  "ParameterIdentification",
  inherit = Printable,
  cloneable = TRUE,
  active = list(
    #' @field simulations Named list with simulation objects, where names are IDs of the root container of the simulation
    simulations = function(value) {
      if (missing(value)) {
        as.list(private$.simulations)
      } else {
        stop(messages$errorPropertyReadOnly("simulations"))
      }
    },

    #' @field parameters List of \code{PIParameters} objects to be optimized. Read-only
    parameters = function(value) {
      if (missing(value)) {
        private$.parameters
      } else {
        stop(messages$errorPropertyReadOnly("parameters"))
      }
    },

    #' @field configuration An object of \code{PIConfiguration}
    configuration = function(value) {
      if (missing(value)) {
        private$.configuration
      } else {
        validateIsOfType(configuration, "PIConfiguration")
        private$.configuration <- value
      }
    },

    #' @field outputMappings List of \code{PIOutputMapping} objects. Each mapping assigns a set of observed data
    #' given by \code{XYData}-objects
    outputMappings = function(value) {
      if (missing(value)) {
        private$.outputMappings
      } else {
        stop(messages$errorPropertyReadOnly("outputMappings"))
      }
    }
  ),
  private = list(
    .simulations = NULL,
    .stateVariables = NULL,
    .parameters = NULL,
    .outputMappings = NULL,
    .configuration = NULL,

    # Perform one iteration of the optimization workflow and calculate the residuals
    # @param currVals Numerical vector of the parameter values to be applied
    # @return Residuals, based on the selected objective function
    .iterate = function(currVals) {
      # Simulate with new parameter values and return the data mappings from which the error will be calculated.
      dataMappings <- private$.evaluate(currVals)
      return(private$.calculateResiduals(dataMappings))
     
    },

    # Evaluate all simulations with given parameter values
    # @param currVals Numerical vector of the parameter values to be applied
    # @return A list of \code{DataMapping} objects - one DataMapping for one output mapping.
    .evaluate = function(currVals) {
      # Iterate through the values and apply them to the parameter instances
      for (idx in seq_along(currVals)) {
        # The order of the values corresponds to the order of PIParameters in $parameters
        piParameter <- private$.parameters[[idx]]
        piParameter$setValue(currVals[[idx]])
      }
      
      singleRun <- function(simulation, configuration) {
        # Simulate steady-states if specified
        if (configuration$simulateSteadyState) {
          initialValues <- getSteadyState(
            quantitiesPaths = private$.stateVariables[[simulation$root$id]],
            simulation = simulation, steadyStateTime = configuration$steadyStateTime
          )
          for (i in seq_along(initialValues$quantities)) {
            quantity <- initialValues$quantities[[i]]
            quantity$value <- initialValues$values[[i]]
          }
        }
       
        simulationResult <- ospsuite::runSimulation(simulation)
       
        return(simulationResult)
        
      }

      # Run simulations
      simulationResults <- lapply(private$.simulations, function(x) {
        singleRun(x, private$.configuration)
      })

      
     

      # Create data mappings for each output mapping
      dataMappings <- lapply(private$.outputMappings, function(x) {
        # Find the simulation that is the parent of the output quantity
        simId <- getSimulationContainer(x$quantity)$id
        simulation <- private$.simulations[[simId]]
        # Create new DataMapping
        dataMapping <- DataMapping$new()
        # Add simulation results to the mapping.
       
        dataMapping$addModelOutputs(
         
          paths = x$quantity$path,
         
          outputValues = getOutputValues(
            simulationResults = simulationResults[[simId]],
            quantitiesOrPaths = x$quantity$path
            # addMetaData = TRUE
          ),
          labels = "Simulation",
          simulation = simulation,
          groups = "PI"
        )
    
        # Manipulate simulated results with a used defined function
        if (!is.null(x$transformResultsFunction)) {
          xyData <- dataMapping$xySeries[["Simulation"]]
          transformedResults <- x$transformResultsFunction(xyData$xValues, xyData$yValues)

          xyData$xValues <- transformedResults$xVals
          xyData$yValues <- transformedResults$yVals
        }

        # Add observed data
        for (observedData in x$observedXYData) {
          dataMapping$addOSPSTimeValues(
            OSPSTimeValues = observedData,
            groups = "PI"
          )
        }
        return(dataMapping)
      })
      return(dataMappings)
    },

    # Calculate residuals between simulated and observed values.
    #
    # @param dataMappingList A \code{DataMapping} or a list of \code{DataMapping} objects.
    # @param userFunction
    #
    # @return Vector of residuals
    .calculateResiduals = function(dataMappingList) {
      dataMappingList <- toList(dataMappingList)
      cost <- NULL
      for (dataMapping in dataMappingList) {
        simulatedResult <- list()

        combinedResults <- lapply(dataMapping$xySeries, function(xySeries) {
          if (xySeries$dataType == XYDataTypes$Simulated) {
            simulatedResult <<- xySeries
            return()
          }
          # Collapse all observed data
   
          return(list(
            dataPointsX = xySeries$xValuesProcessed(dataMapping$xDimension,dataMapping$xUnit),
            dataPointsY = xySeries$yValuesProcessed(dataMapping$yDimension,dataMapping$yUnit),
            dataError = xySeries$yErrorProcessed(dataMapping$yDimension,dataMapping$yUnit)
          ))
        })

        
        dataPointsX <- unlist(lapply(combinedResults, function(x) {
          x$dataPointsX
        }), use.names = FALSE)
        dataPointsY <- unlist(lapply(combinedResults, function(x) {
          x$dataPointsY
        }), use.names = FALSE)
        dataError <- unlist(lapply(combinedResults, function(x) {
          x$dataError
        }), use.names = FALSE)

        # Calculate the distance between each point of the observed data to the simulated result
        modelDf <- data.frame("Time" = simulatedResult$xValues, "Values" = simulatedResult$yValues)
        obsDf <- data.frame("Time" = dataPointsX, "Values" = dataPointsY)

        # Error is not used so far as the cost function is buggy - if the error is 0, it tries to divide by zero.
        cost <- FME::modCost(model = modelDf, obs = obsDf, x = "Time", cost = cost)
      }

      if (private$.configuration$printIterationFeedback) {
        print(cost)
      }

      return(cost)
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #'
    #' @param simulations An object or a list of objects of class \code{Simulation}.
    #' Parameters of the simulation object will be varied and the results simulated
    #' @param parameters An object or a list of objects of class \code{PIParameter}. These parameters will be varied.
    #' @param configuration Optional. Object of type \code{PIConfiguration} defining
    #' further options of the parameter identification. If no \code{PIConfiguration} is passed, a default one
    #' @param outputMappings List of objects of the class \code{PIOutputMapping}. Each objects
    #' maps a model output (represented by a \code{Quantity}) with a set of observed data given as \code{XYData} objects.
    #' is used.
    #' @return A new `ParameterIdentification` object.
    initialize = function(simulations, parameters, outputMappings, configuration = NULL) {
      validateIsOfType(simulations, "Simulation")
      validateIsOfType(parameters, "PIParameters")
      validateIsOfType(configuration, "PIConfiguration", nullAllowed = TRUE)
      validateIsOfType(outputMappings, "PIOutputMapping")
      private$.configuration <- configuration %||% PIConfiguration$new()
      private$.simulations <- hash::hash()

      for (simulation in c(simulations)) {
        id <- simulation$root$id
        private$.simulations[[id]] <- simulation
      }
      private$.parameters <- parameters
      private$.outputMappings <- c(outputMappings)
    },
    #' @description
    #' Clean up upon object removal
    finalize = function() {
      hash::clear(private$.simulations)
    },

    #' @description
    #' Start identification of parameters
    #' @details When the identification if finished, the best identified values of the parameters are accessible via the \code{currValue}-field of the \code{PIParameters}-object.
    #'
    #' @return Output of the PI algorithm. Depends on the selected algorithm.
    run = function() {
      # Prepare simulations
      # Clear output intervals of all simulations and only add points that are present in the observed data.
      # Also add output quantities.
      # If steady-state should be simulated, get the set of all state variables for each simulation
      private$.stateVariables <- lapply(private$.simulations, function(simulation) {
        simulation$outputSchema$clear()
        clearOutputs(simulation)
        if (private$.configuration$simulateSteadyState) {
          return(getAllStateVariables(simulation))
        }
        return()
      })

      for (outputMapping in private$.outputMappings) {
        simId <- getSimulationContainer(outputMapping$quantity)$id
        simulation <- private$.simulations[[simId]]
        ospsuite::addOutputs(quantitiesOrPaths = outputMapping$quantity, simulation = simulation)
        for (observedData in outputMapping$observedXYData) {
          xVals <- ospsuite::toBaseUnit(Dimensions$Time,
            values = (observedData$xValues + observedData$xOffset) * observedData$xFactor,
            unit = observedData$xUnit
          )
          simulation$outputSchema$addTimePoints(round(xVals))
        }
      }

      startValues <- unlist(lapply(self$parameters, function(x) {
        x$startValue
      }), use.names = FALSE)
      lower <- unlist(lapply(self$parameters, function(x) {
        x$minValue
      }), use.names = FALSE)
      upper <- unlist(lapply(self$parameters, function(x) {
        x$maxValue
      }), use.names = FALSE)

      results <- FME::modFit(f = private$.iterate, p = startValues, lower = lower, upper = upper, method = "Marq")
      return(results)
    },
    #for bobyqa add control:
# , control = list(rhobeg = 4e-6)
    #' Plot the current results
    #'
    #' @details Runs all simulations with current parameter values and creates plots of every output mapping
    plotCurrentResults = function() {
      parValues <- unlist(lapply(self$parameters, function(x) {
        x$currValue
      }), use.names = FALSE)
      dataMappings <- private$.evaluate(parValues)

      lapply(dataMappings, function(x) {
        x$plot()
      })

      invisible(self)
    },


    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Simulations", unlist(lapply(private$.simulations, function(x) {
        x$sourceFile
      }), use.names = FALSE))
      private$printLine("Number of parameters", length(private$.parameters))
      private$printLine("Simulate to steady-state", private$.configuration$simulateSteadyState)
      private$printLine("Steady-state time [min]", private$.configuration$steadyStateTime)
      private$printLine("Print feedback after each iteration", private$.configuration$printIterationFeedback)
      private$printLine("Execute in parallel", private$.configuration$parallelize)
      private$printLine("Maximal number of cores", private$.configuration$numberOfCores)
      invisible(self)
    }
  )
)
