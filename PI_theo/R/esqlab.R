
# esqlab fonctions for compatibility with ospsuite 9.1.4 (as initpksim)




`%||%` <- function (lhs, rhs) 
{
    if (!is.null(lhs)) {
        lhs
    }
    else {
        rhs
    }
}
# .__NAMESPACE__. <-
# <environment>
# .__S3MethodsTable__. <-
# <environment>
.onLoad <-
function (...) 
{
    options(warnPartialMatchDollar = TRUE)
    Sys.setenv(`_R_CHECK_LENGTH_1_CONDITION_` = "true")
}
# .packageName <-
# "esqlabsR"
.typeNamesFrom <-
function (type)
{
    type <- c(type)
    sapply(type, function(t) {
        if (is.character(t)) {
            return(t)
        }
        t$classname
    })
}
applyIndividualParameters <-
function (individualCharacteristics, simulation) 
{
    validateIsOfType(individualCharacteristics, "IndividualCharacteristics")
    validateIsOfType(simulation, "Simulation")
    individual <- createIndividual(individualCharacteristics)
    allParamPaths <- c(individual$distributedParameters$paths, 
        individual$derivedParameters$paths)
    allParamValues <- c(individual$distributedParameters$values, 
        individual$derivedParameters$values)
    condition <- function(p) {
        TRUE
    }
    if (individualCharacteristics$species == Species$Human) {
        condition <- function(p) {
            !p$isFormula
        }
    }
    setParameterValuesByPathWithCondition(parameterPaths = allParamPaths, 
        values = allParamValues, simulation = simulation, condition = condition)
}
calculateRMSE <-
function (dataMappingList) 
{
    if (typeof(dataMappingList) != "list") {
        dataMappingList <- list(dataMappingList)
    }
    error <- 0
    for (dataMapping in dataMappingList) {
        for (grouping in dataMapping$groupings) {
            dataPointsX <- c()
            dataPointsY <- c()
            simulatedResults <- list()
            for (dataName in grouping) {
                xySeries <- dataMapping$xySeries[[dataName]]
                if (xySeries$dataType == XYDataTypes$Simulated) {
                  simulatedResults <- append(simulatedResults, 
                    xySeries)
                  next
                }
                dataPointsX <- c(dataPointsX, xySeries$xValuesProcessed(dataMapping$xDimension, 
                  dataMapping$xUnit))
                dataPointsY <- c(dataPointsY, xySeries$yValuesProcessed(dataMapping$yDimension, 
                  dataMapping$yUnit))
            }
            for (simulatedResult in simulatedResults) {
                simulatedPointsX <- simulatedResult$xValuesProcessed(dataMapping$xDimension, 
                  dataMapping$xUnit)
                simulatedPointsY <- simulatedResult$yValuesProcessed(dataMapping$yDimension, 
                  dataMapping$yUnit)
                for (i in seq_along(dataPointsX)) {
                  idx <- getIndexClosestToValue(dataPointsX[[i]], 
                    (simulatedPointsX))
                  for (pointIdx in idx) {
                    error <- error + (simulatedPointsY[[pointIdx]] - 
                      dataPointsY[[i]])^2
                  }
                }
            }
        }
    }
    return(sqrt(error))
}
closeOutputDevice <-
function (plotConfiguration) 
{
    if (is.null(plotConfiguration$outputDevice)) {
        return()
    }
    if (enumHasKey(plotConfiguration$outputDevice, GraphicsDevices)) {
        dev.off()
    }
}
col2hsv <-
function (color) 
{
    validateIsString(color)
    rgb <- col2rgb(color)
    return(rgb2hsv(rgb))
}
compareSimulationParameters <-
function (simulation1, simulation2) 
{
    sim1Params <- getAllParametersMatching("**", simulation1)
    sim2Params <- getAllParametersMatching("**", simulation2)
    pathsIn1NotIn2 <- list()
    pathsIn2NotIn1 <- list()
    pathsDiff <- list()
    for (param1 in sim1Params) {
        path <- param1$path
        param2 <- getParameter(path, simulation2, stopIfNotFound = FALSE)
        if (is.null(param2)) {
            pathsIn1NotIn2 <- append(pathsIn1NotIn2, values = path)
            next
        }
        if (!isParametersEqual(param1, param2)) {
            pathsDiff <- append(pathsDiff, path)
        }
    }
    for (param2 in sim2Params) {
        path <- param2$path
        param1 <- getParameter(path, simulation1, stopIfNotFound = FALSE)
        if (is.null(param1)) {
            pathsIn2NotIn1 <- append(pathsIn2NotIn1, values = path)
            next
        }
    }
    return(list(In1NotIn2 = pathsIn1NotIn2, In2NotIn1 = pathsIn2NotIn1, 
        Different = pathsDiff))
}
compareWithNA <-
function (v1, v2) 
{
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
}
DataConfiguration <- R6::R6Class(
        "DataConfiguration",
        inherit = Printable,
        cloneable = FALSE,
        active = list(),
        public = list(
            dataFolder = "", 
            dataFile = "",
            compoundPropertiesFile = "Compound Properties.xlsx",
            dataSheets = NULL,
            columnsToSplitBy = "", 
            XValuesColumn = 0,
            YValuesColumn = 0,
            YErrorColumn = 0,
            initialize = function (dataFolder, dataFile, compoundPropertiesFile, dataSheets) 
            {
                self$dataFolder <- dataFolder
                self$dataFile <- dataFile
                self$compoundPropertiesFile <- compoundPropertiesFile
                self$dataSheets <- dataSheets
                self$columnsToSplitBy <- esqlabsEnv$columnsToSplitDataBy
                self$XValuesColumn <- esqlabsEnv$XValuesColumn
                self$YValuesColumn <- esqlabsEnv$YValuesColumn
                self$YErrorColumn <- esqlabsEnv$YErrorColumn
            },
            print = function(...) {
                private$printClass()
                private$printLine("Data file directory", self$dataFolder)
                private$printLine("Data file", self$dataFile)
                private$printLine("Sheets", self$dataSheets)
                private$printLine("CompoundProperties file", self$compoundPropertiesFile)
                private$printLine("Columns to split by", self$columnsToSplitBy)
                private$printLine("X values column", self$XValuesColumn)
                private$printLine("Y values column", self$YValuesColumn)
                private$printLine("Y error column", self$YErrorColumn)
                invisible(self)
            }
        )
    )
DataMapping <-
   R6::R6Class( 
        "DataMapping",
        inherit = Printable,
        cloneable = FALSE,
        public = list(
            log = "",
            title = "",
            addLegend = TRUE,
            legendPosition = "topright",
            initialize = function ()             {
                private$.xySeries <- list()
                private$.xySeriesGroupMap <- list()
                private$.groupings <- list()
                private$.emptyGrouping <- list()
            },
            addModelOutputs = function (paths, labels, outputValues, simulation, groups = NULL, removeNA = TRUE) 
            {
             
               validateIsString(c(paths, labels))
                validateIsSameLength(paths, labels)
                for (idx in seq_along(paths)) {
                    yValues <- outputValues$data[[paths[[idx]]]]
                    if (is.null(yValues)) {
                        stop(messages$errorOutputPathNotFound(paths[[idx]]))
                    }
                    xValues <- outputValues$data$Time
                    if (removeNA) {
                        naVals <- is.na(yValues)
                        yValues <- yValues[!naVals]
                        xValues <- xValues[!naVals]
                    }
                   
                    label <- labels[[idx]] %||% paths[[idx]]
                    timeValues <- OSPSTimeValues$new(xValues, yValues, label = label)
                    self$addOSPSTimeValues(timeValues, groups = groups[[idx]])
                    xySeries <- self$xySeries[[label]]
                    xySeries$type <- "l"
                    xySeries$dataType <- XYDataTypes$Simulated
                    xySeries$yDimension <- outputValues$metaData[[paths[[idx]]]]$dimension
                    xySeries$yUnit <- outputValues$metaData[[paths[[idx]]]]$unit
                    entity <- getQuantity(path = paths[[idx]], container = simulation)
                    mw <- NULL
                    if (entity$quantityType == "Drug") {
                        mw <- getParameter(path = paste(entity$name, "Molecular weight", 
                                                        sep = "|"), container = simulation, stopIfNotFound = F)
                    }
                    else if (entity$parentContainer$containerType == "Molecule") {
                        mw <- getParameter(path = paste(entity$parentContainer$name, 
                                                        "Molecular weight", sep = "|"), container = simulation, 
                                           stopIfNotFound = F)
                    }
                    if (!is.null(mw)) {
                        xySeries$MW <- toDisplayUnit(quantity = mw, values = mw$value)
                    }
                }
                invisible(self)
            },
            addXYSeries =  function (xValsList, yValsList, labels, yErrorList = NULL, groups = NULL) 
            {
                validateIsString(labels)
                if (typeof(xValsList) != "list") {
                    xValsList <- list(xValsList)
                }
                if (typeof(yValsList) != "list") {
                    yValsList <- list(yValsList)
                }
                if (!is.null(yErrorList) && typeof(yErrorList) != "list") {
                    yErrorList <- list(yErrorList)
                }
                validateIsSameLength(xValsList, yValsList, labels)
                for (idx in seq_along(labels)) {
                    xyData <- XYData$new(xVals = xValsList[[idx]], yVals = yValsList[[idx]], 
                                         yError = yErrorList[[idx]], label = labels[[idx]])
                    group <- groups[[idx]]
                    self$addOSPSTimeValues(xyData, group)
                }
                invisible(self)
            },
            addOSPSTimeValues =  function (OSPSTimeValues, groups = NULL) 
            {
                validateIsOfType(OSPSTimeValues, "XYData")
                if (typeof(OSPSTimeValues) != "list") {
                    OSPSTimeValues <- list(OSPSTimeValues)
                }
                for (idx in seq_along(OSPSTimeValues)) {
                    newGroupName <- groups[[idx]]
                    if (is.null(newGroupName)) {
                        newGroupName <- NA
                    }
                    label <- OSPSTimeValues[[idx]]$label
                    private$.xySeries <- mapPut(keys = label, values = c(OSPSTimeValues[[idx]]), 
                                                map = private$.xySeries, overwrite = TRUE)
                    if (mapHasKey(label, private$.xySeriesGroupMap)) {
                        if (compareWithNA(private$.xySeriesGroupMap[[label]], 
                                          newGroupName)) {
                            next
                        }
                        private$.removeLabelFromGroup(label = label, group = private$.xySeriesGroupMap[[label]])
                    }
                    if (is.na(newGroupName)) {
                        private$.xySeriesGroupMap <- mapPut(keys = label, 
                                                            values = newGroupName, map = private$.xySeriesGroupMap, 
                                                            overwrite = TRUE)
                        private$.emptyGrouping <- append(private$.emptyGrouping, 
                                                         label)
                        next
                    }
                    if (mapHasKey(newGroupName, private$.groupings)) {
                        private$.groupings[[newGroupName]] <- append(private$.groupings[[newGroupName]], 
                                                                     label)
                        private$.xySeriesGroupMap <- mapPut(keys = label, 
                                                            values = newGroupName, map = private$.xySeriesGroupMap, 
                                                            overwrite = TRUE)
                        next
                    }
                    private$.groupings <- mapPut(newGroupName, label, map = private$.groupings)
                    private$.xySeriesGroupMap <- mapPut(keys = label, values = newGroupName, 
                                                        map = private$.xySeriesGroupMap, overwrite = TRUE)
                }
            },
            removeXYSeries = function (label) 
            {
                if (!mapHasKey(label, private$.xySeries)) {
                    warning(messages$warningLabelNotInDataMapping(label))
                    return(invisible(self))
                }
                private$.xySeries <- mapRemove(keys = label, map = private$.xySeries)
                private$.removeLabelFromGroup(label = label, group = private$.xySeriesGroupMap[[label]])
                private$.xySeriesGroupMap[[label]] <- NULL
                invisible(self)
            }, 
            setXFactors =  function (labels, xFactors) 
            {
                validateIsString(labels, nullAllowed = TRUE)
                validateIsNumeric(xFactors, nullAllowed = TRUE)
                validateIsSameLength(labels, xFactors)
                for (idx in seq_along(labels)) {
                    xySeries <- self$xySeries[[labels[[idx]]]]
                    xySeries$xFactor <- xFactors[[idx]]
                }
                invisible(self)
            },
            setYFactors = function (labels, yFactors) 
            {
                validateIsString(labels, nullAllowed = TRUE)
                validateIsNumeric(yFactors, nullAllowed = TRUE)
                validateIsSameLength(labels, yFactors)
                for (idx in seq_along(labels)) {
                    xySeries <- self$xySeries[[labels[[idx]]]]
                    xySeries$yFactor <- yFactors[[idx]]
                }
                invisible(self)
            },
            setXOffset = function (labels, xOffset) 
            {
                validateIsString(labels, nullAllowed = TRUE)
                validateIsNumeric(xOffset, nullAllowed = TRUE)
                validateIsSameLength(labels, xOffset)
                for (idx in seq_along(labels)) {
                    xySeries <- self$xySeries[[labels[[idx]]]]
                    xySeries$xOffset <- xOffset[[idx]]
                }
                invisible(self)
            }, 
            setTypes = function (labels, types) 
            {
                validateIsString(c(labels, types), nullAllowed = TRUE)
                validateIsSameLength(labels, types)
                for (idx in seq_along(labels)) {
                    xySeries <- self$xySeries[[labels[[idx]]]]
                    xySeries$type <- types[[idx]]
                }
                invisible(self)
            },  
            setColors = function (labels, colors) 
            {
                validateIsString(c(labels), nullAllowed = TRUE)
                validateIsSameLength(labels, colors)
                for (idx in seq_along(labels)) {
                    xySeries <- self$xySeries[[labels[[idx]]]]
                    xySeries$color <- colors[[idx]]
                }
                invisible(self)
            }, 
            setConfiguration = function (dataMappingConfiguration) 
            {
                self$setXFactors(names(dataMappingConfiguration$xFactors), 
                                 dataMappingConfiguration$xFactors)
                self$setYFactors(names(dataMappingConfiguration$yFactors), 
                                 dataMappingConfiguration$yFactors)
                self$setXOffset(names(dataMappingConfiguration$xOffsets), 
                                dataMappingConfiguration$xOffsets)
                self$setTypes(names(dataMappingConfiguration$lineTypes), 
                              dataMappingConfiguration$lineTypes)
                self$setColors(names(dataMappingConfiguration$colors), dataMappingConfiguration$colors)
            }, 
            plot = function (lwd = esqlabsEnv$defaultLwd, ...) 
            {
                fun <- get(x = paste0("plot", self$plotType))
                fun(self, lwd = lwd, ...)
            }, 
            print = function (...) 
            {
                private$printClass()
                private$printLine("Plot type", self$plotType)
                private$printLine("Population quantiles", self$populationQuantiles)
                private$printLine("labels", mapKeys(private$.xySeries))
                private$printLine("X limits", self$xLim)
                private$printLine("Y limits", self$yLim)
                private$printLine("X label", self$xLab)
                private$printLine("Y label", self$yLab)
                private$printLine("X unit", self$xUnit)
                private$printLine("Y unit", self$yUnit)
                private$printLine("Title", self$title)
                private$printLine("Log axes", self$log)
                invisible(self)
            }
        ),
        active = list(
        xySeries = function (value) 
        {
            if (missing(value)) {
                private$.xySeries
            }
            else {
                stop(messages$errorPropertyReadOnly("xySeries"))
            }
        },
        xySeriesCount = function (value) 
        {
            if (missing(value)) {
                length(private$.xySeries)
            }
            else {
                stop(messages$errorPropertyReadOnly("xySeriesCount"))
            }
        },
        xLim = function (value) 
        {
            if (missing(value)) {
                if (is.null(private$.xLim)) {
                    if (length(self$xySeries) == 0) {
                        return(c(0, 0))
                    }
                    xMax <- max(sapply(self$xySeries, function(x) {
                        x$xMax * x$xUnitDimensionFactor(self$xDimension, 
                                                        self$xUnit)
                    }))
                    xMin <- min(sapply(self$xySeries, function(x) {
                        x$xMin * x$xUnitDimensionFactor(self$xDimension, 
                                                        self$xUnit)
                    }))
                    return(c(xMin * 0.9, xMax * 1.1))
                }
                else {
                    private$.xLim
                }
            }
            else {
                validateIsNumeric(value, nullAllowed = TRUE)
                private$.xLim <- value
            }
        }, 
        yLim = function (value) 
        {
            if (missing(value)) {
                if (is.null(private$.yLim)) {
                    if (length(self$xySeries) == 0) {
                        return(c(0, 0))
                    }
                    yMax <- max(sapply(self$xySeries, function(x) {
                        x$yMax * x$yUnitDimensionFactor(self$yDimension, 
                                                        self$yUnit)
                    }))
                    yMin <- min(sapply(self$xySeries, function(x) {
                        if (isCharInString("y", self$log)) {
                            x$yMinPositive() * x$yUnitDimensionFactor(self$yDimension, 
                                                                      self$yUnit)
                        } else {
                            x$yMin * x$yUnitDimensionFactor(self$yDimension, 
                                                            self$yUnit)
                        }
                    }))
                    return(c(yMin * 0.9, yMax * 1.1))
                }
                else {
                    private$.yLim
                }
            }
            else {
                validateIsNumeric(value, nullAllowed = TRUE)
                private$.yLim <- value
            }
        },  
        xLab = function (value) 
        {
            if (missing(value)) {
                private$.xLab
            }
            else {
                validateIsString(value)
                private$.xLab <- value
            }
        },
        yLab = function (value) 
        {
            if (missing(value)) {
                private$.yLab
            }
            else {
                validateIsString(value)
                private$.yLab <- value
            }
        },
        xDimension = function (value) 
        {
            if (missing(value)) {
                if (!is.null(private$.xDimension)) {
                    return(private$.xDimension)
                }
                if (self$xySeriesCount == 0) {
                    return(NULL)
                }
                return(self$xySeries[[1]]$xDimension)
            }
            else {
                validateDimension(value)
                private$.xDimension <- value
                self$xUnit <- getBaseUnit(value)
            }
        },
        yDimension = function (value) 
        {
            if (missing(value)) {
                if (!is.null(private$.yDimension)) {
                    return(private$.yDimension)
                }
                if (self$xySeriesCount == 0) {
                    return(NULL)
                }
                return(self$xySeries[[1]]$yDimension)
            }
            else {
                validateDimension(value)
                private$.yDimension <- value
                private$.yUnit <- getBaseUnit(value)
            }
        },
        xUnit = function (value) 
        {
            if (missing(value)) {
                if (!is.null(private$.xUnit)) {
                    return(private$.xUnit)
                }
                if (is.null(self$xDimension)) {
                    return(NULL)
                }
                return(getBaseUnit(self$xDimension))
            }
            else {
                validateUnit(value, dimension = self$xDimension)
                private$.xUnit <- value
            }
        }, 
        yUnit = function (value) 
        {
            if (missing(value)) {
                if (!is.null(private$.yUnit)) {
                    return(private$.yUnit)
                }
                if (is.null(self$yDimension)) {
                    return(NULL)
                }
                return(getBaseUnit(self$yDimension))
            }
            else {
                validateUnit(value, dimension = self$yDimension)
                private$.yUnit <- value
            }
        },
        groupings = function (value) 
        {
            if (missing(value)) {
                private$.groupings
            }
            else {
                stop(messages$errorPropertyReadOnly("groupings", 
                                                    optionalMessage = "Data sets are assigned to groupings when adding via `addModelOutputs'\n                                            or 'addXYSeries'."))
            }
        },
        ungroupedSeries = function (value) 
        {
            if (missing(value)) {
                private$.emptyGrouping
            }
            else {
                stop(messages$errorPropertyReadOnly("ungroupedSeries", 
                                                    optionalMessage = "Data sets are assigned to groupings when adding via `addModelOutputs'\n                                            or 'addXYSeries'."))
            }
        }, 
        plotType = function (value) 
        {
            if (missing(value)) {
                private$.plotType
            }
            else {
                validateEnumValue(enum = PlotTypes, value = value)
                private$.plotType <- value
            }
        },
        populationQuantiles =  function (value) 
        {
            if (missing(value)) {
                private$.populationQuantiles
            }
            else {
                validateIsNumeric(value)
                private$.populationQuantiles <- value
            }
        }
        ),
        private = list(
            .xySeries =  NULL,
            .xySeriesGroupMap = NULL,
            .xLim = NULL,
            .yLim = NULL,
            .xLab = NULL,
            .yLab = NULL,
            .xDimension = NULL,
            .yDimension = NULL,
            .xUnit = NULL,
            .yUnit = NULL,
            .groupings = NULL,
            .emptyGrouping = NULL,
            .plotType =  "IndividualProfile",
            .populationQuantiles = c(0.05, 0.5, 0.95),
            .removeLabelFromGroup = function (label, group) 
            {
                if (is.na(group)) {
                    private$.emptyGrouping <- removeFromList(label, private$.emptyGrouping)
                }
                else {
                    private$.groupings[[group]] <- removeFromList(label, 
                                                                  private$.groupings[[group]])
                    if (length(private$.groupings[[group]]) == 0) {
                        private$.groupings <- mapRemove(group, private$.groupings)
                    }
                }
            }   
        )
        )       
            
            
 DataMappingConfiguration <-   R6::R6Class( 
     "DataMappingConfiguration",
     inherit = Printable,
     cloneable = FALSE,
     public = list(
        initialize = function () 
        {
            private$.xFactors <- list()
            private$.yFactors <- list()
            private$.xOffsets <- list()
            private$.yOffsets <- list()
            private$.lineTypes <- list()
            private$.colors <- list()
        },
        setXFactors = function (outputNames, xFactors) 
        {
            private$.xFactors <- mapPut(outputNames, xFactors, map = private$.xFactors, 
                                        overwrite = TRUE)
        },
        setYFactors = function (outputNames, yFactors) 
        {
            private$.yFactors <- mapPut(outputNames, yFactors, map = private$.yFactors, 
                                        overwrite = TRUE)
        }, 
        setXOffsets = function (outputNames, xOffsets) 
        {
            private$.xOffsets <- mapPut(outputNames, xOffsets, map = private$.xOffsets, 
                                        overwrite = TRUE)
        },
        setYOffsets = function (outputNames, yOffsets) 
        {
            private$.yOffsets <- mapPut(outputNames, yOffsets, map = private$.yOffsets, 
                                        overwrite = TRUE)
        },
        setColors = function (outputNames, colors) 
        {
            private$.colors <- mapPut(outputNames, colors, map = private$.colors, 
                                      overwrite = TRUE)
        }, 
        setLineTypes = function (outputNames, lineTypes) 
        {
            private$.lineTypes <- mapPut(outputNames, lineTypes, map = private$.lineTypes, 
                                         overwrite = TRUE)
        },
        print = function (...) 
        {
            private$printClass()
            invisible(self)
        }
 ),
   active = list(
    xFactors = function (value) 
        {
            if (missing(value)) {
                private$.xFactors
            }
            else {
                stop(messages$errorPropertyReadOnly("xFactors", 
                                                    optionalMessage = "Use function 'setXFactors' to set the values."))
            }
        }, 
    yFactors = function (value) 
    {
        if (missing(value)) {
            private$.yFactors
        }
        else {
            stop(messages$errorPropertyReadOnly("yFactors", 
                                                optionalMessage = "Use function 'setYFactors' to set the values."))
        }
    },
    xOffsets = function (value) 
    {
        if (missing(value)) {
            private$.xOffsets
        }
        else {
            stop(messages$errorPropertyReadOnly("xOffsets", 
                                                optionalMessage = "Use function 'setXOffsets' to set the values."))
        }
    }, 
    yOffsets = function (value) 
    {
        if (missing(value)) {
            private$.yOffsets
        }
        else {
            stop(messages$errorPropertyReadOnly("yOffsets", 
                                                optionalMessage = "Use function 'setYOffsets' to set the values."))
        }
    },
    lineTypes = function (value) 
    {
        if (missing(value)) {
            private$.lineTypes
        }
        else {
            stop(messages$errorPropertyReadOnly("lineTypes", 
                                                optionalMessage = "Use function 'setLineTypes' to set the values."))
        }
    },
    colors = function (value) 
    {
        if (missing(value)) {
            private$.colors
        }
        else {
            stop(messages$errorPropertyReadOnly("colors", optionalMessage = "Use function 'setColors' to set the values."))
        }
    } 
   ),
 private = list(
     .xFactors = NULL,
     .yFactors = NULL,
     .xOffsets = NULL,
     .yOffsets = NULL,
     .lineTypes = NULL,
     .colors = NULL
 )
  )   
     
Dimensions <-
list(`Abundance per mass protein` = "Abundance per mass protein", 
    `Abundance per tissue` = "Abundance per tissue", `Age in weeks` = "Age in weeks", 
    `Age in years` = "Age in years", Amount = "Amount", `Amount per time` = "Amount per time", 
    Ampere = "Ampere", Area = "Area", `AUC (mass)` = "AUC (mass)", 
    `AUC (molar)` = "AUC (molar)", `AUCM (molar)` = "AUCM (molar)", 
    Becquerel = "Becquerel", BMI = "BMI", Candela = "Candela", 
    `CL per mass protein` = "CL per mass protein", `CL per recombinant enzyme` = "CL per recombinant enzyme", 
    Compliance = "Compliance", `Compliance (Area)` = "Compliance (Area)", 
    `Concentration (mass)` = "Concentration (mass)", `Concentration (molar)` = "Concentration (molar)", 
    `Concentration (molar) per time` = "Concentration (molar) per time", 
    Coulomb = "Coulomb", Count = "Count", `Count per mass` = "Count per mass", 
    `Count per volume` = "Count per volume", `CV mmHg*s?/ml` = "CV mmHg*s?/ml", 
    `CV Viscosity` = "CV Viscosity", `CV Viscosity per Volume` = "CV Viscosity per Volume", 
    Density = "Density", `Diffusion coefficient` = "Diffusion coefficient", 
    Dimensionless = "Dimensionless", `Dose per body surface area` = "Dose per body surface area", 
    `Dose per body weight` = "Dose per body weight", Elastance = "Elastance", 
    Energy = "Energy", Farad = "Farad", Flow = "Flow", `Flow per weight` = "Flow per weight", 
    `Flow per weight organ` = "Flow per weight organ", `Flow` = "Flow", 
    Fraction = "Fraction", Gray = "Gray", Henry = "Henry", Hertz = "Hertz", 
    `Hydraulic conductivity` = "Hydraulic conductivity", `Inversed concentration (molar)` = "Inversed concentration (molar)", 
    `Inversed length` = "Inversed length", `Inversed mol` = "Inversed mol", 
    `Inversed time` = "Inversed time", `Inversed volume` = "Inversed volume", 
    Joule = "Joule", Katal = "Katal", Kelvin = "Kelvin", Length = "Length", 
    `Log Units` = "Log Units", Lumen = "Lumen", Lux = "Lux", 
    Mass = "Mass", `Mass per area` = "Mass per area", `Mass per area per time` = "Mass per area per time", 
    `Mass per time` = "Mass per time", `Mass per tissue` = "Mass per tissue", 
    `Molecular weight` = "Molecular weight", Newton = "Newton", 
    Ohm = "Ohm", Pressure = "Pressure", Radian = "Radian", Resistance = "Resistance", 
    Resolution = "Resolution", RT = "RT", `Second order rate constant` = "Second order rate constant", 
    Siemens = "Siemens", Sievert = "Sievert", Slope = "Slope", 
    Steradian = "Steradian", Temperature = "Temperature", Tesla = "Tesla", 
    Time = "Time", Velocity = "Velocity", Viscosity = "Viscosity", 
    `Vmax per mass protein` = "Vmax per mass protein", `Vmax per recombinant enzyme` = "Vmax per recombinant enzyme", 
    `Vmax per transporter` = "Vmax per transporter", `Vmax per weight organ tissue` = "Vmax per weight organ tissue", 
    Volt = "Volt", Volume = "Volume", `Volume per body weight` = "Volume per body weight", 
    Watt = "Watt", Weber = "Weber")
dimensionsConversionFactor <-
function (dimension1, dimension2, mw = NULL) 
{
    validateIsString(c(dimension1, dimension2))
    if (dimension1 == dimension2) {
        return(1)
    }
    if (dimension1 == Dimensions$Dimensionless) {
        if (dimension2 == Dimensions$Fraction) {
            return(1)
        }
    }
    if (dimension1 == Dimensions$Fraction) {
        if (dimension2 == Dimensions$Dimensionless) {
            return(1)
        }
    }
    if (dimension1 == Dimensions$`Concentration (molar)`) {
        if (dimension2 == Dimensions$`Concentration (mass)`) {
            if (is.null(mw)) {
                stop(messages$errorCannotConvertDimensionsNoMW(dimension1, 
                  dimension2))
            }
            return(1 * 1e-06 * mw * 0.001)
        }
    }
    if (dimension1 == Dimensions$`Concentration (mass)`) {
        if (dimension2 == Dimensions$`Concentration (molar)`) {
            if (is.null(mw)) {
                stop(messages$errorCannotConvertDimensionsNoMW(dimension1, 
                  dimension2))
            }
            return(1 * 1000/mw * 1e+06)
        }
    }
    stop(messages$errorCannotConvertDimensions(dimension1, dimension2))
}
Distributions <-
list(Normal = "Normal", LogNormal = "LogNormal")
enum <-
function (enumValues) 
{
    myEnum <- as.list(enumValues)
    enumNames <- names(myEnum)
    if (is.null(enumNames)) {
        names(myEnum) <- myEnum
    }
    else if (any(enumNames == "")) {
        stop(messages$errorEnumNotAllNames)
    }
    return(myEnum)
}
enumGetKey <-
function (enum, value) 
{
    output <- names(which(enum == value))
    if (length(output) == 0) {
        return(NULL)
    }
    return(output)
}
enumGetValue <-
function (enum, key) 
{
    if (!enumHasKey(key, enum)) {
        stop(messages$errorkeyNotInEnum(key))
    }
    return(enum[[key]])
}
enumHasKey <-
function (key, enum) 
{
    return(any(enumKeys(enum) == key))
}
enumKeys <-
function (enum) 
{
    names(enum)
}
enumPut <-
function (keys, values, enum, overwrite = FALSE) 
{
    validateIsSameLength(keys, values)
    for (i in seq_along(keys)) {
        if (enumHasKey(keys[[i]], enum) && !overwrite) {
            stop(messages$errorKeyInEnumPresent(keys[[i]]))
        }
        enum[[keys[[i]]]] <- values[[i]]
    }
    return(enum)
}
enumPutList <-
function (key, values, enum, overwrite = FALSE) 
{
    if (length(key) > 1) {
        stop(messages$errorEnumPutListMultipleKeys())
    }
    if (enumHasKey(key, enum) && !overwrite) {
        stop(messages$errorKeyInEnumPresent(key))
    }
    enum[[key]] <- values
    return(enum)
}
enumRemove <-
function (keys, enum) 
{
    for (key in keys) {
        enum[[key]] <- NULL
    }
    return(enum)
}
enumValues <-
function (enum) 
{
    unlist(enum, use.names = FALSE)
}
escapeForRegex <-
function (string) 
{
    paste0("\\Q", string, "\\E")
}
esqLABS_colors <-
function (nrOfColors) 
{
    esqRed_hsv <- rgb2hsv(235, 23, 51, maxColorValue = 255)
    esqBlue_hsv <- rgb2hsv(13, 141, 218, maxColorValue = 255)
    esqGreen_hsv <- rgb2hsv(38, 176, 66, maxColorValue = 255)
    esq_palette <- c(hsv(esqBlue_hsv[1], esqBlue_hsv[2], esqBlue_hsv[3]), 
        hsv(esqRed_hsv[1], esqRed_hsv[2], esqRed_hsv[3]), hsv(esqGreen_hsv[1], 
            esqGreen_hsv[2], esqGreen_hsv[3]))
    deltaH_b_r <- (esqRed_hsv[1] - esqBlue_hsv[1])
    deltaS_b_r <- max(esqRed_hsv[2], esqBlue_hsv[2]) - min(esqRed_hsv[2], 
        esqBlue_hsv[2])
    deltaV_b_r <- max(esqRed_hsv[3], esqBlue_hsv[3]) - min(esqRed_hsv[3], 
        esqBlue_hsv[3])
    deltaH_r_g <- abs(esqRed_hsv[1] - (esqGreen_hsv[1] + 1))
    deltaS_r_g <- max(esqRed_hsv[2], esqGreen_hsv[2]) - min(esqRed_hsv[2], 
        esqGreen_hsv[2])
    deltaV_r_g <- max(esqRed_hsv[3], esqGreen_hsv[3]) - min(esqRed_hsv[3], 
        esqGreen_hsv[3])
    if (nrOfColors < 0) {
        stop("nrOfColors must be positive, value ", nrOfColors, 
            " is not valid!")
    }
    if (nrOfColors == 0) {
        return(c())
    }
    if (nrOfColors == 2) {
        palette <- c(esq_palette[1], esq_palette[3])
        return(palette)
    }
    if (nrOfColors <= 3) {
        palette <- esq_palette[1:nrOfColors]
        return(palette)
    }
    nrOfColorsToGenerate <- nrOfColors - 3
    palette <- esq_palette[1]
    nrOfColors_first <- nrOfColorsToGenerate%/%2 + nrOfColorsToGenerate%%2
    nrOfColors_second <- nrOfColorsToGenerate%/%2
    for (i in 1:nrOfColors_first) {
        deltaH <- deltaH_b_r/(nrOfColors_first + 1)
        deltaS <- deltaS_b_r/(nrOfColors_first + 1)
        deltaV <- deltaV_b_r/(nrOfColors_first + 1)
        h <- esqBlue_hsv[1] + deltaH * i
        if (h > 1) {
            h <- h - 1
        }
        s <- min(esqBlue_hsv[2], esqRed_hsv[2]) + deltaS * i
        v <- min(esqBlue_hsv[3], esqRed_hsv[3]) + deltaV * i
        palette <- c(palette, hsv(h, s, v))
    }
    palette <- c(palette, esq_palette[2])
    if (nrOfColors_second > 0) {
        for (i in 1:nrOfColors_second) {
            deltaH <- deltaH_r_g/(nrOfColors_second + 1)
            deltaS <- deltaS_r_g/(nrOfColors_second + 1)
            deltaV <- deltaV_r_g/(nrOfColors_second + 1)
            h <- esqRed_hsv[1] + deltaH * i
            if (h > 1) {
                h <- h - 1
            }
            s <- min(esqGreen_hsv[2], esqRed_hsv[2]) + deltaS * 
                i
            v <- min(esqGreen_hsv[3], esqRed_hsv[3]) + deltaV * 
                i
            palette <- c(palette, hsv(h, s, v))
        }
    }
    palette <- c(palette, esq_palette[3])
    return(palette)
}
# ici
esqlabsEnv <- new.env(parent = emptyenv())
esqlabsEnv$packageName <- "esqlabsR"
esqlabsEnv$maxNumberOfCores <- detectCores() - 1
esqlabsEnv$XValuesColumn <- 10
esqlabsEnv$YValuesColumn <- 11
esqlabsEnv$YErrorColumn <- 12  
esqlabsEnv$columnsToSplitDataBy <- "Group.Id"


executeInParallel <-
function (fun, firstArguments, exports = NULL, ..., outputNames = NULL, 
    nrOfCores = esqlabsEnv$maxNumberOfCores) 
{
    if (!is.null(outputNames)) {
        validateIsSameLength(firstArguments, outputNames)
    }
    cl <- makeCluster(nrOfCores)
    tmp <- clusterEvalQ(cl, library(esqlabsR))
    tmp <- clusterExport(cl, exports)
    result <- tryCatch({
        parLapply(cl = cl, X = firstArguments, fun = fun, ...)
    }, error = function(e) {
        stop(e)
    }, finally = {
        stopCluster(cl)
    })
    names(result) <- outputNames
    return(result)
}
existsDimension <-
function (dimension) 
{
    enumHasKey(dimension, enum = Dimensions)
}
exportPopulationCSV <-
function (population, pathToCSV) 
{
    validateIsOfType(population, "Population")
    validateIsString(pathToCSV)
    popDF <- populationAsDataFrame(population = population$population)
    write.table(popDF, file = pathToCSV, row.names = FALSE)
}
exportSteadyStateToXLS <-
function (simulation, quantities = NULL, resultsXLSPath = "", 
    steadyStateTime = 1000, ignoreIfFormula = TRUE, stopIfNotFound = TRUE, 
    lowerThreshold = 1e-15) 
{
    validateIsOfType(simulation, type = "Simulation")
    if (resultsXLSPath == "") {
        simulationPath <- file_path_sans_ext(simulation$sourceFile)
        resultsXLSPath <- paste0(simulationPath, "_SS.xlsx")
    }
    else {
        resultsDir <- dirname(resultsXLSPath)
        if (!file.exists(resultsDir)) {
            dir.create(resultsDir, recursive = TRUE)
        }
    }
    initialValues <- getSteadyState(simulation = simulation, 
        steadyStateTime = steadyStateTime, ignoreIfFormula = ignoreIfFormula, 
        stopIfNotFound = stopIfNotFound, lowerThreshold = lowerThreshold)
    nrOfEntries <- length(initialValues$quantities)
    moleculeContainerPath <- c()
    moleculeName <- c()
    moleculeIsPresent <- c()
    moleculeValue <- c()
    moleculeUnits <- c()
    moleculeScaleDivisor <- c()
    moleculeNegValsAllowed <- c()
    parameterContainerPath <- c()
    parameterName <- c()
    parameterValue <- c()
    parameterUnits <- c()
    for (i in 1:nrOfEntries) {
        quantity <- initialValues$quantities[[i]]
        value <- initialValues$values[[i]]
        if (isOfType(quantity, "Molecule")) {
            moleculeValue <- append(moleculeValue, value)
            moleculeContainerPath <- append(moleculeContainerPath, 
                quantity$parentContainer$path)
            moleculeName <- append(moleculeName, quantity$name)
            moleculeIsPresent <- append(moleculeIsPresent, TRUE)
            moleculeUnits <- append(moleculeUnits, quantity$unit)
            moleculeScaleDivisor <- append(moleculeScaleDivisor, 
                quantity$scaleDivisor)
            moleculeNegValsAllowed <- append(moleculeNegValsAllowed, 
                "")
        }
        else {
            parameterValue <- append(parameterValue, value)
            parameterContainerPath <- append(parameterContainerPath, 
                quantity$parentContainer$path)
            parameterName <- append(parameterName, quantity$name)
            parameterUnits <- append(parameterUnits, quantity$unit)
        }
    }
    speciesInitVals <- data.frame(unlist(moleculeContainerPath, 
        use.names = FALSE), unlist(moleculeName, use.names = FALSE), 
        unlist(moleculeIsPresent, use.names = FALSE), unlist(moleculeValue, 
            use.names = FALSE), unlist(moleculeUnits, use.names = FALSE), 
        unlist(moleculeScaleDivisor, use.names = FALSE), unlist(moleculeNegValsAllowed, 
            use.names = FALSE))
    if (length(speciesInitVals) > 0) {
        colnames(speciesInitVals) <- c("Container Path", "Molecule Name", 
            "Is Present", "Value", "Units", "Scale Divisor", 
            "Neg. Values Allowed")
    }
    parameterInitVals <- data.frame(unlist(parameterContainerPath, 
        use.names = FALSE), unlist(parameterName, use.names = FALSE), 
        unlist(parameterValue, use.names = FALSE), unlist(parameterUnits, 
            use.names = FALSE))
    if (length(parameterInitVals) > 0) {
        colnames(parameterInitVals) <- c("Container Path", "Parameter Name", 
            "Value", "Units")
    }
    write.xlsx(list(Molecules = speciesInitVals, Parameters = parameterInitVals), 
        resultsXLSPath, colNames = T)
}
extendPopulationByUserDefinedParams <-
function (population, parameterPaths, meanValues, sdValues, distributions = Distributions$Normal) 
{
    validateIsOfType(population, "Population")
    validateIsString(parameterPaths)
    validateIsNumeric(meanValues, sdValues)
    distributions <- distributions %||% rep(Distributions$Normal, 
        length(parameterPaths))
    validateIsSameLength(parameterPaths, meanValues, sdValues, 
        distributions)
    for (i in seq_along(parameterPaths)) {
        path <- parameterPaths[[i]]
        mean <- meanValues[[i]]
        sd <- sdValues[[i]]
        vals <- sampleRandomValue(distribution = distributions[[i]], 
            mean = mean, sd = sd, n = population$count)
        population$setParameterValues(parameterOrPath = path, 
            values = vals)
    }
}
extendPopulationFromXLS <-
function (population, XLSpath, sheet = NULL) 
{
    validateIsOfType(population, "Population")
    validateIsString(XLSpath)
    validateIsString(sheet, nullAllowed = TRUE)
    if (is.null(sheet)) {
        sheet <- 1
    }
    columnNames <- c("Container.Path", "Parameter.Name", "Mean", 
        "SD", "Distribution")
    data <- read.xlsx(xlsxFile = XLSpath, sheet = sheet)
    if (!(all(length(names(data)) == length(columnNames)) && 
        all(names(data) == columnNames))) {
        stop(messages$errorWrongPopCharXLSStructure)
    }
    paramPaths <- c(dim(data)[[1]])
    meanVals <- c(dim(data)[[1]])
    sdVals <- c(dim(data)[[1]])
    distributions <- c(dim(data)[[1]])
    for (i in seq_along(data$Container.Path)) {
        paramPath <- paste(data[["Container.Path"]][[i]], data[["Parameter.Name"]][[i]], 
            sep = "|")
        paramPaths[[i]] <- paramPath
        meanVals[[i]] <- data[["Mean"]][[i]]
        sdVals[[i]] <- data[["SD"]][[i]]
        distributions[[i]] <- data[["Distribution"]][[i]]
    }
    extendPopulationByUserDefinedParams(population = population, 
        parameterPaths = paramPaths, meanValues = meanVals, sdValues = sdVals, 
        distributions = distributions)
}
figureAddLabel <-
function (label, location = "topleft", offset = c(0, 0)) 
{
    coords <- switch(location, topleft = c(0.015, 0.98), topcenter = c(0.5525, 
        0.98), topright = c(0.985, 0.98), bottomleft = c(0.015, 
        0.02), bottomcenter = c(0.5525, 0.02), bottomright = c(0.985, 
        0.02), c(0.015, 0.98))
    this.x <- grconvertX(coords[1] + offset[1], from = "nfc", 
        to = "user")
    this.y <- grconvertY(coords[2] + offset[2], from = "nfc", 
        to = "user")
    text(labels = label[1], x = this.x, y = this.y, xpd = T)
}
foldChangeFunction <-
function (x, x_0, alpha = 1) 
{
    (x/x_0)^alpha
}
GenderInt <-
list(MALE = 1, FEMALE = 2, UNKNOWN = 3)
geomean <-
function (x, na.rm = FALSE, trim = 0) 
{
    exp(mean(log(x), na.rm = na.rm, trim = trim))
}
geosd <-
function (x, na.rm = FALSE) 
{
    exp(sd(log(x), na.rm = na.rm))
}
getAllStateVariables <-
function (simulation, ignoreIfFormula = TRUE) 
{
    validateIsOfType(simulation, type = "Simulation")
    quantities <- getAllMoleculesMatching("Organism|**", container = simulation)
    allParams <- getAllParametersMatching("**", container = simulation)
    rhsParams <- list()
    rhsParams <- lapply(allParams, function(param) {
        if (param$isStateVariable) {
            if (ignoreIfFormula && param$isFormula) {
                return()
            }
            return(param)
        }
        return()
    })
    quantities <- append(quantities, unlist(rhsParams, use.names = FALSE))
    return(quantities)
}
getBaseUnit <-
function (dimension) 
{
    validateDimension(dimension)
    dimensionTask <- getDimensionTask()
    dimension <- rClr::clrCall(dimensionTask, "DimensionByName", 
        enc2utf8(dimension))
    unit <- rClr::clrCall(dimension, "get_BaseUnit")
    rClr::clrCall(unit, "ToString")
}
getDimensionTask <-
function () 
{
    dimTask <- esqlabsEnv$DimensionTask
    if (is.null(dimTask)) {
        dimTask <- getNetTask("DimensionTask")
        esqlabsEnv$DimensionTask <- dimTask
    }
    return(dimTask)
}
getEsqlabsRSetting <-
function (settingName) 
{
    if (!(any(names(esqlabsEnv) == settingName))) {
        stop(messages$errorEsqlabsRSettingNotFound(settingName))
    }
    obj <- esqlabsEnv[[settingName]]
    if (is.function(obj)) {
        return(obj())
    }
    return(obj)
}
getIndexClosestToValue <-
function (value, array) 
{
    validateIsNumeric(c(value, array))
    idx <- which(abs(array - value) == min(abs(array - value)))
    return(idx)
}
getNetHashCode <-
function (netWrapper) 
{
    validateIsOfType(netWrapper, "DotNetWrapper")
    rClr::clrCall(netWrapper$ref, "GetHashCode")
}
getNetTask <-
function (taskName) 
{
    rClr::clrCallStatic("OSPSuite.R.Api", paste0("Get", taskName))
}
getQuantilesYData <-
function (xValues, yValues, quantiles = c(0.05, 0.5, 0.95)) 
{
    validateIsNumeric(c(xValues, yValues, quantiles))
    validateIsSameLength(xValues, yValues)
    output <- list()
    for (quantile in quantiles) {
        aggregatedData <- aggregate(yValues, by = list(xVals = xValues), 
            FUN = quantile, quantile)
        output[[as.character(quantile)]] <- list()
        output[[as.character(quantile)]][["xValues"]] <- aggregatedData$xVals
        output[[as.character(quantile)]][["yValues"]] <- aggregatedData[[2]]
    }
    return(output)
}
getSimulationContainer <-
function (entity) 
{
    validateIsOfType(entity, "Entity")
    if (isOfType(entity, "Container")) {
        if (entity$containerType == "Simulation") {
            return(entity)
        }
    }
    return(getSimulationContainer(entity$parentContainer))
}



getSteadyState <-
function (quantities = NULL, simulation, steadyStateTime, ignoreIfFormula = TRUE, 
    stopIfNotFound = TRUE, lowerThreshold = 1e-15) 
{
    validateIsOfType(simulation, type = "Simulation")
    validateIsOfType(object = quantities, type = "Quantity", 
        nullAllowed = TRUE)
    if (steadyStateTime <= 0) {
        stop("steadyStateTime must be > 0!")
    }
    oldOutputIntervals <- simulation$outputSchema$intervals
    oldTimePoints <- simulation$outputSchema$timePoints
    setOutputInterval(simulation = simulation, startTime = 0, 
        endTime = steadyStateTime, resolution = 1/steadyStateTime)
    if (is.null(quantities)) {
        quantities <- getAllStateVariables(simulation, ignoreIfFormula)
    }
    addOutputs(quantities, simulation)
    simulationResults <- runSimulation(simulation)
    allOutputs <- getOutputValues(simulationResults, quantitiesOrPaths = quantities, 
        stopIfNotFound = stopIfNotFound)
    endValues <- lapply(quantities, function(quantity) {
        if (ignoreIfFormula && quantity$isFormula) {
            return(NULL)
        }
        path <- quantity$path
        value <- tail(allOutputs$data[path][[1]], 1)
        if (is.na(value)) {
            return(NULL)
        }
        if (!is.null(lowerThreshold) && value < lowerThreshold) {
            value <- 0
        }
        return(value)
    })
    indices <- which(lengths(endValues) != 0)
    simulation$outputSchema$clear()
    for (outputInterval in oldOutputIntervals) {
        addOutputInterval(simulation = simulation, startTime = outputInterval$startTime$value, 
            endTime = outputInterval$endTime$value, resolution = outputInterval$resolution$value)
    }
    if (length(oldTimePoints) > 0) {
        simulation$outputSchema$addTimePoints(oldTimePoints)
    }
    return(list(quantities = quantities[indices], values = endValues[indices]))
}
getUnitConversionFactor <-
function (fromUnit, toUnit, dimension) 
{
    validateDimension(dimension)
    validateUnit(fromUnit, dimension)
    validateUnit(toUnit, dimension)
    dimensionTask <- getDimensionTask()
    dimension <- rClr::clrCall(dimensionTask, "DimensionByName", 
        enc2utf8(dimension))
    fromUnitObj <- rClr::clrCall(dimension, "UnitOrDefault", 
        enc2utf8(fromUnit))
    toUnitObj <- rClr::clrCall(dimension, "UnitOrDefault", enc2utf8(toUnit))
    convFac <- rClr::clrCall(dimension, "UnitValueToBaseUnitValue", 
        fromUnitObj, 1)
    convFac <- convFac * rClr::clrCall(dimension, "BaseUnitValueToUnitValue", 
        toUnitObj, 1)
    return(convFac)
}
GraphicsDevices <-
list(PNG = "PNG")
hasUnit <-
function (unit, dimension) 
{
    validateIsString(c(unit, dimension))
    validateDimension(dimension)
    dimensionTask <- getDimensionTask()
    dimension <- rClr::clrCall(dimensionTask, "DimensionByName", 
        enc2utf8(dimension))
    rClr::clrCall(dimension, "HasUnit", enc2utf8(unit))
}
hillFunction <-
function (x, Vmax, Km, alpha = 1) 
{
    Vmax * x^alpha/(x^alpha + Km^alpha)
}
initializeSimulation <-
function (simulation, individualCharacteristics = NULL, additionalParams = NULL, 
    simulateSteadyState = FALSE, steadyStateTime = 1000, ignoreIfFormula = TRUE) 
{
    validateIsOfType(simulation, "Simulation", nullAllowed = FALSE)
    validateIsOfType(individualCharacteristics, "IndividualCharacteristics", 
        nullAllowed = TRUE)
    validateIsLogical(simulateSteadyState)
    if (!is.null(individualCharacteristics)) {
        applyIndividualParameters(individualCharacteristics, 
            simulation)
    }
    if (!is.null(additionalParams)) {
        if (all(names(additionalParams) != c("paths", "values", 
            "units"))) {
            stop(messages$errorWrongAdditionalParams)
        }
        for (i in seq_along(additionalParams$paths)) {
            param <- getParameter(additionalParams$paths[[i]], 
                container = simulation)
            unit <- additionalParams$units[[i]]
            if (!is.na(unit)) {
                value <- toBaseUnit(quantity = param, values = additionalParams$values[[i]], 
                  unit = unit)
            }
            else {
                value <- additionalParams$values[[i]]
            }
            setParameterValues(param, value)
        }
    }
    if (simulateSteadyState) {
        initialValues <- getSteadyState(simulation = simulation, 
            steadyStateTime = steadyStateTime, ignoreIfFormula = ignoreIfFormula)
        for (i in seq_along(initialValues$quantities)) {
            quantity <- initialValues$quantities[[i]]
            quantity$value <- initialValues$values[[i]]
        }
    }
}
isCharInString <-
function (char, string) 
{
    any(unlist(strsplit(string, ""), use.names = FALSE) == char)
}
isLine <-
function (type) 
{
    isCharInString("l", type) || (type == "b")
}
isOfType <-
function (object, type) 
{
    if (is.null(object)) {
        return(FALSE)
    }
    type <- .typeNamesFrom(type)
    inheritType <- function(x) inherits(x, type)
    if (inheritType(object)) {
        return(TRUE)
    }
    object <- c(object)
    all(sapply(object, inheritType))
}
isParametersEqual <-
function (parameter1, parameter2, checkFormulaValues = FALSE) 
{
    validateIsOfType(c(parameter1, parameter2), "Parameter")
    if (parameter1$path != parameter2$path) {
        return(FALSE)
    }
    formula1 <- parameter1$formula
    formula2 <- parameter2$formula
    if (!all(c(formula1$isConstant, formula1$isDistributed, formula1$isExplicit, 
        formula1$isTable) == c(formula2$isConstant, formula2$isDistributed, 
        formula2$isExplicit, formula2$isTable))) {
        return(FALSE)
    }
    if (formula1$isConstant || formula1$isDistributed) {
        return(parameter1$value == parameter2$value)
    }
    if (parameter1$isFixedValue) {
        if (!parameter2$isFixedValue) {
            return(FALSE)
        }
        if (parameter1$value != parameter2$value) {
            return(FALSE)
        }
    }
    if (formula1$isExplicit) {
        if (checkFormulaValues && (parameter1$value != parameter2$value)) {
            return(FALSE)
        }
        return(formula1$formulaString == formula2$formulaString)
    }
    if (formula1$isTable) {
        return(isTableFormulasEqual(formula1, formula2))
    }
    return(FALSE)
}
isPoint <-
function (type) 
{
    isCharInString("p", type) || (type == "b")
}
isSameLength <-
function (...) 
{
    args <- list(...)
    nrOfLengths <- length(unique(lengths(args)))
    return(nrOfLengths == 1)
}
isTableFormulasEqual <-
function (formula1, formula2) 
{
    allPoints1 <- formula1$allPoints
    allPoints2 <- formula2$allPoints
    if (length(allPoints1) != length(allPoints2)) {
        return(FALSE)
    }
    for (i in seq_along(allPoints1)) {
        point1 <- allPoints1[[i]]
        point2 <- allPoints2[[i]]
        return((point1$x == point2$x) && (point1$y == point2$y))
    }
}
map <-
function (keys, values) 
{
    validateIsSameLength(keys, values)
    myMap <- as.list(values)
    names(myMap) <- keys
    return(myMap)
}
mapGetKey <-
function (map, value) 
{
    output <- names(which(map == value))
    if (length(output) == 0) {
        return(NULL)
    }
    return(output)
}
mapGetValue <-
function (map, key) 
{
    if (!mapHasKey(key, map)) {
        stop(messages$errorkeyNotInEnum(key))
    }
    return(map[[key]])
}
mapHasKey <-
function (key, map) 
{
    return(any(names(map) == key))
}
mapKeys <-
function (map) 
{
    names(map)
}
mapPut <-
function (keys, values, map, overwrite = FALSE) 
{
    validateIsSameLength(keys, values)
    for (i in seq_along(keys)) {
        if (mapHasKey(keys[[i]], map) && !overwrite) {
            stop(messages$errorKeyInEnumPresent(keys[[i]]))
        }
        map[[keys[[i]]]] <- values[[i]]
    }
    return(map)
}
mapPutList <-
function (key, values, map, overwrite = FALSE) 
{
    if (length(key) > 1) {
        stop(messages$errorEnumPutListMultipleKeys())
    }
    if (!overwrite && mapHasKey(key, map)) {
        stop(messages$errorKeyInEnumPresent(key))
    }
    map[[key]] <- values
    return(map)
}
mapRemove <-
function (keys, map) 
{
    for (key in keys) {
        map[[key]] <- NULL
    }
    return(map)
}
mapValues <-
function (map) 
{
    unlist(map, use.names = FALSE)
}
messages <-
list(errorWrongType = function (objectName, type, expectedType, 
    optionalMessage = NULL) 
{
    callingFunctions <- sys.calls()
    callingFunction <- sys.call(-length(callingFunctions) + 1)[[1]]
    expectedTypeMsg <- paste0(expectedType, collapse = ", or ")
    paste0(callingFunction, ": argument '", objectName, "' is of type '", 
        type, "', but expected '", expectedTypeMsg, "'!", optionalMessage)
}, errorDifferentLength = function (objectNames, optionalMessage = NULL) 
{
    callingFunctions <- sys.calls()
    callingFunction <- sys.call(-length(callingFunctions) + 1)[[1]]
    paste0(callingFunction, ": Arguments '", objectNames, "' must have the same length, but they don't!", 
        optionalMessage)
}, errorValueNotInEnum = function (enum, value) 
{
    paste0("Value '", value, "' is not in defined enumeration values: '", 
        paste0(enum, collapse = ", "), "'.")
}, errorWrongParamsXLSStructure = function (filePath, optinalMessage = NULL) 
{
    paste0("Loading parameter values from XLS failed, the file '", 
        filePath, "' has wrong structure!\n    The file should consist of columns 'Container Path', 'Parameter Name', 'Value', and 'Units'. ", 
        optionalMessage)
}, errorWrongPopCharXLSStructure = function (filePath, optinalMessage = NULL) 
{
    paste0("Loading population characteristics from XLS failed, the file ", 
        filePath, " has wrong structure!\n    The file should consist of columns'PopulationName', 'Species','Population','NrIndiv', '% female',\n    'Weight_min', 'Weight_max', 'Height_min', 'Height_max','Age_min', 'Age_max', 'BMI_min', 'BMI_max'. ", 
        optionalMessage)
}, errorWrongPopulationname = function (filePath, populationName, 
    optinalMessage = NULL) 
{
    paste0("Loading population characteristics from XLS file  '", 
        filePath, "' failed,\n    cannot find population with the name '", 
        populationName, "'! ", optionalMessage)
}, errorWrongAdditionalParams = function (optionalMessage = NULL) 
{
    paste0("Wrong argument 'additionalParams'! Must be a list containing lists 'paths', 'values', and 'units' ", 
        optionalMessage)
}, errorCouldNotCompareParameters = function (parameter1, parameter2, 
    optionalMessage = NULL) 
{
    paste0("Could not compare parameters with paths '", parameter1$path, 
        "' and '", parameter2$path, "'. ", optionalMessage)
}, errorKeyInEnumPresent = function (key, optionalMessage = NULL) 
{
    paste0("enum already contains the key '", key, "'! Use 'overwrite = TRUE' to overwrite the value. ", 
        optionalMessage)
}, errorkeyNotInEnum = function (key) 
{
    paste0("No value with the key '", key, "' is present in the enum!")
}, errorEnumPutListMultipleKeys = function () 
{
    paste0("Trying to put multiple keys, but only one key is allowed!")
}, errorPropertyReadOnly = function (propertyName, optionalMessage = NULL) 
{
    paste0("Property '$", propertyName, "' is readonly")
}, erroFileNotFound = function (filePath, optionalMessage = NULL) 
{
    paste0("File '", filePath, "' could not be found!")
}, errorDimensionNotSupported = function (dimension, optionalMessage = NULL) 
{
    paste0("Dimension '", dimension, "' is not supported! See enum Dimensions for the list of supported dimensions.")
}, errorUnitNotSupported = function (unit, dimension, optionalMessage = NULL) 
{
    paste0("Unit '", unit, "' is not supported by the dimension '", 
        dimension, "'!")
}, errorCannotConvertDimensions = function (dimension1, dimension2, 
    optionalMessage = NULL) 
{
    paste0("Cannot convert dimension '", dimension1, "' to dimension '", 
        dimension2, "'", optionalMessage)
}, errorCannotConvertDimensionsNoMW = function (dimension1, dimension2, 
    optionalMessage = NULL) 
{
    paste0("Cannot convert dimension '", dimension1, "' to dimension '", 
        dimension2, "' without a valid molecular weight!", optionalMessage)
}, errorEsqlabsRSettingNotFound = function (settingName) 
{
    paste0("No global setting with the name '", settingName, 
        "' exists. Available global settings are:\n", paste0(names(esqlabsEnv), 
            collapse = ", "))
}, errorDistributionNotSupported = function (string) 
{
    paste0("The distribution '", string, "' is not supported. Supported distributions are listed in `Distributions`.")
}, errorOutputPathNotFound = function (string) 
{
    paste0("The output with the path '", string, "' is not found.")
}, warningLabelNotInDataMapping = function (string) 
{
    paste0("No xy-series with label ", string, " exists in the DataMapping. Nothing to remove")
}, errorOneArgumentNullButNotBoth = function (name1, name2, optionalMessage = NULL) 
{
    paste0("Either both arugments ", name1, ", ", name2, " must be NULL or no, but\n           only one argument is NULL.", 
        optionalMessage)
})
openOuptutDevice <-
function (plotConfiguration, width, height) 
{
    if (is.null(plotConfiguration$outputDevice)) {
        return()
    }
    if (plotConfiguration$outputDevice == GraphicsDevices$PNG) {
        png(filename = file.path(plotConfiguration$outputFolder, 
            paste0(plotConfiguration$outputName, ".png")), width = width, 
            height = height, units = "cm", res = plotConfiguration$res, 
            pointsize = plotConfiguration$pointsize)
    }
}
# ici

# timeValues <- OSPSTimeValues$new(10, 10, 
#                                  label = "test", 
#                                  yError = 1)


OSPSTimeValues <-   R6::R6Class( 
    "OSPSTimeValues",
    inherit = XYData,
    cloneable = FALSE,
    public = list(
        StudyId = NULL,
        PatientId = NULL,
        Organ = NULL,
        Compartment = NULL,
        Species = NULL,
        Gender = NULL,
        Molecule = NULL,
        MW = NULL,
        GroupId = NULL,
    
        # ici  yUnitDimensionFactor = function (dimension, unit)
        yUnitDimensionFactor = function (dimension, unit, mw) 
        {
          super$yUnitDimensionFactor(dimension, unit, self$MW)
        },
        #ici  yErrorUnitDimensionFactor = function (dimension, unit) 
        yErrorUnitDimensionFactor = function (dimension, unit, mw) 
        {
          super$yErrorUnitDimensionFactor(dimension, unit, self$MW)
        },
        yValuesProcessed = function (dimension = NULL, unit = NULL, mw = NULL) 
        {
          super$yValuesProcessed(dimension, unit, self$MW)
        },
        yErrorProcessed = function (dimension = NULL, unit = NULL, mw = NULL) 
        {
          super$yErrorProcessed(dimension, unit, self$MW)
        },
        initialize = function (xVals, yVals, label, yError = NULL) 
        {
      
          super$initialize(xVals, yVals, label, yError)
        },
       
       print = function (...) 
        {
          super$print()
          private$printLine("Group Id", self$GroupId)
          private$printLine("Study Id", self$StudyId)
          private$printLine("Species", self$Species)
          private$printLine("Patient Id", self$PatientId)
          private$printLine("Organ", self$Organ)
          private$printLine("Compartment", self$Compartment)
          private$printLine("Gender", self$Gender)
          private$printLine("Molecule", self$Molecule)
          private$printLine("Molecular weight", self$MW)
          invisible(self)
        }
        ),
active = list()
)
    
    
pathFromClipboard <-
function (path = "clipboard") 
{
    y <- if (path == "clipboard") {
        readClipboard()
    }
    else {
        cat("Please enter the path:\n\n")
        readline()
    }
    x <- chartr("\\", "/", y)
    writeClipboard(x)
    return(x)
}

Plotable  <- R6::R6Class(
"Plotable",
inherit = Printable,
public = list(
  label = NULL,
  color = NULL,
  xFactor = 1,
  yFactor = 1,
  xOffset = 0,
  yOffset = 0,
  type = "p",
  pch = NULL,
  lty = NULL, 
  initialize = function (label) 
  {
    validateIsString(label)
    self$label <- label
  },
  print = function (...) 
  {
    private$printClass()
    private$printLine("label", self$label)
    private$printLine("x factor", self$xFactor)
    private$printLine("y factor", self$yFactor)
    private$printLine("x offset", self$xOffset)
    private$printLine("y offset", self$yOffset)
    private$printLine("Color", self$color)
    private$printLine("Type", self$type)
    private$printLine("pch", self$pch)
    private$printLine("lty", self$lty)
    invisible(self)
  }
),
active = list()
)


plotBoxPlot <-
function (dataMapping, ...) 
{
    validateIsOfType(dataMapping, "DataMapping")
    legendEntries <- vector(mode = "character", length = length(dataMapping$xySeries))
    allData <- vector(mode = "list", length = length(dataMapping$xySeries))
    for (i in seq_along(dataMapping$xySeries)) {
        legendEntries[i] <- paste0(i, ": ", dataMapping$xySeries[[i]]$label)
        allData[[i]] <- dataMapping$xySeries[[i]]$yValues
    }
    boxplot(allData, xlab = dataMapping$xLab, ylab = dataMapping$yLab, 
        main = dataMapping$title)
    if (dataMapping$addLegend) {
        legend(dataMapping$legendPosition, legend = legendEntries, 
            lty = rep(1, length(legendEntries)), ...)
    }
}
PlotConfiguration <-  R6::R6Class( 
    "PlotConfiguration",
    inherit = Printable,
    cloneable = FALSE,
    public = list(
        outputDevice = NULL,
        outputName = "",
        outputFolder = NULL,
        width = NULL,
        height = NULL,
        nrOfCols = NULL,
        res = 600,
        pointsize = 8,
        addTitle = TRUE,
        initialize = function () 
        {
        }, 
        print = function (...) 
        {
            private$printClass()
            private$printLine("Output device", self$outputDevice)
            private$printLine("Output name", self$outputName)
            private$printLine("Output path", self$outputPath)
            private$printLine("Width", self$width)
            private$printLine("Height", self$height)
            private$printLine("Number of columns", self$nrOfCols)
            private$printLine("Resulution", self$res)
            private$printLine("Point size", self$pointsize)
            private$printLine("addTitle", self$addTitle)
            invisible(self)
        } 
    ),
    active = list()
)
plotErrorBars <-
function (x, y, upper, lower = upper, length = par()$cin[[1]]/2, 
    axis = "y", ...) 
{
    validateIsNumeric(c(x, y))
    validateIsSameLength(x, y, upper, lower)
    if (axis == "y") {
        arrows(x, y + upper, x, y - lower, angle = 90, code = 3, 
            length = length, ...)
    }
    if (axis == "x") {
        arrows(x + upper, y, x - lower, y, angle = 90, code = 3, 
            length = length, ...)
    }
}
plotIndividualProfile <-
function (dataMapping, ...) 
{
    plotTimeValues(dataMapping, aggregated = FALSE, ...)
}
plotMultiPanel <-
function (dataMappingList, plotConfiguration, ...) 
{
    if (typeof(dataMappingList) != "list") {
        dataMappingList <- list(dataMappingList)
    }
    nrOfCols <- plotConfiguration$nrOfCols
    if (is.null(nrOfCols)) {
        nrOfCols <- ceiling(sqrt(length(dataMappingList)))
    }
    nrOfRows <- ceiling(length(dataMappingList)/nrOfCols)
    width <- plotConfiguration$width %||% esqlabsEnv$widthPerPlotMapping * 
        nrOfCols
    height <- plotConfiguration$height %||% esqlabsEnv$heightPerPlotMapping * 
        nrOfRows
    openOuptutDevice(plotConfiguration, width, height)
    split.screen(c(nrOfRows, nrOfCols))
    for (idx in seq_along(dataMappingList)) {
        screen(idx)
        dataMappingList[[idx]]$plot(cex = 1 - 0.1 * (nrOfRows - 
            1), ...)
        if (length(dataMappingList) > 1) {
            figureAddLabel(letters[[idx]], offset = c(0, -0.1))
        }
    }
    if (plotConfiguration$addTitle) {
        title(plotConfiguration$outputName, outer = TRUE, line = -1, 
            cex = 1.2)
    }
    close.screen(all.screens = TRUE)
    closeOutputDevice(plotConfiguration)
}
plotPopulationQuantiles <-
function (dataMapping, ...) 
{
    plotTimeValues(dataMapping, aggregated = TRUE, ...)
}
plotPredictedVsObserved <-
function (dataMapping, foldDistance = 2, ...) 
{
    validateIsOfType(dataMapping, "DataMapping")
    legendEntries <- c()
    legendColors <- c()
    legendPch <- c()
    nrOfEntries <- length(mapKeys(dataMapping$groupings)) + length(dataMapping$ungroupedSeries)
    colors <- esqLABS_colors(nrOfColors = nrOfEntries)
    pchArr <- 1:dataMapping$xySeriesCount
    ltyArr <- 1:nrOfEntries
    graphicsParIdx <- 1
    if (isCharInString("y", dataMapping$log) && !all(dataMapping$yLim > 
        0)) {
        dataMapping$yLim <- NULL
    }
    plot(NULL, NULL, xlim = dataMapping$yLim, ylim = dataMapping$yLim, 
        xlab = "Observed values", ylab = "Simulated values", 
        log = if (isCharInString("y", dataMapping$log)) {
            "xy"
        }
        else {
            ""
        }, main = dataMapping$title, ...)
    points(dataMapping$yLim, dataMapping$yLim, type = "l")
    points(dataMapping$yLim, dataMapping$yLim * 1/foldDistance, 
        type = "l", lty = 2)
    points(dataMapping$yLim, dataMapping$yLim * foldDistance, 
        type = "l", lty = 2)
    graphicsParIdx <- 1
    for (groupingIdx in seq_along(dataMapping$groupings)) {
        grouping <- dataMapping$groupings[[groupingIdx]]
        dataPointsX <- c()
        dataPointsY <- c()
        dataErrorY <- c()
        simulatedResults <- list()
        for (dataName in grouping) {
            xySeries <- dataMapping$xySeries[[dataName]]
            if (xySeries$dataType == XYDataTypes$Simulated) {
                simulatedResults <- append(simulatedResults, 
                  xySeries)
                next
            }
            dataPointsX <- c(dataPointsX, xySeries$xValuesProcessed(dataMapping$xDimension, 
                dataMapping$xUnit))
            dataPointsY <- c(dataPointsY, xySeries$yValuesProcessed(dataMapping$yDimension, 
                dataMapping$yUnit))
        }
        for (simulatedResult in simulatedResults) {
            simulatedPointsX <- simulatedResult$xValuesProcessed(dataMapping$xDimension, 
                dataMapping$xUnit)
            simulatedPointsY <- simulatedResult$yValuesProcessed(dataMapping$yDimension, 
                dataMapping$yUnit)
            for (i in seq_along(dataPointsX)) {
                idx <- getIndexClosestToValue(dataPointsX[[i]], 
                  (simulatedPointsX))
                for (pointIdx in idx) {
                  points(dataPointsY[[i]], simulatedPointsY[[pointIdx]], 
                    pch = pchArr[[graphicsParIdx]], col = colors[[graphicsParIdx]])
                }
            }
        }
        legendEntries <- c(legendEntries, mapKeys(map = dataMapping$groupings)[[groupingIdx]])
        legendColors <- c(legendColors, colors[[graphicsParIdx]])
        legendPch <- c(legendPch, pchArr[[graphicsParIdx]])
        graphicsParIdx <- graphicsParIdx + 1
    }
    if (dataMapping$addLegend) {
        legend(dataMapping$legendPosition, legend = legendEntries, 
            col = legendColors, pch = legendPch, ...)
    }
}
plotTimeValues <-
function (dataMapping, aggregated, ...) 
{
    validateIsOfType(dataMapping, "DataMapping")
    legendEntries <- c()
    legendColors <- c()
    legendLty <- c()
    legendPch <- c()
    nrOfEntries <- length(mapKeys(dataMapping$groupings)) + length(dataMapping$ungroupedSeries)
    colors <- esqLABS_colors(nrOfColors = nrOfEntries)
    pchArr <- 1:dataMapping$xySeriesCount
    ltyArr <- 1:nrOfEntries
    graphicsParIdx <- 1
    if (isCharInString("y", dataMapping$log) && !all(dataMapping$yLim > 
        0)) {
        dataMapping$yLim <- NULL
    }
    plot(NULL, NULL, xlim = dataMapping$xLim, ylim = dataMapping$yLim, 
        xlab = dataMapping$xLab, ylab = dataMapping$yLab, log = dataMapping$log, 
        main = dataMapping$title, ...)
    updateLegend <- function(legendEntry) {
        legendEntries <<- c(legendEntries, legendEntry)
        legendColors <<- c(legendColors, colors[[graphicsParIdx]])
        if (isPch) {
            legendPch <<- c(legendPch, pchArr[[pchParIdx]])
        }
        else {
            legendPch <<- c(legendPch, NA)
        }
        if (isLty) {
            legendLty <<- c(legendLty, ltyArr[[graphicsParIdx]])
        }
        else {
            legendLty <<- c(legendLty, 0)
        }
    }
    pchParIdx <- 0
    for (i in seq_along(dataMapping$groupings)) {
        isPch <- FALSE
        isLty <- FALSE
        for (j in seq_along(dataMapping$groupings[[i]])) {
            pchParIdx <- pchParIdx + 1
            xySeriesEntry <- dataMapping$xySeries[[dataMapping$groupings[[i]][[j]]]]
            resetPch <- FALSE
            resetLty <- FALSE
            resetColor <- FALSE
            if (is.null(xySeriesEntry$lty)) {
                xySeriesEntry$lty <- ltyArr[[graphicsParIdx]]
                resetLty <- TRUE
            }
            if (is.null(xySeriesEntry$pch)) {
                xySeriesEntry$pch <- pchArr[[pchParIdx]]
                resetPch <- TRUE
            }
            if (is.null(xySeriesEntry$color)) {
                xySeriesEntry$color <- colors[[graphicsParIdx]]
                resetColor <- TRUE
            }
            isPch <- isPoint(xySeriesEntry$type)
            isLty <- isLine(xySeriesEntry$type)
            if (xySeriesEntry$dataType == XYDataTypes$Simulated && 
                aggregated) {
                plotXYDataAggregated(xySeriesEntry, dataMapping$xDimension, 
                  dataMapping$xUnit, dataMapping$yDimension, 
                  dataMapping$yUnit, quantiles = dataMapping$populationQuantiles, 
                  ...)
            }
            else {
                plotXYData(xySeriesEntry, dataMapping$xDimension, 
                  dataMapping$xUnit, dataMapping$yDimension, 
                  dataMapping$yUnit, ...)
            }
            if (resetPch) {
                xySeriesEntry$pch <- NULL
            }
            if (resetLty) {
                xySeriesEntry$lty <- NULL
            }
            if (resetColor) {
                xySeriesEntry$color <- NULL
            }
        }
        updateLegend(mapKeys(dataMapping$groupings)[[i]])
        graphicsParIdx <- graphicsParIdx + 1
    }
    if (pchParIdx == 0) {
        pchParIdx <- 1
    }
    for (xySeriesName in dataMapping$ungroupedSeries) {
        xySeriesEntry <- dataMapping$xySeries[[xySeriesName]]
        resetPch <- FALSE
        resetLty <- FALSE
        resetColor <- FALSE
        if (is.null(xySeriesEntry$lty)) {
            xySeriesEntry$lty <- ltyArr[[graphicsParIdx]]
            resetLty <- TRUE
        }
        if (is.null(xySeriesEntry$pch)) {
            xySeriesEntry$pch <- pchArr[[pchParIdx]]
            resetPch <- TRUE
        }
        if (is.null(xySeriesEntry$color)) {
            xySeriesEntry$color <- colors[[graphicsParIdx]]
            resetColor <- TRUE
        }
        isPch <- isPoint(xySeriesEntry$type)
        isLty <- isLine(xySeriesEntry$type)
        if (xySeriesEntry$dataType == XYDataTypes$Simulated && 
            aggregated) {
            plotXYDataAggregated(xySeriesEntry, xySeriesEntry$xUnitDimensionFactor(dataMapping$xDimension, 
                dataMapping$xUnit), xySeriesEntry$yUnitDimensionFactor(dataMapping$yDimension, 
                dataMapping$yUnit), quantiles = dataMapping$populationQuantiles, 
                ...)
        }
        else {
            plotXYData(xySeriesEntry, xySeriesEntry$xUnitDimensionFactor(dataMapping$xDimension, 
                dataMapping$xUnit), xySeriesEntry$yUnitDimensionFactor(dataMapping$yDimension, 
                dataMapping$yUnit), ...)
        }
        if (resetPch) {
            xySeriesEntry$pch <- NULL
        }
        if (resetLty) {
            xySeriesEntry$lty <- NULL
        }
        if (resetColor) {
            xySeriesEntry$color <- NULL
        }
        isPch <- isPoint(xySeriesEntry$type)
        isLty <- isLine(xySeriesEntry$type)
        updateLegend(xySeriesName)
        graphicsParIdx <- graphicsParIdx + 1
        pchParIdx <- graphicsParIdx
    }
    if (dataMapping$addLegend && (length(legendEntries) > 0)) {
        legend(dataMapping$legendPosition, legend = legendEntries, 
            col = legendColors, lty = legendLty, pch = legendPch)
    }
}
PlotTypes <-
list(IndividualProfile = "IndividualProfile", PopulationQuantiles = "PopulationQuantiles", 
    PredictedVsObserved = "PredictedVsObserved", BoxPlot = "BoxPlot")
plotXYData <-
function (xySeries, xDimension = NULL, xUnit = NULL, yDimension = NULL, 
    yUnit = NULL, aggregated = FALSE, ...) 
{
    validateIsOfType(xySeries, "XYData")
    points(xySeries$xValuesProcessed(xDimension, xUnit), xySeries$yValuesProcessed(yDimension, 
        yUnit), type = xySeries$type, lty = xySeries$lty, pch = xySeries$pch, 
        col = xySeries$color, ...)
    if (!is.null(xySeries$yError)) {
        plotErrorBars(xySeries$xValuesProcessed(xDimension, xUnit), 
            xySeries$yValuesProcessed(yDimension, yUnit), xySeries$yErrorProcessed(yDimension, 
                yUnit), col = xySeries$color, ...)
    }
}
plotXYDataAggregated <-
function (xySeries, xDimension = NULL, xUnit = NULL, yDimension = NULL, 
    yUnit = NULL, quantiles = c(0.05, 0.5, 0.95), ...) 
{
    validateIsOfType(xySeries, "XYData")
    aggregatedData <- getQuantilesYData(xValues = xySeries$xValuesProcessed(xDimension, 
        xUnit), yValues = xySeries$yValuesProcessed(yDimension, 
        yUnit), quantiles = quantiles)
    polygon(c(aggregatedData[[3]]$xValues, rev(aggregatedData[[1]]$xValues)), 
        c(aggregatedData[[3]]$yValues, rev(aggregatedData[[1]]$yValues)), 
        col = paste0(xySeries$color, "80"), border = 0)
    points(aggregatedData[[2]]$xValues, aggregatedData[[2]]$yValues, 
        type = xySeries$type, lty = xySeries$lty, pch = xySeries$pch, 
        col = xySeries$color, ...)
}


Printable <-  R6::R6Class( 
    "Printable",
    private = list(
        printLine = function (entry, value = NULL) 
        {
            entries <- c("  ", entry)
            if (!is.null(value)) {
                entries <- c(entries, ": ", value)
            }
            entries <- c(entries, "\n")
            cat(entries, sep = " ")
            invisible(self)
        },
    printClass = function () 
    {
        cat(class(self)[1], ": \n", sep = "")
    }
    )
)
readOSPSTimeValues <-
  
  function (dataConfiguration) 
  {
    validateIsString(c(dataConfiguration$dataFolder, 
                                  dataConfiguration$dataFile, dataConfiguration$sheets))
    filePath <- file.path(dataConfiguration$dataFolder, dataConfiguration$dataFile)
    validateFileExists(filePath)
    observedData <- list()
    for (sheet in dataConfiguration$dataSheets) {
      data <- openxlsx::read.xlsx(xlsxFile = filePath, sheet = sheet)
      allFactors <- list()
      groupings <- c()
      for (columnName in dataConfiguration$columnsToSplitBy) {
        if (length(data[[columnName]]) > 0 && all(!is.na(data[[columnName]]))) {
          groupings <- c(groupings, columnName)
          allFactors <- append(allFactors, list(data[[columnName]]))
        }
      }
      data <- split(data, allFactors, drop = TRUE)
      for (groupIdx in seq_along(data)) {
        group <- data[[groupIdx]]
        groupName <- names(data)[[groupIdx]]
        xVals <- group[[dataConfiguration$XValuesColumn]]
        yVals <- group[[dataConfiguration$YValuesColumn]]
        yErrorVals <- group[[dataConfiguration$YErrorColumn]]
        xName <- colnames(group)[[dataConfiguration$XValuesColumn]]
        yName <- colnames(group)[[dataConfiguration$YValuesColumn]]
        yErrorName <- colnames(group)[[dataConfiguration$YErrorColumn]]
        xDim <- gsub(pattern = ".", replacement = " ", 
                     strsplit(xName, "\\.?\\[")[[1]][[1]], fixed = TRUE)
        xUnit <- gsub(pattern = ".", replacement = " ", 
                      strsplit(xName, "\\.?\\[")[[1]][[2]], fixed = TRUE)
        xUnit <- gsub(pattern = "]", replacement = "", 
                      xUnit, fixed = TRUE)
        yDim <- gsub(pattern = ".", replacement = " ", 
                     strsplit(yName, "\\.?\\[")[[1]][[1]], fixed = TRUE)
        yUnit <- gsub(pattern = ".", replacement = " ", 
                      strsplit(yName, "\\.?\\[")[[1]][[2]], fixed = TRUE)
        yUnit <- gsub(pattern = "]", replacement = "", 
                      yUnit, fixed = TRUE)
        yErrorUnit <- gsub(pattern = ".", replacement = " ", 
                           strsplit(yErrorName, "\\.?\\[")[[1]][[2]], 
                           fixed = TRUE)
        yErrorUnit <- gsub(pattern = "]", replacement = "", 
                           yErrorUnit, fixed = TRUE)
        timeValues <- OSPSTimeValues$new(stringToNum(xVals), 
                                         stringToNum(yVals), label = paste(sheet, groupName, 
                                                                           sep = "."), yError = stringToNum(yErrorVals))
        timeValues$xDimension <- xDim
        timeValues$xUnit <- xUnit
        timeValues$yDimension <- yDim
        timeValues$yUnit <- yUnit
        timeValues$yErrorUnit <- yErrorUnit
        timeValues$StudyId <- group$Study.Id[[1]]
        timeValues$PatientId <- group$PatientId[[1]]
        timeValues$Organ <- group$Organ[[1]]
        timeValues$Compartment <- group$Compartment[[1]]
        timeValues$Species <- group$Species[[1]]
        timeValues$Gender <- group$Gender[[1]]
        timeValues$Molecule <- group$Molecule[[1]]
        if (!is.na(timeValues$Molecule)) {
          compoundProperties <- openxlsx::read.xlsx(xlsxFile = file.path(dataConfiguration$dataFolder, 
                                                                         dataConfiguration$compoundPropertiesFile), 
                                                    sheet = timeValues$Molecule)
          mwIdx <- which(compoundProperties$`Parameter,.[AdditionalParameter]` == 
                           "MW")
          mw <- compoundProperties$`Value.[1,1]`[[mwIdx]]
          unit <- compoundProperties$`Unit.[1,1]`[[mwIdx]]
          timeValues$MW <- as.numeric(mw)
        }
        timeValues$GroupId <- group$GroupId[[1]]
        timeValues$dataType <- XYDataTypes$Observed
        levelString <- unlist(lapply(groupings, function(x) {
          group[[x]][[1]]
        }), use.names = FALSE)
        levelString <- paste0("'", levelString, "'", 
                              collapse = "$")
        evalString <- paste0("observedData[[sheet]]$", 
                             levelString, " <- timeValues")
        eval(parse(text = evalString))
      }
    }
    return(observedData)
  }

readParametersFromXLS <-
function (paramsXLSpath, sheets = NULL) 
{
    columnNames <- c("Container.Path", "Parameter.Name", "Value", 
        "Units")
    validateIsString(paramsXLSpath)
    validateIsString(sheets, nullAllowed = TRUE)
    if (is.null(sheets)) {
        sheets <- c(1)
    }
    paths <- c()
    values <- c()
    units <- c()
    for (sheet in sheets) {
        data <- read.xlsx(xlsxFile = paramsXLSpath, sheet = sheet)
        if (!all(names(data) == columnNames)) {
            stop(messages$errorWrongParamsXLSStructure(paramsXLSpath))
        }
        for (i in seq_along(data[["Container.Path"]])) {
            path <- paste(data[["Container.Path"]][[i]], data[["Parameter.Name"]][[i]], 
                sep = "|")
            value <- data[["Value"]][[i]]
            unit <- data[["Units"]][[i]]
            idx <- match(path, paths)
            if (is.na(idx)) {
                paths <- c(paths, path)
                values <- c(values, value)
                units <- c(units, unit)
            }
            else {
                values[[idx]] <- value
                units[[idx]] <- unit
            }
        }
    }
    return(list(paths = paths, values = values, units = units))
}
readPopulationCharacteristicsFromXLS <-
function (XLSpath, populationName, sheet = NULL) 
{
    columnNames <- c("PopulationName", "species", "population", 
        "numberOfIndividuals", "proportionOfFemales", "weightMin", 
        "weightMax", "weightUnit", "heightMin", "heightMax", 
        "heightUnit", "ageMin", "ageMax", "BMIMin", "BMIMax", 
        "BMIUnit")
    validateIsString(c(XLSpath, populationName))
    validateIsString(sheet, nullAllowed = TRUE)
    if (is.null(sheet)) {
        sheet <- 1
    }
    data <- read.xlsx(xlsxFile = XLSpath, sheet = sheet)
    if (!(all(length(names(data)) == length(columnNames)) && 
        all(names(data) == columnNames))) {
        stop(messages$errorWrongPopCharXLSStructure)
    }
    rowIdx <- which(data$PopulationName == populationName)
    if (length(rowIdx) == 0) {
        stop(messages$errorWrongPopulationname())
    }
    arguments <- list()
    for (i in 2:length(data[rowIdx, ])) {
        value <- data[rowIdx, i]
        if (is.na(value)) {
            next
        }
        name <- names(data[rowIdx, ][i])
        arguments[[name]] <- value
    }
    populationCharacterstics <- do.call(createPopulationCharacteristics, 
        arguments)
    return(populationCharacterstics)
}
removeFromList <-
function (entry, listArg) 
{
    if (!is.list(listArg)) {
        listArg <- list(listArg)
    }
    idx <- which(entry == listArg)
    listArg[idx] <- NULL
    return(listArg)
}
sampleRandomValue <-
function (distribution, mean, sd, n) 
{
    if (!enumHasKey(distribution, Distributions)) {
        stop(messages$errorDistributionNotSupported(distribution))
    }
    if (distribution == Distributions$Normal) {
        return(rnorm(n, mean, sd))
    }
    if (distribution == Distributions$LogNormal) {
        location <- log(mean^2/sqrt(sd^2 + mean^2))
        shape <- sqrt(log(1 + (sd^2/mean^2)))
        vals <- rlnorm(n = n, meanlog = location, sdlog = shape)
        return(vals)
    }
    return(NULL)
}
setParameterValuesByPathWithCondition <-
function (parameterPaths, values, simulation, condition = function(p) {
    TRUE
}, units = NULL) 
{
    validateIsString(c(parameterPaths, units))
    validateIsNumeric(values)
    validateIsOfType(simulation, "Simulation")
    for (i in seq_along(parameterPaths)) {
        param <- getParameter(parameterPaths[[i]], simulation)
        if (condition(param)) {
            setParameterValuesByPathWithUnit(parameterPaths = parameterPaths[[i]], 
                values = values[[i]], simulation = simulation, 
                units = units)
        }
    }
}
setParameterValuesByPathWithUnit <-
function (parameterPaths, values, simulation, units = NULL) 
{
    validateIsString(c(parameterPaths, units))
    validateIsNumeric(values)
    validateIsOfType(simulation, "Simulation")
    for (i in seq_along(parameterPaths)) {
        param <- getParameter(parameterPaths[[i]], simulation)
        valueInBaseUnit <- values[[i]]
        if (!is.null(units)) {
            valueInBaseUnit <- toBaseUnit(quantity = param, values = valueInBaseUnit, 
                unit = units[[i]])
        }
    }
    setParameterValues(parameters = param, values = valueInBaseUnit)
}
sineFunction <-
function (x, amplitude, period, xOffset, yOffset) 
{
    amplitude * sin((2 * pi/period) * (x + xOffset)) + yOffset
}
sourceAll <-
function (folderPath, recursive = FALSE) 
{
    filesPaths <- list.files(folderPath, recursive = recursive)
    sourceFile <- function(filePath) {
        if (toupper(file_ext(filePath)) == "R") {
            source(filePath, encoding = "UTF-8")
        }
        invisible()
    }
    invisible(lapply(file.path(folderPath, filesPaths), sourceFile))
}
stringToNum <-
function (string) 
{
    numVals <- as.numeric(string)
    naVals <- is.na(numVals)
    if (any(naVals)) {
        for (idx in which(naVals)) {
            if (is.na(string[[idx]])) {
                next
            }
            if (substring(string[[idx]], first = 1, last = 1) == 
                "<") {
                numVals[[idx]] <- 0
            }
        }
    }
    return(numVals)
}
validateDimension <-
function (dimension) 
{
    if (!existsDimension(dimension)) {
        stop(messages$errorDimensionNotSupported(dimension))
    }
}
validateEnumValue <-
function (value, enum, nullAllowed = FALSE) 
{
    if (is.null(value)) {
        if (nullAllowed) {
            return()
        }
        stop(messages$errorEnumValueUndefined(enum))
    }
    enumKey <- enumGetKey(enum, value)
    if (any(names(enum) == enumKey)) {
        return()
    }
    stop(messages$errorValueNotInEnum(enum, enumKey))
}
validateFileExists <-
function (filePath) 
{
    if (!file.exists(filePath)) {
        stop(messages$erroFileNotFound(filePath))
    }
}
validateIsInteger <-
function (object, nullAllowed = FALSE) 
{
    if (nullAllowed && is.null(object)) {
        return()
    }
    if (all(floor(object) == object, na.rm = TRUE)) {
        return()
    }
    objectName <- deparse(substitute(object))
    objectTypes <- "integer"
    stop(messages$errorWrongType(objectName, class(object)[1], 
        objectTypes))
}
validateIsLogical <-
function (object, nullAllowed = FALSE) 
{
    validateIsOfType(object, "logical", nullAllowed)
}
validateIsNumeric <-
function (object, nullAllowed = FALSE) 
{
    validateIsOfType(object, c("numeric", "integer"), nullAllowed)
}
validateIsOfType <-
function (object, type, nullAllowed = FALSE) 
{
    if (nullAllowed && is.null(object)) {
        return()
    }
    if (isOfType(object, type)) {
        return()
    }
    objectName <- deparse(substitute(object))
    objectTypes <- .typeNamesFrom(type)
    stop(messages$errorWrongType(objectName, class(object)[1], 
        objectTypes))
}
validateIsSameLength <-
function (...) 
{
    if (isSameLength(...)) {
        return()
    }
    objectName <- deparse(substitute(list(...)))
    argnames <- sys.call()
    arguments <- paste(lapply(argnames[-1], as.character), collapse = ", ")
    stop(messages$errorDifferentLength(arguments))
}
validateIsString <-
function (object, nullAllowed = FALSE) 
{
    validateIsOfType(object, "character", nullAllowed)
}
validateUnit <-
function (unit, dimension) 
{
    if (!hasUnit(unit, dimension)) {
        stop(messages$errorUnitNotSupported(unit, dimension))
    }
}
writeIndividualToXLS <-
function (individualCharacteristics, outputXLSPath) 
{
    validateIsOfType(individualCharacteristics, "IndividualCharacteristics")
    validateIsString(outputXLSPath)
    individual <- createIndividual(individualCharacteristics)
    columnNames <- c("Container Path", "Parameter Name", "Value", 
        "Units")
    containerPaths <- vector("character", length(individual$distributedParameters$paths))
    paramNames <- vector("character", length(individual$distributedParameters$paths))
    values <- vector("numeric", length(individual$distributedParameters$paths))
    units <- vector("character", length(individual$distributedParameters$paths))
    for (i in seq_along(individual$distributedParameters$paths)) {
        fullPathParts <- strsplit(individual$distributedParameters$paths[[i]], 
            split = "|", fixed = TRUE)[[1]]
        containerPath <- paste(fullPathParts[1:length(fullPathParts) - 
            1], collapse = "|")
        paramName <- fullPathParts[[length(fullPathParts)]]
        containerPaths[i] <- containerPath
        paramNames[i] <- paramName
        values[i] <- individual$distributedParameters$values[[i]]
        units[i] <- individual$distributedParameters$units[[i]]
    }
    output <- data.frame(unlist(containerPaths, use.names = FALSE), 
        unlist(paramNames, use.names = FALSE), unlist(as.numeric(values), 
            use.names = FALSE), unlist(units, use.names = FALSE))
    colnames(output) <- columnNames
    write.xlsx(output, file = outputXLSPath, colNames = TRUE)
}
XYData <- R6::R6Class( 
 
    "XYData",
    inherit = Plotable,
    cloneable = FALSE,
    public = list(
        initialize = function (xVals, yVals, label, yError = NULL) 
        {
          validateIsNumeric(c(xVals, yVals))
            validateIsNumeric(yError, nullAllowed = TRUE)
            validateIsSameLength(xVals, yVals)
            super$initialize(label)
            private$.xVals <- xVals
            private$.yVals <- yVals
            if (!is.null(yError)) {
                validateIsSameLength(yVals, yError)
                yError[is.na(yError)] <- 0
                private$.yError <- yError
            }
            private$.dataType <- XYDataTypes$Unspecified
            self$xDimension <- Dimensions$Time
            self$yDimension <- Dimensions$Dimensionless
        },
        yMinPositive = function () 
        {
            effectiveVals <- private$.yVals + self$yOffset - (private$.yError %||% 
                                                                  0)
            min(effectiveVals[effectiveVals > 0]) * self$yFactor
        },
        xUnitDimensionFactor = function (dimension, unit) 
        {
            getUnitConversionFactor(self$xUnit, getBaseUnit(self$xDimension), 
                                    self$xDimension) * dimensionsConversionFactor(self$xDimension, 
                                                                                  dimension) * getUnitConversionFactor(getBaseUnit(dimension), 
                                                                                                                       unit, dimension)
        },
        yUnitDimensionFactor = function (dimension, unit, mw = NULL) 
        {
            getUnitConversionFactor(self$yUnit, getBaseUnit(self$yDimension), 
                                    self$yDimension) * dimensionsConversionFactor(self$yDimension, 
                                                                                  dimension, mw) * getUnitConversionFactor(getBaseUnit(dimension), 
                                                                                                                           unit, dimension)
        },
       yErrorUnitDimensionFactor = function (dimension, unit, mw = NULL) 
        {
            getUnitConversionFactor(self$yErrorUnit, getBaseUnit(self$yDimension), 
                                    self$yDimension) * dimensionsConversionFactor(self$yDimension, 
                                                                                  dimension, mw) * getUnitConversionFactor(getBaseUnit(dimension), 
                                                                                                                           unit, dimension)
        },
      xValuesProcessed = function (dimension = NULL, unit = NULL) 
        {
            unitDimensionFactor <- 1
            if ((is.null(dimension) || is.null(unit)) && !(is.null(dimension) && 
                                                           is.null(unit))) {
                stop(messages$errorOneArgumentNullButNotBoth("dimension", 
                                                             "unit"))
            }
            if (!is.null(dimension)) {
                unitDimensionFactor <- self$xUnitDimensionFactor(dimension, 
                                                                 unit)
            }
            return((private$.xVals + self$xOffset) * self$xFactor * unitDimensionFactor)
        },
        yValuesProcessed = function (dimension = NULL, unit = NULL, mw = NULL) 
        {
            
          unitDimensionFactor <- 1
            if ((is.null(dimension) || is.null(unit)) && !(is.null(dimension) && 
                                                           is.null(unit))) {
                stop(messages$errorOneArgumentNullButNotBoth("dimension", 
                                                             "unit"))
            }
            if (!is.null(dimension)) {
                unitDimensionFactor <- self$yUnitDimensionFactor(dimension, 
                                                                 unit, mw)
            }
            return((private$.yVals + self$yOffset) * self$yFactor * unitDimensionFactor)
        },
        yErrorProcessed = function (dimension = NULL, unit = NULL, mw = NULL) 
        {
            unitDimensionFactor <- 1
            if ((is.null(dimension) || is.null(unit)) && !(is.null(dimension) && 
                                                           is.null(unit))) {
                stop(messages$errorOneArgumentNullButNotBoth("dimension", 
                                                             "unit"))
            }
            if (!is.null(dimension)) {
                unitDimensionFactor <- self$yErrorUnitDimensionFactor(dimension, 
                                                                      unit, mw)
            }
            return((private$.yError + self$yOffset) * self$yFactor * 
                       unitDimensionFactor)
        },
       print = function (...) 
        {
            super$print()
            private$printLine("Data type", c(private$.dataType))
            private$printLine("X dimension", c(private$.XDim))
            private$printLine("X unit", c(private$.XUnit))
            private$printLine("Y dimension", c(private$.YDim))
            private$printLine("Y unit", c(private$.YUnit))
            private$printLine("Y error unit", c(private$.YErrorUnit))
            invisible(self)
        }
    ),
    active = list(
        xValues = function (value) 
        {
            if (missing(value)) {
                private$.xVals
            }
            else {
                validateIsNumeric(value)
                validateIsSameLength(value, private$.yVals)
                private$.xVals <- value
            }
        },
        yValues = function (value) 
        {
            if (missing(value)) {
                private$.yVals
            }
            else {
                validateIsNumeric(value)
                validateIsSameLength(value, private$.xVals)
                private$.yVals <- value
            }
        },
        yError = function (value) 
        {
            if (missing(value)) {
                private$.yError
            }
            else {
                validateIsNumeric(value, nullAllowed = TRUE)
                if (!is.null(value)) {
                    validateIsSameLength(private$.yVals, value)
                    value[is.na(value)] <- 0
                    private$.yError <- value
                }
            }
        },
        xMax = function (value) 
        {
            if (missing(value)) {
                max(private$.xVals + self$xOffset) * self$xFactor
            }
            else {
                stop(messages$errorPropertyReadOnly("xMax"))
            }
        },
       xMin = function (value) 
        {
            if (missing(value)) {
                min(private$.xVals + self$xOffset) * self$xFactor
            }
            else {
                stop(messages$errorPropertyReadOnly("xMin"))
            }
        },
        yMax = function (value) 
        {
            if (missing(value)) {
                max(private$.yVals + self$yOffset + (private$.yError %||% 
                                                         0)) * self$yFactor
            }
            else {
                stop(messages$errorPropertyReadOnly("yMax"))
            }
        },
        yMin = function (value) 
        {
            if (missing(value)) {
                min(private$.yVals + self$yOffset - (private$.yError %||% 
                                                         0)) * self$yFactor
            }
            else {
                stop(messages$errorPropertyReadOnly("yMin"))
            }
        },
        dataType = function (value) 
        {
            if (missing(value)) {
                private$.dataType
            }
            else {
                validateEnumValue(enum = XYDataTypes, value)
                private$.dataType <- value
            }
        },
        xDimension = function (value) 
        {
            if (missing(value)) {
                private$.XDim
            }
            else {
           
                validateDimension(value)
                private$.XDim <- value
                private$.XUnit <- getBaseUnit(value)
            }
        },
       xUnit = function (value) 
        {
            if (missing(value)) {
                private$.XUnit
            }
            else {
                validateUnit(value, self$xDimension)
                private$.XUnit <- value
            }
        },
       yDimension = function (value) 
        {
            if (missing(value)) {
                private$.YDim
            }
            else {
                validateDimension(value)
                private$.YDim <- value
                private$.YUnit <- getBaseUnit(value)
                private$.YErrorUnit <- getBaseUnit(value)
            }
        },
        yUnit = function (value) 
        {
            if (missing(value)) {
                private$.YUnit
            }
            else {
                validateUnit(value, self$yDimension)
                private$.YUnit <- value
            }
        },
       yErrorUnit = function (value) 
        {
            if (missing(value)) {
                private$.YErrorUnit
            }
            else {
                if (value == "") {
                    private$.YErrorUnit <- value
                }
                else {
                    validateUnit(value, self$yDimension)
                    private$.YErrorUnit <- value
                }
            }
        }
    ),
    private = list(
        .xVals = NULL,
        .yVals = NULL,
        .yError = NULL,
       .dataType = NULL,
       .XDim = NULL,
       .XUnit = NULL,
       .YDim = NULL,
       .YUnit = NULL,
       .YErrorUnit = NULL
    )
)

XYDataTypes <-
list(Simulated = "Simulated", Observed = "Observed", Unspecified = "Unspecified")

#in ospsuite 10 not in 9
toList <-
  function (object) 
  {
    if (is.list(object)) {
      return(object)
    }
    return(list(object))
  }