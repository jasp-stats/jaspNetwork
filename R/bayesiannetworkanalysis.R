#
# Copyright (C) 2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#' @export
BayesianNetworkAnalysis <- function(jaspResults, dataset, options) {

  options <- .bayesianNetworkAnalysisNormalizeVariableOptions(options)
  options <- .bayesianNetworkAnalysisNormalizeModelOptions(options)

  # MissingValues needed for the .networkAnalysisReadData function in the frequentist network module:
  options[["missingValues"]] <- "listwise" # Unfortunately BDgraph does not work with pairwise missing values

  dataset <- .networkAnalysisReadData(dataset, options) # from networkanalysis.R

  mainContainer <- .bayesianNetworkAnalysisSetupMainContainerAndTable(jaspResults, dataset, options)
  .bayesianNetworkAnalysisErrorCheck(mainContainer, dataset, options)

  network <- .bayesianNetworkAnalysisRun(mainContainer, dataset, options)

  .bayesianNetworkAnalysisMainTable          (mainContainer, dataset, options, network)
  .bayesianNetworkAnalysisEdgeOverviewTable  (mainContainer, network, options)
  .bayesianNetworkAnalysisInterpretativeScale(mainContainer, network, options)
  .bayesianNetworkAnalysisEdgeEvidenceTable(mainContainer, network, options)
  .bayesianNetworkAnalysisPlotContainer    (mainContainer, network, options)
  .bayesianNetworkAnalysisCentralityTable  (mainContainer, network, options)

  # Stochastic Block Model output
  .bayesianNetworkAnalysisSbmAllocationsTable    (mainContainer, network, options)
  .bayesianNetworkAnalysisSbmNumBlocksTable      (mainContainer, network, options)
  .bayesianNetworkAnalysisSbmCoclusteringTable   (mainContainer, network, options)
  .bayesianNetworkAnalysisSbmClusterBayesFactor  (mainContainer, network, options)

  return()
}

.bayesianNetworkAnalysisSetupMainContainerAndTable <- function(jaspResults, dataset, options) {

  mainContainer <- jaspResults[["mainContainer"]]
  if (is.null(mainContainer)) {
    mainContainer <- createJaspContainer(dependencies = c("variables", "groupingVariable",
                                         "variablesBlumeCapel",
                                                          "burnin", "iter", "seed", "gPrior",
                                                          "edgePrior", "interactionScale",
                                                          "betaAlpha", "betaBeta",
                                                          "betaAlpha_between", "betaBeta_between",
                                                          "lambda", "dirichletAlpha",
                                                          "thresholdAlpha", "thresholdBeta",
                                                          "chains", "omrfUpdateMethod"))
    jaspResults[["mainContainer"]] <- mainContainer
  }
  .bayesianNetworkAnalysisMainTableMeta(mainContainer, dataset, options)

  return(mainContainer)
}

.bayesianNetworkAnalysisMainTableMeta <- function(mainContainer, dataset, options) {

  if (is.null(mainContainer[["generalTable"]])) {

    tb <- createJaspTable(gettext("Summary of Network"), position = 1, dependencies = c("minEdgeStrength", "edgeSpecificOverviewInclusionCriteria"))

    if (length(dataset) > 1L) tb$addColumnInfo(name = "info", title = gettext("Network"), type = "string")

    tb$addColumnInfo(name = "nodes",        title = gettext("Number of nodes"),                          type = "integer")
    tb$addColumnInfo(name = "included",     title = gettext("Number of included edges"),                 type = "string")
    tb$addColumnInfo(name = "excluded",     title = gettext("Number of excluded edges"),                 type = "integer")
    tb$addColumnInfo(name = "inconclusive", title = gettext("Number of edges with inconclusive evidence"), type = "integer")
    tb$addColumnInfo(name = "Sparsity",     title = gettext("Sparsity"),                                 type = "number")


    tb$addFootnote(gettext("Edge categorization is based on the inclusion criteria BF: included (BF\u2081\u2080 \u2265 threshold), excluded (BF\u2081\u2080 \u2264 1/threshold), inconclusive (otherwise)."))

    mainContainer[["generalTable"]] <- tb
  }
  return()
}

.bayesianNetworkAnalysisErrorCheck <- function(mainContainer, dataset, options) {

  if (length(options[["variables"]]) < 3)
    return()

  # check for errors, but only if there was a change in the data (which implies state[["network"]] is NULL)
  if (is.null(mainContainer[["networkState"]])) {
    groupingVariable <- attr(dataset, "groupingVariable")
    dataset <- Reduce(rbind.data.frame, dataset)

    if (options[["groupingVariable"]] != "") {
      # these cannot be chained unfortunately - this will be changed to bgmCompare soon
      groupingVariableName <- options[["groupingVariable"]]
      dfGroup <- data.frame(groupingVariable)
      colnames(dfGroup) <- groupingVariableName
      .hasErrors(dataset = dfGroup,
                 type = c("missingValues", "factorLevels", "observations"),
                 missingValues.target = groupingVariableName,
                 factorLevels.target = groupingVariableName,
                 factorLevels.amount = "< 2",
                 observations.amount = "< 3",
                 observations.grouping = groupingVariableName,
                 exitAnalysisIfErrors = TRUE)
      dataset[[options[["groupingVariable"]]]] <- groupingVariable
      groupingVariable <- options[["groupingVariable"]]
    } else {
      .hasErrors(dataset = dataset,
                 type = c("observations"),
                 observations.amount = "< 3",
                 exitAnalysisIfErrors = TRUE)
    }
  }
}

.bayesianNetworkAnalysisRun <- function(mainContainer, dataset, options) {

  # List that contains state or is empty:
  networkList <- list(
    network    = mainContainer[["networkState"]]$object, # stores the results
    centrality = mainContainer[["centralityState"]]$object,
    layout     = mainContainer[["layoutState"]]$object
  )

  if (length(options[["variables"]]) <= 2L) # returns an empty table if there are less than 3 variables
    return(networkList)

  if (is.null(networkList[["network"]]))
    tryCatch(
      networkList[["network"]] <- .bayesianNetworkAnalysisComputeNetworks(options, dataset),
      error = function(e) {
        # rethrow the error if it was .quitAnalysis was called
        if (inherits(e, "validationError"))
          stop(e)

        mainContainer$setError(.extractErrorMessage(e[["message"]]))
      }
    )

  if (!mainContainer$getError() && !is.null(networkList[["network"]])) {
    if (is.null(networkList[["layout"]]))
      networkList[["layout"]] <- .bayesianNetworkAnalysisComputeLayout(networkList[["network"]], dataset, options)

    if (is.null(networkList[["centrality"]]) && (options[["centralityTable"]] || options[["centralityPlot"]]) &&
        options[["groupingVariable"]] == "")
      networkList[["centrality"]] <- .bayesianNetworkAnalysisComputeCentrality(networkList[["network"]], options)

    if (is.null(names(networkList[["network"]])) || any(names(networkList[["network"]]) == "")) {
      defaultNames <- names(dataset)
      if (is.null(defaultNames) || length(defaultNames) != length(networkList[["network"]]))
        defaultNames <- paste0(gettext("Network "), seq_along(networkList[["network"]]))

      names(networkList[["network"]]) <- defaultNames
    }

    mainContainer[["networkState"]]    <- createJaspState(networkList[["network"]])
    mainContainer[["centralityState"]] <- createJaspState(networkList[["centrality"]], dependencies = c("maxEdgeStrength", "minEdgeStrength", "credibilityInterval"))
    mainContainer[["layoutState"]]     <- createJaspState(networkList[["layout"]],
                                                          dependencies = c("layout", "layoutSpringRepulsion", "layoutX", "layoutY"))

  }

  return(networkList)
}

.bayesianNetworkAnalysisComputeCentrality <- function(networks, options) {

  centralities <- vector("list", length(networks))
  for (nw in seq_along(networks)) {

    network <- networks[[nw]]

    if (options[["credibilityInterval"]]) {

      centralitySamples <- centrality(network = network, options = options)

      # Compute centrality measures for each posterior sample:
      nSamples <- nrow(network$samplesPosterior)

      # centralitySamples is in wide format, so to select all samples (without cols representing the variables) we need 3:(nSamples+2)
      posteriorMeans <- apply(centralitySamples[, 3:(nSamples+2)], MARGIN = 1, mean)

      centralityHDIintervals <- apply(centralitySamples[, 3:(nSamples+2)], MARGIN = 1,
                                      FUN = HDInterval::hdi, allowSplit = FALSE)

      centralitySummary <- cbind(centralitySamples[, 1:2], posteriorMeans, t(centralityHDIintervals))

    } else {
      centralitySummary <- centrality(network = network, options = options)
    }

    centralities[[nw]] <- centralitySummary

  }

  return(centralities)
}

.bayesianNetworkAnalysisComputeLayout <- function(networks, dataset, options) {

  # Reformat networks to fit averageLayout:
  weightMatrices <- list()
  for (i in seq_along(networks)) {
    weightMatrices[[i]] <- networks[[i]]$graph
  }
  jaspBase::.suppressGrDevice(layout <- qgraph::averageLayout(weightMatrices, layout = options[["layout"]], repulsion = options[["layoutSpringRepulsion"]]))
  rownames(layout) <- colnames(networks[[1L]])

  return(layout)
}

.bayesianNetworkAnalysisAssignedVariableRows <- function(assignedVariables) {

  if (is.null(assignedVariables) || length(assignedVariables) == 0L || identical(assignedVariables, ""))
    return(list())

  if (!is.list(assignedVariables)) {
    assignedVariables <- as.list(as.character(assignedVariables))
    names(assignedVariables) <- NULL
  }

  lapply(assignedVariables, function(entry) {
    if (is.list(entry))
      return(entry)

    list(variable = as.character(entry))
  })
}

.bayesianNetworkAnalysisNormalizeVariableOptions <- function(options) {

  variableRows <- .bayesianNetworkAnalysisAssignedVariableRows(options[["variables"]])

  if (length(variableRows) == 0L) {
    options[["variables"]] <- character(0)
    if (is.null(options[["variablesBlumeCapel"]]))
      options[["variablesBlumeCapel"]] <- list()

    return(options)
  }

  variables <- vapply(variableRows, `[[`, character(1L), "variable")
  options[["variables"]] <- unname(variables)

  hasBlumeCapelColumn <- any(vapply(variableRows, function(entry) !is.null(entry[["blumeCapel"]]), logical(1L)))

  if (!hasBlumeCapelColumn) {
    if (is.null(options[["variablesBlumeCapel"]]))
      options[["variablesBlumeCapel"]] <- list()

    return(options)
  }

  blumeCapelRows <- variableRows[vapply(variableRows, function(entry) isTRUE(entry[["blumeCapel"]]), logical(1L))]

  options[["variablesBlumeCapel"]] <- lapply(blumeCapelRows, function(entry) {
    list(
      variable = entry[["variable"]],
      levels   = entry[["levels"]]
    )
  })

  return(options)
}

.bayesianNetworkAnalysisResolveBaselineCategory <- function(variableName, variableData, baselineValue) {

  if (is.null(baselineValue) || length(baselineValue) == 0L || identical(baselineValue, ""))
    return(1L)

  baselineCategory <- suppressWarnings(as.integer(baselineValue))
  if (!is.na(baselineCategory))
    return(max(1L, baselineCategory))

  if (is.factor(variableData)) {
    baselineCategory <- match(as.character(baselineValue), levels(variableData))
    if (!is.na(baselineCategory))
      return(baselineCategory)
  }

  .quitAnalysis(gettextf("Could not determine the baseline category for variable %s.", variableName))
}

.bayesianNetworkAnalysisBuildVariableTypeSpec <- function(options, dataset) {

  variables <- options[["variables"]]

  inferredType <- vapply(variables, function(variableName) {
    if (is.factor(dataset[[variableName]]))
      return("ordinal")

    "continuous"
  }, character(1L))

  baselineCategory <- stats::setNames(rep(1L, length(variables)), variables)

  variablesBlumeCapel <- .bayesianNetworkAnalysisAssignedVariableRows(options[["variablesBlumeCapel"]])

  explicitTypes <- stats::setNames(rep("blume-capel", length(variablesBlumeCapel)),
                                   vapply(variablesBlumeCapel, `[[`, character(1L), "variable"))

  unknownVariables <- setdiff(names(explicitTypes), variables)
  if (length(unknownVariables) > 0L) {
    .quitAnalysis(gettextf("Some model type assignments refer to variables that are not part of the analysis: %s.",
                           paste(unknownVariables, collapse = ", ")))
  }

  if (length(explicitTypes) > 0L)
    inferredType[names(explicitTypes)] <- unname(explicitTypes)

  for (entry in variablesBlumeCapel) {
    variableName <- entry[["variable"]]

    if (!is.factor(dataset[[variableName]])) {
      .quitAnalysis(gettextf("Variable %s cannot be treated as Blume-Capel because it is not ordinal.",
                             variableName))
    }

    baselineCategory[[variableName]] <- .bayesianNetworkAnalysisResolveBaselineCategory(
      variableName  = variableName,
      variableData  = dataset[[variableName]],
      baselineValue = entry[["levels"]]
    )
  }

  list(
    type             = unname(inferredType[variables]),
    baselineCategory = unname(baselineCategory[variables])
  )
}

.bayesianNetworkAnalysisCompareSupported <- function(options, variableSpec, nGroups) {

  if (options[["groupingVariable"]] == "" || nGroups < 2L)
    return(FALSE)

  all(variableSpec[["type"]] %in% c("ordinal", "blume-capel"))
}

.bayesianNetworkAnalysisStochasticBlockAllowed <- function(options) {
  options[["groupingVariable"]] == ""
}

.bayesianNetworkAnalysisAssertSupportedPriors <- function(options) {

  if (options[["edgePrior"]] == "Stochastic-Block" && !.bayesianNetworkAnalysisStochasticBlockAllowed(options)) {
    .quitAnalysis(gettext(
      "The Stochastic block model edge prior is not available when Split is selected. Please use Bernoulli or Beta-binomial."
    ))
  }
}

.bayesianNetworkAnalysisMakeProgressCallback <- function(label = "") {
  lastDone <- 0L
  initialized <- FALSE
  callback <- function(done, total_iter) {
    if (!initialized) {
      jaspBase::startProgressbar(total_iter, label)
      initialized <<- TRUE
    }
    ticks <- done - lastDone
    for (i in seq_len(ticks))
      jaspBase::progressbarTick()
    lastDone <<- done
  }
  return(callback)
}

.bayesianNetworkAnalysisFitSingleNetwork <- function(data, variableSpec, options, progressLabel = "") {

  updateMethod <- .bayesianNetworkAnalysisNormalizeUpdateMethod(options[["omrfUpdateMethod"]])

  jaspBase::.setSeedJASP(options)
  easybgmFit <- try(easybgm::easybgm(
    data    = data,
    type    = variableSpec[["type"]],
    baseline_category            = variableSpec[["baselineCategory"]],
    package = "bgms",
    iter                         = options[["iter"]],
    seed                         = options[["seed"]],
    save                         = TRUE,
    centrality                   = FALSE,
    progress                     = FALSE,
    warmup                       = options[["burnin"]],
    chains                       = as.integer(options[["chains"]]),
    update_method                = updateMethod,
    inclusion_probability        = options[["gPrior"]],
    pairwise_scale               = options[["interactionScale"]],
    edge_prior                   = options[["edgePrior"]],
    main_alpha                   = options[["thresholdAlpha"]],
    main_beta                    = options[["thresholdBeta"]],
    beta_bernoulli_alpha         = options[["betaAlpha"]],
    beta_bernoulli_beta          = options[["betaBeta"]],
    beta_bernoulli_alpha_between = options[["betaAlpha_between"]],
    beta_bernoulli_beta_between  = options[["betaBeta_between"]],
    lambda                       = options[["lambda"]],
    dirichlet_alpha              = options[["dirichletAlpha"]],
    progress_callback            = .bayesianNetworkAnalysisMakeProgressCallback(progressLabel)
  ))

  if (isTryError(easybgmFit)) {
    message <- .extractErrorMessage(easybgmFit)
    .quitAnalysis(gettextf("The analysis failed with the following error message:\n%s", message))
  }

  easybgmFit
}

.bayesianNetworkAnalysisExtractEasybgmResult <- function(easybgmFit, variableSpec, options, keepRawFit = FALSE) {

  easybgmResult <- list()

  easybgmResult$graphWeights           <- easybgmFit$graph_weights
  easybgmResult$inclusionProbabilities <- easybgmFit$inc_probs
  easybgmResult$BF                     <- easybgmFit$inc_BF
  easybgmResult$structure              <- easybgmFit$structure
  easybgmResult$estimates              <- as.matrix(easybgmFit$parameters)
  easybgmResult$graph                  <- easybgmResult$estimates * easybgmResult$structure
  easybgmResult$sampleGraphs           <- easybgmFit$sample_graph
  easybgmResult$samplesPosterior       <- easybgmFit$samples_posterior
  easybgmResult$variableType           <- variableSpec[["type"]]
  easybgmResult$baselineCategory       <- variableSpec[["baselineCategory"]]
  easybgmResult$logOdds                <- easybgmFit$log_odds
  easybgmResult$precisionMatrix        <- easybgmFit$precision_matrix
  easybgmResult$partialCorrelations    <- easybgmFit$partial_correlations

  # Store SBM-specific results if Stochastic-Block edge prior was used.
  if (options[["edgePrior"]] == "Stochastic-Block" && .bayesianNetworkAnalysisStochasticBlockAllowed(options)) {
    easybgmResult$sbm <- list(
      posterior_mean_allocations         = easybgmFit$sbm$posterior_mean_allocations,
      posterior_mode_allocations         = easybgmFit$sbm$posterior_mode_allocations,
      posterior_num_blocks               = easybgmFit$sbm$posterior_num_blocks,
      posterior_mean_coclustering_matrix = easybgmFit$sbm$posterior_mean_coclustering_matrix
    )
  }

  if (keepRawFit)
    easybgmResult$easybgmFit <- easybgmFit

  easybgmResult
}

.bayesianNetworkAnalysisNormalizeUpdateMethod <- function(updateMethodRaw) {

  if (is.null(updateMethodRaw) || length(updateMethodRaw) == 0L)
    return("nuts")

  # Some option parsers may include object metadata in the selected value.
  # Keep only the first comma-separated token and normalize spacing/case.
  updateMethod <- trimws(strsplit(as.character(updateMethodRaw)[1L], ",", fixed = TRUE)[[1L]][1L])
  updateMethod <- tolower(gsub("[[:space:]_]+", "-", updateMethod))

  aliases <- c(
    "adaptive-metropolis" = "adaptive-metropolis",
    "adaptivemetropolis"  = "adaptive-metropolis",
    "nuts"                = "nuts"
  )

  mapped <- unname(aliases[updateMethod])
  if (length(mapped) == 1L && !is.na(mapped))
    return(mapped)

  .quitAnalysis(gettextf(
    "Unsupported update method '%s'. Please select one of: adaptive-metropolis, nuts.",
    updateMethodRaw
  ))
}

.bayesianNetworkAnalysisNormalizeModelOptions <- function(options) {

  .normalizeScalarOption <- function(value, default = "") {
    if (is.null(value) || length(value) == 0L)
      return(default)

    token <- trimws(strsplit(as.character(value)[1L], ",", fixed = TRUE)[[1L]][1L])
    if (is.na(token) || token %in% c("", "NA", "NULL"))
      return(default)

    token
  }

  edgePriorRaw <- .normalizeScalarOption(options[["edgePrior"]], default = "Bernoulli")
  edgePriorKey <- tolower(gsub("[[:space:]_]+", "-", edgePriorRaw))

  edgePriorAliases <- c(
    "bernoulli"              = "Bernoulli",
    "beta-bernoulli"         = "Beta-Bernoulli",
    "beta-binomial"          = "Beta-Bernoulli",
    "stochastic-block"       = "Stochastic-Block",
    "stochastic-block-model" = "Stochastic-Block"
  )

  edgePrior <- unname(edgePriorAliases[edgePriorKey])
  if (length(edgePrior) != 1L || is.na(edgePrior)) {
    .quitAnalysis(gettextf(
      "Unsupported edge prior '%s'. Please select one of: Bernoulli, Beta-binomial, Stochastic block model.",
      edgePriorRaw
    ))
  }

  chainsRaw <- .normalizeScalarOption(options[["chains"]], default = "4")
  chains <- suppressWarnings(as.integer(chainsRaw))
  if (is.na(chains) || chains < 1L)
    chains <- 4L

  options[["edgePrior"]] <- edgePrior
  options[["chains"]] <- as.character(chains)
  options[["omrfUpdateMethod"]] <- .bayesianNetworkAnalysisNormalizeUpdateMethod(options[["omrfUpdateMethod"]])

  return(options)
}

.bayesianNetworkAnalysisComputeNetworks <- function(options, dataset) {

  .bayesianNetworkAnalysisAssertSupportedPriors(options)

  groupData <- lapply(dataset, function(df) df[options[["variables"]]])
  pooledData <- Reduce(rbind.data.frame, groupData)

  nGroups <- length(groupData)
  groupNames <- names(groupData)
  if (is.null(groupNames) || any(groupNames == ""))
    groupNames <- paste0(gettext("Group "), seq_len(nGroups))

  networks <- list()

  pooledVariableSpec <- .bayesianNetworkAnalysisBuildVariableTypeSpec(options, pooledData)
  useCompare <- .bayesianNetworkAnalysisCompareSupported(options, pooledVariableSpec, nGroups)

  if (useCompare) {
    updateMethod <- .bayesianNetworkAnalysisNormalizeUpdateMethod(options[["omrfUpdateMethod"]])
    groupIndicator <- rep(seq_len(nGroups), times = vapply(groupData, nrow, integer(1L)))

    jaspBase::.setSeedJASP(options)
    compareFit <- try(easybgm::easybgm_compare(
      data    = pooledData,
      group_indicator             = groupIndicator,
      type    = pooledVariableSpec[["type"]],
      baseline_category           = pooledVariableSpec[["baselineCategory"]],
      package = "bgms",
      iter                        = options[["iter"]],
      seed                        = options[["seed"]],
      save                        = TRUE,
      progress                    = FALSE,
      warmup                      = options[["burnin"]],
      chains                      = as.integer(options[["chains"]]),
      update_method               = updateMethod,
      difference_probability      = options[["gPrior"]],
      difference_scale            = options[["interactionScale"]],
      difference_prior            = options[["edgePrior"]],
      main_alpha                  = options[["thresholdAlpha"]],
      main_beta                   = options[["thresholdBeta"]],
      beta_bernoulli_alpha        = options[["betaAlpha"]],
      beta_bernoulli_beta         = options[["betaBeta"]],
      progress_callback           = .bayesianNetworkAnalysisMakeProgressCallback(gettext("Estimating group comparison"))
    ))

    if (isTryError(compareFit)) {
      message <- .extractErrorMessage(compareFit)
      .quitAnalysis(gettextf("The group comparison failed with the following error message:\n%s", message))
    }

    networks[[gettext("Differences")]] <- .bayesianNetworkAnalysisExtractEasybgmResult(
      easybgmFit   = compareFit,
      variableSpec = pooledVariableSpec,
      options      = options,
      keepRawFit   = FALSE
    )

    pooledFit <- .bayesianNetworkAnalysisFitSingleNetwork(
      data          = pooledData,
      variableSpec  = pooledVariableSpec,
      options       = options,
      progressLabel = gettext("Estimating pooled network")
    )

    networks[[gettext("Pooled")]] <- .bayesianNetworkAnalysisExtractEasybgmResult(
      easybgmFit   = pooledFit,
      variableSpec = pooledVariableSpec,
      options      = options,
      keepRawFit   = options[["edgePrior"]] == "Stochastic-Block" && .bayesianNetworkAnalysisStochasticBlockAllowed(options)
    )
  }

  for (nw in seq_along(groupData)) {
    variableSpec <- .bayesianNetworkAnalysisBuildVariableTypeSpec(options, groupData[[nw]])
    progressLabel <- if (nGroups > 1L) gettextf("Estimating %s", groupNames[[nw]]) else gettext("Estimating network")
    easybgmFit <- .bayesianNetworkAnalysisFitSingleNetwork(groupData[[nw]], variableSpec, options,
                                                            progressLabel = progressLabel)

    networks[[groupNames[[nw]]]] <- .bayesianNetworkAnalysisExtractEasybgmResult(
      easybgmFit   = easybgmFit,
      variableSpec = variableSpec,
      options      = options,
      keepRawFit   = options[["edgePrior"]] == "Stochastic-Block" && .bayesianNetworkAnalysisStochasticBlockAllowed(options)
    )
  }

  if (!useCompare && options[["groupingVariable"]] != "" && nGroups >= 2L) {
    attr(networks, "compareUnavailableReason") <- gettext(
      "Difference network not estimated: all selected variables must be ordinal or Blume-Capel. Showing separate group estimates instead."
    )
  }

  return(networks)
}

.bayesianNetworkAnalysisMainTable <- function(mainContainer, dataset, options, network) {

  if (is.null(network[["network"]]) || mainContainer$getError())
    return()

  tb <- mainContainer[["generalTable"]]
  nGraphs <- length(network[["network"]])

  # Check if group comparison is active and 'Differences' network is present
  groupComparison <- options[["groupingVariable"]] != "" && gettext("Differences") %in% names(network[["network"]])

  if (options[["minEdgeStrength"]] != 0) {
    ignored <- logical(nGraphs)
    for (i in seq_along(network[["network"]])) {
      ignored[i] <- all(abs(network[["network"]][[i]][["graph"]]) <= options[["minEdgeStrength"]])
    }
    if (any(ignored)) {
      if (nGraphs == 1L) {
        text <- gettext("Minimum edge strength ignored in the network plot because it was larger than the absolute value of the strongest edge.")
      } else {
        text <- gettextf("Minimum edge strength ignored in the network plot of group%1$s %2$s because it was larger than the absolute value of the strongest edge.",
                         ifelse(sum(ignored) == 2L, "s", ""),
                         paste0(names(network[["network"]])[ignored], collapse = ", ")
        )
      }
      tb$addFootnote(text, symbol = gettext("<em>Warning: </em>"))
    }
  }

  if (groupComparison) {
    # Only show Differences row, rename columns, drop sparsity
    nw <- network[["network"]][[gettext("Differences")]]
    nVar <- ncol(nw[["graph"]])
    nEdges <- (nVar * (nVar - 1L)) %/% 2
    bfUpper <- nw[["BF"]][upper.tri(nw[["BF"]], diag = FALSE)]
    nDifferent <- sum(bfUpper >= options[["edgeSpecificOverviewInclusionCriteria"]])
    nEqual <- sum(bfUpper <= 1 / options[["edgeSpecificOverviewInclusionCriteria"]])
    nInconclusive <- nEdges - nDifferent - nEqual
    df <- data.frame(
      nodes = nrow(nw[["graph"]]),
      different = paste(nDifferent, "/", nEdges),
      equal = nEqual,
      inconclusive = nInconclusive,
      stringsAsFactors = FALSE
    )
    # Remove info column if present
    if ("info" %in% names(tb$columns)) tb$removeColumn("info")
    # Remove Sparsity column if present
    if ("Sparsity" %in% names(tb$columns)) tb$removeColumn("Sparsity")
    # Rename columns
    tb$setColumnTitle("included", gettext("Number of different edges"))
    tb$setColumnTitle("excluded", gettext("Number of equal edges"))
    tb$setColumnTitle("inconclusive", gettext("Number of edges with inconclusive evidence"))
    # Set data with new column names
    names(df) <- c("nodes", "included", "excluded", "inconclusive")
    tb$setData(df)
  } else {
    df <- data.frame(nodes = integer(nGraphs), included = character(nGraphs), excluded = integer(nGraphs),
                     inconclusive = integer(nGraphs), Sparsity = numeric(nGraphs), stringsAsFactors = FALSE)
    if (nGraphs > 1L)
      df[["info"]] <- names(network[["network"]])

    threshold <- options[["edgeSpecificOverviewInclusionCriteria"]]
    nVar <- ncol(network[["network"]][[1L]][["graph"]])
    for (i in seq_len(nGraphs)) {
      nw <- network[["network"]][[i]]
      nEdges <- (nVar * (nVar - 1L)) %/% 2
      bfUpper <- nw[["BF"]][upper.tri(nw[["BF"]], diag = FALSE)]
      nIncluded    <- sum(bfUpper >= threshold)
      nExcluded    <- sum(bfUpper <= 1 / threshold)
      nInconclusive <- nEdges - nIncluded - nExcluded
      df[["nodes"]][i]        <- nrow(nw[["graph"]])
      df[["included"]][i]     <- paste(nIncluded, "/", nEdges)
      df[["excluded"]][i]     <- nExcluded
      df[["inconclusive"]][i] <- nInconclusive
      df[["Sparsity"]][i]     <- 1 - nIncluded / nEdges
    }
    compareUnavailableReason <- attr(network[["network"]], "compareUnavailableReason")
    if (!is.null(compareUnavailableReason))
      tb$addFootnote(compareUnavailableReason, symbol = gettext("<em>Warning: </em>"))
    tb$setData(df)
  }
}

.bayesianNetworkAnalysisPlotContainer <- function(mainContainer, network, options) {

  plotContainer <- mainContainer[["plotContainer"]]

  if (is.null(plotContainer)) {
    plotContainer <- createJaspContainer(dependencies = c("labelAbbreviation", "labelAbbreviationLength",
                                                          "legend", "variableNamesShown")) # position = 5
    mainContainer[["plotContainer"]] <- plotContainer
  }

  # Only show network plot if NOT comparing networks (i.e., no differences network)
  allNetworks <- network[["network"]]
  hasDifferences <- !is.null(allNetworks) && gettext("Differences") %in% names(allNetworks)
  if (!hasDifferences)
    .networkAnalysisNetworkPlot                    (plotContainer, network, options, method = "Bayesian")

  .bayesianNetworkAnalysisEvidencePlot           (plotContainer, network, options)
  .bayesianNetworkAnalysisPosteriorStructurePlot (plotContainer, network, options)
  .bayesianNetworkAnalysisCentralityPlot         (plotContainer, network, options)
  .bayesianNetworkAnalysisParameterHdiPlot       (plotContainer, network, options)
  .bayesianNetworkAnalysisPosteriorComplexityPlot(plotContainer, network, options)
  .bayesianNetworkAnalysisSbmCoclusteringPlot    (plotContainer, network, options)
}

.bayesianNetworkAnalysisPosteriorStructurePlot <- function(plotContainer, network, options) {

  if (!is.null(plotContainer[["posteriorStructurePlotContainer"]]) || !options[["posteriorStructurePlot"]])
    return()

  allNetworks <- network[["network"]]
  nGraphs <- length(allNetworks)

  title <- if (nGraphs == 1L) "" else gettext("Posterior Probability Structure Plots")

  posteriorStructurePlotContainer <- createJaspContainer(title = title, dependencies = c("posteriorStructurePlot")) # , position = 51

  plotContainer[["posteriorStructurePlotContainer"]] <- posteriorStructurePlotContainer

  if (is.null(network[["network"]]) || plotContainer$getError()) {
    posteriorStructurePlotContainer[["dummyPlot"]] <- createJaspPlot(title = gettext("Posterior Probability Structure Plot"))
    return()
  }

  for (v in names(allNetworks))
    posteriorStructurePlotContainer[[v]] <- createJaspPlot(title = v)

  jaspBase::.suppressGrDevice({

    for (v in names(allNetworks)) {

      networkToPlot <- allNetworks[[v]]

      sortedStructureProbability <- as.data.frame(sort(networkToPlot$graphWeights/sum(networkToPlot$graphWeights), decreasing = TRUE))
      colnames(sortedStructureProbability) <- "posteriorProbability"
      plot <- ggplot2::ggplot(sortedStructureProbability, ggplot2::aes(x = 1:nrow(sortedStructureProbability), y = posteriorProbability)) +
        jaspGraphs::geom_point() +
        ggplot2::ylab("Posterior Structure Probability") +
        ggplot2::xlab("Structure Index")  +
        jaspGraphs::geom_rangeframe() +
        jaspGraphs::themeJaspRaw(legend.position = c(.85, 0.25))

      posteriorStructurePlotContainer[[v]]$plotObject <- plot

    }
  })
}

.bayesianNetworkAnalysisCentralityTable <- function(mainContainer, network, options) {

  if (!is.null(mainContainer[["centralityTable"]]) || !options[["centralityTable"]] ||
      options[["groupingVariable"]] != "")
    return()

  nGraphs <- max(1L, length(network[["network"]]))

  table <- createJaspTable(gettext("Centrality measures per variable"), #position = 2,

                           dependencies = c("centralityTable", "maxEdgeStrength", "minEdgeStrength"))
  table$addColumnInfo(name = "Variable", title = gettext("Variable"), type = "string")

  # shared titles
  overTitles <- names(network[["network"]])
  if (is.null(overTitles))
    overTitles <- gettext("Network")

  for (i in seq_len(nGraphs)) {
    table$addColumnInfo(name = paste0("Betweenness", i),        title = gettext("Betweenness"),        type = "number", overtitle = overTitles[i])
    table$addColumnInfo(name = paste0("Closeness", i),          title = gettext("Closeness"),          type = "number", overtitle = overTitles[i])
    table$addColumnInfo(name = paste0("Strength", i),           title = gettext("Strength"),           type = "number", overtitle = overTitles[i])
    table$addColumnInfo(name = paste0("Expected influence", i), title = gettext("Expected influence"), type = "number", overtitle = overTitles[i])
  }

  mainContainer[["centralityTable"]] <- table
  if (is.null(network[["centrality"]]) || mainContainer$getError())
    return()

  # fill with results
  TBcolumns <- NULL
  for (i in seq_len(nGraphs)) {

    toAdd <- network[["centrality"]][[i]]
    toAdd <- stats::reshape(toAdd, idvar = "node", timevar = "measure", direction = "wide")

    toAdd <- dplyr::select(toAdd, c("node", "posteriorMeans.Betweenness", "posteriorMeans.Closeness", "posteriorMeans.Strength", "posteriorMeans.ExpectedInfluence"))

    names(toAdd) <- c("Variable", paste0(c("Betweenness",
                                           "Closeness",
                                           "Strength",
                                           "Expected influence"), i))

    # If more than 1 network drop the first column which indicates the variable:
    if (i == 1L) {
      TBcolumns <- toAdd
    } else {
      toAdd <- toAdd[, -1L]
      TBcolumns <- cbind(TBcolumns, toAdd)
    }
  }
  table$setData(TBcolumns)

}

.bayesianNetworkAnalysisCentralityPlot <- function(plotContainer, network, options) {

  if (!is.null(plotContainer[["centralityPlot"]]) || !options[["centralityPlot"]] ||
      options[["groupingVariable"]] != "")
    return()

  measuresToShow <- unlist(options[c("betweenness", "closeness", "strength", "expectedInfluence")], use.names = FALSE)
  hasMeasures <- any(measuresToShow)

  width <- if (hasMeasures) 120 * sum(measuresToShow) else 120
  plot <- createJaspPlot(title = gettext("Centrality Plot"), width = width,
                         dependencies = c("centralityPlot", "betweenness", "closeness", "strength", "expectedInfluence", "credibilityInterval"))

  plotContainer[["centralityPlot"]] <- plot

  if (is.null(network[["centrality"]]) || plotContainer$getError() || !hasMeasures)
    return()

  centralitySummary <- network[["centrality"]]

  if (length(centralitySummary) > 1L) {
    centralitySummary <- dplyr::bind_rows(centralitySummary, .id = 'graph')
  } else {
    centralitySummary[[1]][["graph"]] <- NA
    centralitySummary <- data.frame(centralitySummary)
  }

  if (!all(measuresToShow)) {
    measuresToFilter <- c("betweenness", "closeness", "strength", "expectedInfluence")[measuresToShow]
    centralitySummary <- subset(centralitySummary, measure %in% firstup(measuresToFilter))
  }

  .bayesianNetworkAnalysisMakeCentralityPlot(plot, centralitySummary, options)
}

.bayesianNetworkAnalysisMakeCentralityPlot <- function(jaspPlot, centralitySummary, options) {

  # code modified from qgraph::centralityPlot(). Type and graph are switched so the legend title says graph
  if (options[["labelAbbreviation"]])
    centralitySummary[["node"]] <- base::abbreviate(centralitySummary[["node"]], options[["labelAbbreviationLength"]])

  # code modified from qgraph::centralityPlot(). Type and graph are switched so the legend title says graph
  centralitySummary <- centralitySummary[gtools::mixedorder(centralitySummary$node), ]
  centralitySummary$node <- factor(as.character(centralitySummary$node),
                                   levels = unique(gtools::mixedsort(as.character(centralitySummary$node))))

  centralitySummary$nodeLabel <- NA
  if (options[["variableNamesShown"]] == "inLegend") {
    centralitySummary$nodeLabel <- as.character(centralitySummary$node)
    centralitySummary$node <- factor(match(as.character(centralitySummary$node), unique(as.character(centralitySummary$node))))
    levels(centralitySummary$node) <- rev(levels(centralitySummary$node))
    centralitySummary$nodeLabel <- paste(as.character(centralitySummary$node), "=", centralitySummary$nodeLabel)
  }

  if (length(unique(centralitySummary$graph)) > 1L) {
    mapping <- ggplot2::aes(x = posteriorMeans, y = node, group = graph, colour = graph)
    # change the name graph into the variable name for splitting
    guide   <- ggplot2::guides(color = ggplot2::guide_legend(title = options[["groupingVariable"]]))
  } else {
    mapping <- ggplot2::aes(x = posteriorMeans, y = node, group = graph)
    guide   <- NULL
  }

  # add a fill element to the mapping -- this is only used to add a legend for the names of the nodes.
  hasNodeLabels <- !all(is.na(centralitySummary[["nodeLabel"]]))
  if (hasNodeLabels)
    mapping$fill <- as.name("nodeLabel")

  g <- ggplot2::ggplot(centralitySummary, mapping) + guide

  g <- g + ggplot2::geom_path() +
    ggplot2::geom_point() +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL)

  if (options[["credibilityInterval"]]) {

    g <- g + ggplot2::geom_errorbar(ggplot2::aes(x = posteriorMeans, xmin = lower, xmax = upper), size = .5, width = 0.4)
  }

  if (length(unique(centralitySummary$type)) > 1) {
    g <- g + ggplot2::facet_grid(type ~ measure, scales = "free")
  } else {
    g <- g + ggplot2::facet_grid(~ measure, scales = "free")
  }
  g <- g + ggplot2::theme_bw()

  if (options[["legend"]] == "hide")
    g <- g + ggplot2::theme(legend.position = "none")
  else if (hasNodeLabels) {
    # the fill aestethic introduces a set of points left of `1 = contNormal`.
    # the statement below sets the size of those points to 0, effectively making them invisible
    # keywidth removes the invisible space introduced so that the legends nicely line up (if there are multiple)
    g <- g + ggplot2::guides(fill = ggplot2::guide_legend(keywidth = 0, override.aes = list(size = 0, alpha = 0)))
  }

  jaspPlot$plotObject <- g
}

.bayesianNetworkAnalysisParameterHdiPlot <- function(plotContainer, network, options) {

  if (!is.null(plotContainer[["parameterHdiPlotContainer"]]) || !options[["parameterHdiPlot"]] ||
      options[["groupingVariable"]] != "")
    return()

  allNetworks     <- network[["network"]]
  differencesName <- gettext("Differences")
  hasDifferences  <- differencesName %in% names(allNetworks)
  if (hasDifferences)
    allNetworks <- allNetworks[differencesName]
  nGraphs <- length(allNetworks)

  title <- if (nGraphs == 1L) gettext("Parameter HDI Plot") else gettext("Parameter HDI Plots")

  parameterHdiContainer <- createJaspContainer(
    title        = title,
    dependencies = c("parameterHdiPlot", "parameterHdiPlotCoverage",
                     "labelAbbreviation", "labelAbbreviationLength")
  )
  plotContainer[["parameterHdiPlotContainer"]] <- parameterHdiContainer

  if (is.null(allNetworks) || plotContainer$getError()) {
    parameterHdiContainer[["dummyPlot"]] <- createJaspPlot(title = gettext("Parameter HDI Plot"))
    return()
  }

  nVar   <- ncol(allNetworks[[1L]]$estimates)
  nEdges <- nVar * (nVar - 1L) / 2L
  height <- max(400L, nEdges * 30L)
  width  <- 500L

  for (v in names(allNetworks))
    parameterHdiContainer[[v]] <- createJaspPlot(title = if (nGraphs == 1L) "" else v, width = width, height = height)

  coverage <- options[["parameterHdiPlotCoverage"]]

  jaspBase::.suppressGrDevice({
    for (v in names(allNetworks)) {
      p <- try(.bayesianNetworkAnalysisMakeParameterHdiPlot(allNetworks[[v]], options, coverage))
      if (inherits(p, "try-error"))
        parameterHdiContainer[[v]]$setError(.extractErrorMessage(p))
      else
        parameterHdiContainer[[v]]$plotObject <- p
    }
  })

  if (hasDifferences) {
    noteHtml <- createJaspHtml()
    noteHtml$text <- paste0("<p>", gettext("The difference parameter HDI plot shows the posterior mean and highest density interval (HDI) for each pairwise difference in partial association between groups. Positive values indicate a stronger association in the first group; negative values indicate a stronger association in the second group."), "</p>")
    noteHtml$position <- 99
    parameterHdiContainer[["differenceHdiNote"]] <- noteHtml
  }
}

.bayesianNetworkAnalysisMakeParameterHdiPlot <- function(network, options, coverage) {

  samplesPosterior <- network[["samplesPosterior"]]
  if (is.null(samplesPosterior))
    stop(gettext("Posterior samples are required for the parameter HDI plot. Please ensure the model was fitted with 'save = TRUE'."))

  # Construct readable edge labels from decoded variable names.
  # Sort indices in row-major order to match the column order of samplesPosterior (as produced by bgms).
  variables   <- colnames(network[["estimates"]])
  decodedVars <- decodeColNames(variables)
  nVar        <- length(variables)
  upperIdx    <- which(upper.tri(matrix(0L, nVar, nVar)), arr.ind = TRUE)
  upperIdx    <- upperIdx[order(upperIdx[, 1L], upperIdx[, 2L]), ]
  edgeLabels  <- paste0(decodedVars[upperIdx[, 1L]], "-", decodedVars[upperIdx[, 2L]])

  if (options[["labelAbbreviation"]])
    edgeLabels <- base::abbreviate(edgeLabels, minlength = options[["labelAbbreviationLength"]])

  # Compute HDI and posterior means for each partial association
  hdiIntervals     <- apply(samplesPosterior, MARGIN = 2L, FUN = HDInterval::hdi, credMass = coverage)
  posteriorMedians <- apply(samplesPosterior, MARGIN = 2L, FUN = mean)

  posterior <- data.frame(
    median = posteriorMedians,
    lower  = hdiIntervals["lower", ],
    upper  = hdiIntervals["upper", ],
    edge   = edgeLabels,
    stringsAsFactors = FALSE
  )

  # Order by posterior median
  posterior <- posterior[order(posterior$median), ]
  posterior$edge <- factor(posterior$edge, levels = posterior$edge)

  coveragePct <- round(coverage * 100)
  yLabel      <- gettextf("%d%% HDI of Partial Association", coveragePct)

  g <- ggplot2::ggplot(posterior, ggplot2::aes(x = edge, y = median, ymin = lower, ymax = upper)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_pointrange() +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = yLabel) +
    jaspGraphs::themeJaspRaw()

  return(g)
}

.bayesianNetworkAnalysisPosteriorComplexityPlot <- function(plotContainer, network, options) {

  if (!is.null(plotContainer[["complexityPlotContainer"]]) || !options[["complexityPlot"]])
    return()

  allNetworks <- network[["network"]]
  nGraphs <- length(allNetworks)

  title <- if (nGraphs == 1L) gettext("Complexity plot") else gettext("Complexity plots")

  complexityPlotContainer <- createJaspContainer(title = title, dependencies = c("complexityPlot")) # position = 51

  plotContainer[["complexityPlotContainer"]] <- complexityPlotContainer

  if (is.null(network[["network"]]) || plotContainer$getError()) {
    complexityPlotContainer[["dummyPlot"]] <- createJaspPlot(title = gettext("Complexity Plot"))
    return()
  }

  for (v in names(allNetworks))
    complexityPlotContainer[[v]] <- createJaspPlot(title = v)

  jaspBase::.suppressGrDevice({

    for (v in names(allNetworks)) {

      networkToPlot <- allNetworks[[v]]

      complexity <- c()
      for(i in 1:length(networkToPlot$sampleGraphs)){
        complexity[i] <- sum(as.numeric(unlist(strsplit(networkToPlot$sampleGraphs[i], ""))))
      }

      dataComplexity <- dplyr::as_tibble(cbind(complexity, networkToPlot$graphWeights))
      dataComplexity <- dplyr::summarise(dplyr::group_by(dataComplexity, complexity), complexityWeight = sum(V2))
      dataComplexity <- dplyr::mutate(dataComplexity, complexityWeight = complexityWeight/sum(complexityWeight))

      plot <- ggplot2::ggplot(dataComplexity, ggplot2::aes(x = complexity, y = complexityWeight)) +
        jaspGraphs::geom_point() +
        ggplot2::ylab(gettext("Posterior Probability")) +
        ggplot2::xlab(gettext("Number of edges"))  +
        jaspGraphs::geom_rangeframe() +
        jaspGraphs::themeJaspRaw(legend.position = c(.85, 0.25))

      complexityPlotContainer[[v]]$plotObject <- plot

    }
  })

  return()

}

.bayesianNetworkAnalysisStructurePlot <- function(plotContainer, network, options) {

  if (!is.null(plotContainer[["structurePlotContainer"]]) || !options[["posteriorStructurePlot"]])
    return()

  allNetworks <- network[["network"]]
  nGraphs <- length(allNetworks)

  title <- if (nGraphs == 1L) gettext("Structure Plot") else gettext("Structure Plots")

  structurePlotContainer <- createJaspContainer(title = title, dependencies = c("posteriorStructurePlot",
                                                                                "layout", "layoutSpringRepulsion", "edgeSize", "nodeSize", "colorNodesBy", "cut", "details", "nodePalette",
                                                                                "legendSpecificPlotNumber", "model",
                                                                                "labelScale", "labelSize", "labelAbbreviation", "labelAbbreviationLength",
                                                                                "layoutNotUpdated", "layoutX", "layoutY",
                                                                                "manualColorGroups", "colorGroupVariables", "manualColor",
                                                                                "legendToPlotRatio", "edgeLabels", "edgeLabelSize", "edgeLabelPosition"
  ))
  plotContainer[["structurePlotContainer"]] <- structurePlotContainer

  if (is.null(network[["network"]]) || plotContainer$getError()) {
    structurePlotContainer[["dummyPlot"]] <- createJaspPlot(title = gettext("Structure Plot"))
    return()
  }

  layout <- network[["layout"]] # calculated in .bayesianNetworkAnalysisRun()

  groups <- NULL
  nodeColor <- NULL
  allLegends <- rep(FALSE, nGraphs) # no legends

  if (length(options[["colorGroupVariables"]]) > 1L) {

    assignedGroup <- vapply(options[["colorGroupVariables"]], `[[`, character(1L), "group")

    if (length(unique(assignedGroup)) > 1L) {
      # user has defined groups and there are variables in the groups
      groupNames  <- vapply(options[["manualColorGroups"]], `[[`, character(1L), "name")
      groupColors <- vapply(options[["manualColorGroups"]], `[[`, character(1L), "color")

      nGroups <- length(groupNames)

      idx <- match(assignedGroup, groupNames)

      groups <- vector("list", nGroups)
      names(groups) <- groupNames
      for (i in seq_len(nGroups))
        groups[[i]] <- which(idx == i)

      nonEmpty <- lengths(groups) > 0L
      groups <- groups[nonEmpty]

      if (options[["manualColor"]])
        nodeColor <- groupColors[nonEmpty]
    }
  }

  # defaults
  shape <- "circle"
  edgeColor <- NULL

  # TODO: footnote if legend off and nodenames used
  if (options[["variableNamesShown"]] == "inNodes") {
    nodeNames <- NULL

    if (nGraphs == 1) {
      labels <- colnames(allNetworks$Network$graph)
    } else {
      labels <- colnames(allNetworks$`1`$graph)
    }

  } else {

    if (nGraphs == 1) {
      nodeNames <- colnames(allNetworks$Network$graph)
    } else {
      nodeNames <- colnames(allNetworks$`1`$graph)
    }
    labels <- seq_along(nodeNames)

  }

  labels <- decodeColNames(labels)

  if (options[["labelAbbreviation"]])
    labels <- base::abbreviate(labels, minlength = options[["labelAbbreviationLength"]])

  # do we need to draw legends?
  if (!is.null(groups) || !is.null(nodeNames)) {
    if (options[["legend"]] ==  "allPlots") {

      allLegends <- rep(TRUE, nGraphs)

    } else if (options[["legend"]] ==  "specificPlot") {

      if (options[["legendSpecificPlotNumber"]] > nGraphs) {

        allLegends[nGraphs] <- TRUE

      } else if (options[["legendSpecificPlotNumber"]] < 1L) {

        allLegends[1L] <- TRUE

      } else {

        allLegends[options[["legendSpecificPlotNumber"]]] <- TRUE

      }
    }
  }

  names(allLegends) <- names(allNetworks) # allows indexing by name

  basePlotSize <- 320
  legendMultiplier <- options[["legendToPlotRatio"]] * basePlotSize
  height <- setNames(rep(basePlotSize, nGraphs), names(allLegends))
  width  <- basePlotSize + allLegends * legendMultiplier
  for (v in names(allNetworks))
    structurePlotContainer[[v]] <- createJaspPlot(title = v, width = width[v], height = height[v])

  jaspBase::.suppressGrDevice({

    for (v in names(allNetworks)) {

      networkToPlot <- allNetworks[[v]]

      legend <- allLegends[[v]]
      structurePlotContainer[[v]]$plotObject <- .bayesianNetworkAnalysisOneStructurePlot(
        network    = networkToPlot,
        options    = options,
        layout     = layout,
        groups     = groups,
        labels     = labels,
        legend     = legend,
        shape      = shape,
        nodeColor  = nodeColor,
        nodeNames  = nodeNames
      )
    }
  })

}

.bayesianNetworkAnalysisEvidencePlot <- function(plotContainer, network, options) {

  if (!is.null(plotContainer[["evidencePlotContainer"]]) || !options[["evidencePlot"]])
    return()

  allNetworks <- network[["network"]]
  differencesName <- gettext("Differences")
  hasDifferences <- differencesName %in% names(allNetworks)
  if (hasDifferences)
    allNetworks <- allNetworks[differencesName]
  nGraphs <- length(allNetworks)

  # we use an empty container without a name if there is only 1 graph. This container is hidden from the output but it
  # enables us to use the same code for a single network plot and for a collection of network plots.
  title <- if (nGraphs == 1L) gettext("Edge Evidence Plot") else gettext("Edge Evidence Plots")

  evidencePlotContainer <- createJaspContainer(title = title, position = 3, dependencies = c("evidencePlot",
                                                                                             "layout", "layoutSpringRepulsion", "edgeSize", "nodeSize", "colorNodesBy", "cut", "details", "nodePalette",
                                                                                             "legendSpecificPlotNumber", "edgeInclusion", "edgeExclusion", "edgeAbsence",
                                                                                             "labelScale", "labelSize", "labelAbbreviation", "labelAbbreviationLength",
                                                                                             "layoutNotUpdated", "layoutX", "layoutY", "edgeInclusionCriteria",
                                                                                             "manualColorGroups", "colorGroupVariables", "manualColor",
                                                                                             "legendToPlotRatio"
  ))
  plotContainer[["evidencePlotContainer"]] <- evidencePlotContainer

  if (is.null(network[["network"]]) || plotContainer$getError()) {
    evidencePlotContainer[["dummyPlot"]] <- createJaspPlot(title = gettext("Edge Evidence Plot"), dependencies = "edgeInclusionCriteria")
    return()
  }

  layout <- network[["layout"]] # calculated in .bayesianNetworkAnalysisRun()

  groups <- NULL
  nodeColor <- NULL
  allLegends <- rep(FALSE, nGraphs) # no legends

  if (length(options[["colorGroupVariables"]]) > 1L) {

    assignedGroup <- vapply(options[["colorGroupVariables"]], `[[`, character(1L), "group")

    if (length(unique(assignedGroup)) > 1L) {

      # user has defined groups and there are variables in the groups
      groupNames  <- vapply(options[["manualColorGroups"]], `[[`, character(1L), "name")
      groupColors <- vapply(options[["manualColorGroups"]], `[[`, character(1L), "color")

      nGroups <- length(groupNames)

      idx <- match(assignedGroup, groupNames)

      groups <- vector("list", nGroups)
      names(groups) <- groupNames

      for (i in seq_len(nGroups))
        groups[[i]] <- which(idx == i)

      nonEmpty <- lengths(groups) > 0L
      groups <- groups[nonEmpty]

      if (options[["manualColor"]])
        nodeColor <- groupColors[nonEmpty]

    }
  }

  # defaults
  shape <- "circle"

  # TODO: footnote if legend off and nodenames used
  if (options[["variableNamesShown"]] == "inNodes") {
    nodeNames <- NULL

    if (nGraphs == 1) {
      labels <- colnames(allNetworks[[1L]]$graph)
    } else {
      labels <- colnames(allNetworks$`1`$graph)
    }

  } else {

    if (nGraphs == 1) {
      nodeNames <- colnames(allNetworks[[1L]]$graph)
    } else {
      nodeNames <- colnames(allNetworks$`1`$graph)
    }
    labels <- seq_along(nodeNames)

  }

  labels <- decodeColNames(labels)

  if (options[["labelAbbreviation"]])
    labels <- base::abbreviate(labels, options[["labelAbbreviationLength"]])

  # do we need to draw legends?
  if (!is.null(groups) || !is.null(nodeNames)) {
    if (options[["legend"]] ==  "allPlots") {

      allLegends <- rep(TRUE, nGraphs)

    } else if (options[["legend"]] ==  "specificPlot") {

      if (options[["legendSpecificPlotNumber"]] > nGraphs) {

        allLegends[nGraphs] <- TRUE

      } else if (options[["legendSpecificPlotNumber"]] < 1L) {

        allLegends[1L] <- TRUE

      } else {

        allLegends[options[["legendSpecificPlotNumber"]]] <- TRUE

      }
    }
  }

  names(allLegends) <- names(allNetworks) # allows indexing by name

  basePlotSize <- 320
  legendMultiplier <- options[["legendToPlotRatio"]] * basePlotSize
  height <- setNames(rep(basePlotSize, nGraphs), names(allLegends))
  width  <- basePlotSize + allLegends * legendMultiplier

  for (v in names(allNetworks))
    evidencePlotContainer[[v]] <- createJaspPlot(title = if (nGraphs == 1L) "" else v, width = width[v], height = height[v])

  jaspBase::.suppressGrDevice({

    for (v in names(allNetworks)) {

      networkToPlot <- allNetworks[[v]]

      legend <- allLegends[[v]]
      evidencePlotContainer[[v]]$plotObject <- .bayesianNetworkAnalysisOneEvidencePlot(
        network    = networkToPlot,
        options    = options,
        layout     = layout,
        groups     = groups,
        labels     = labels,
        legend     = legend,
        shape      = shape,
        nodeColor  = nodeColor,
        nodeNames  = nodeNames
      )
    }
  })

}

.bayesianNetworkAnalysisOneEvidencePlot <- function(network, options, layout, groups, labels, legend, shape,
                                                    nodeColor, nodeNames) {

  # Select options for edges (inclusion, exclusion, absence):
  graphColor <- matrix(NA, ncol = nrow(network[["graph"]]), nrow = nrow(network[["graph"]]))
  if (options$edgeInclusion) graphColor[network[["BF"]] >= options[["edgeInclusionCriteria"]]] <- "#36648b"
  if (options$edgeExclusion) graphColor[network[["BF"]] < (1 / options[["edgeInclusionCriteria"]])] <- "#eeb004"
  if (options$edgeAbsence) graphColor[network[["BF"]] < options[["edgeInclusionCriteria"]] & network[["BF"]] > (1 / options[["edgeInclusionCriteria"]])] <- "#bfbfbf"


  # Determine the edges:
  edges <- matrix(ifelse(is.na(graphColor), 0, 1), ncol = nrow(network[["graph"]]), nrow = nrow(network[["graph"]]))

  return(
    qgraph::qgraph(
      input               = edges,
      layout              = layout,
      groups              = groups,
      repulsion           = options[["layoutSpringRepulsion"]],
      cut                 = options[["cut"]],
      edge.width          = options[["edgeSize"]] * 2,
      node.width          = options[["nodeSize"]],
      details             = options[["details"]],
      labels              = labels,
      palette             = if (options[["manualColor"]]) NULL else options[["nodePalette"]],
      legend              = legend,
      shape               = shape,
      color               = nodeColor,
      edge.color          = graphColor,
      nodeNames           = nodeNames,
      label.scale         = options[["labelScale"]],
      label.cex           = options[["labelSize"]],
      GLratio             = 1 / options[["legendToPlotRatio"]],
      edge.labels         = options[["edgeLabels"]],
      edge.label.cex      = options[["edgeLabelSize"]],
      edge.label.position = options[["edgeLabelPosition"]]
    ))
}

.bayesianNetworkAnalysisEdgeEvidenceTable <- function(mainContainer, network, options) {

  if (!is.null(mainContainer[["edgeEvidenceTable"]]) || !options[["edgeEvidenceTable"]])
    return()

  variables <- unlist(options[["variables"]])
  nVar <- length(variables)
  nGraphs <- max(1L, length(network[["network"]]))

  table <- createJaspTable(gettext("Edge evidence probability table"), dependencies = c("edgeEvidenceTable", "evidenceType")) # , position = 4
  table$addColumnInfo(name = "Variable", title = gettext("Variable"), type = "string")

  overTitles <- names(network[["network"]])
  if (is.null(overTitles))
    overTitles <- gettext("Network")

  for (i in seq_len(nGraphs))
    for (v in seq_len(nVar))
      table$addColumnInfo(name = paste0(variables[v], i), title = variables[v], type = "number", overtitle = overTitles[i])

  if (length(options[["variables"]]) <= 2L || is.null(network[["network"]]) || mainContainer$getError()) { # make empty table
    if (nVar > 0L) { # otherwise, a 1 by 1 table with a . is generated by default
      # create a table of nVariables by nVariables
      table$setExpectedSize(nVar)
      table[["Variable"]] <- variables

    }
  } else { # fill with results

    allNetworks <- network[["network"]]
    TBcolumns <- data.frame(Variable = variables)
    for (i in seq_len(nGraphs)) {

      # Check with values to add to the edge evidence table:
      if (options$evidenceType == "inclusionProbability") {
        toAdd <- allNetworks[[i]][["inclusionProbabilities"]]
      } else if (options$evidenceType == "BF10") {
        toAdd <- allNetworks[[i]][["BF"]]
      } else if (options$evidenceType == "BF01") {
        toAdd <- 1 / allNetworks[[i]][["BF"]]
      } else {
        toAdd <- log(allNetworks[[i]][["BF"]])
      }

      toAdd <- as.data.frame(toAdd)
      names(toAdd) <- paste0(variables, i)

      TBcolumns <- cbind(TBcolumns, toAdd)
    }
    table$setData(TBcolumns)
  }

  # add footnote on the infinities only show this message of the evidence type is BF10 or BF01

  if (options$evidenceType %in% c("BF10", "BF01")){
    table$addFootnote(gettext("Bayes factors with values of infinity indicate that the estimated posterior inclusion probability is either 1 or 0. Please see the help file for more information."))
  }
  mainContainer[["edgeEvidenceTable"]] <- table
}

.bayesianNetworkAnalysisEdgeOverviewTable <- function(mainContainer, network, options) {

  if (!is.null(mainContainer[["edgeOverviewTable"]]) || !options[["edgeSpecificOverviewTable"]])
    return()

  threshold   <- options[["edgeSpecificOverviewInclusionCriteria"]]
  allNetworks <- network[["network"]]
  nGraphs     <- max(1L, length(allNetworks))
  hasDifferencesNetwork <- !is.null(allNetworks) && gettext("Differences") %in% names(allNetworks)

  if (nGraphs > 1L && hasDifferencesNetwork) {
    table <- createJaspTable(gettext("Edge Specific Overview"),
                             dependencies = c("edgeSpecificOverviewTable",
                                              "edgeSpecificOverviewInclusionCriteria"))
    table$position <- 2
    mainContainer[["edgeOverviewTable"]] <- table

    if (is.null(allNetworks) || mainContainer$getError())
      return()

    .bayesianNetworkAnalysisFillCombinedEdgeOverviewTable(table, allNetworks, threshold, options)
  } else if (nGraphs > 1L) {
    container <- createJaspContainer(gettext("Edge Specific Overview"),
                                     dependencies = c("edgeSpecificOverviewTable",
                                                       "edgeSpecificOverviewInclusionCriteria"))
    container$position <- 2
    mainContainer[["edgeOverviewTable"]] <- container

    if (is.null(allNetworks) || mainContainer$getError())
      return()

    for (nwName in names(allNetworks)) {
      table         <- createJaspTable(nwName)
      isDifferences <- nwName == gettext("Differences")
      .bayesianNetworkAnalysisFillEdgeOverviewTable(table, allNetworks[[nwName]], threshold, options, isDifferences)
      container[[nwName]] <- table
    }
  } else {
    table <- createJaspTable(gettext("Edge Specific Overview"),
                             dependencies = c("edgeSpecificOverviewTable",
                                               "edgeSpecificOverviewInclusionCriteria"))
    table$position <- 2
    mainContainer[["edgeOverviewTable"]] <- table

    if (is.null(allNetworks) || mainContainer$getError())
      return()

    .bayesianNetworkAnalysisFillEdgeOverviewTable(table, allNetworks[[1]], threshold, options, FALSE)
  }
}

.bayesianNetworkAnalysisFillCombinedEdgeOverviewTable <- function(table, allNetworks, threshold, options) {

  differencesName <- gettext("Differences")
  nwDiff <- allNetworks[[differencesName]]

  variables   <- colnames(nwDiff$estimates)
  nVar        <- length(variables)
  nEdges      <- nVar * (nVar - 1L) / 2L
  decodedVars <- decodeColNames(variables)

  # Upper triangle indices in row-major order
  upperTriIdx <- which(upper.tri(nwDiff$estimates), arr.ind = TRUE)
  upperTriIdx <- upperTriIdx[order(upperTriIdx[, 1], upperTriIdx[, 2]), ]

  relation      <- character(nEdges)
  diffEstimate  <- numeric(nEdges)
  inclusionProb <- numeric(nEdges)
  inclusionBF   <- numeric(nEdges)
  category      <- character(nEdges)

  for (k in seq_len(nEdges)) {
    i <- upperTriIdx[k, 1]
    j <- upperTriIdx[k, 2]

    relation[k]      <- paste0(decodedVars[j], "-", decodedVars[i])
    diffEstimate[k]  <- nwDiff$graph[i, j]
    inclusionProb[k] <- nwDiff$inclusionProbabilities[i, j]
    inclusionBF[k]   <- nwDiff$BF[i, j]

    if (inclusionBF[k] >= threshold)
      category[k] <- gettext("difference")
    else if (inclusionBF[k] <= 1 / threshold)
      category[k] <- gettext("equal")
    else
      category[k] <- gettext("inconclusive")
  }

  table$addColumnInfo(name = "relation",         title = gettext("Relation"),              type = "string")
  table$addColumnInfo(name = "differenceEstimate", title = gettext("Difference Estimate"),   type = "number")
  table$addColumnInfo(name = "inclusionProb",    title = gettext("Posterior Diff. Prob."), type = "number")
  table$addColumnInfo(name = "inclusionBF",      title = gettext("Difference BF"),          type = "number")
  table$addColumnInfo(name = "category",         title = gettext("Category"),              type = "string")

  df <- data.frame(
    relation           = relation,
    differenceEstimate = diffEstimate,
    inclusionProb      = inclusionProb,
    inclusionBF        = inclusionBF,
    category           = category,
    stringsAsFactors   = FALSE
  )

  # Try to add convergence from posterior samples in the difference network
  convergence <- .bayesianNetworkAnalysisComputeEdgeConvergence(nwDiff, upperTriIdx, nEdges, as.integer(options[["chains"]]))
  if (!is.null(convergence)) {
    table$addColumnInfo(name = "convergence", title = gettext("Convergence"), type = "number")
    df$convergence <- convergence
  }

  estimateNetworkNames <- setdiff(names(allNetworks), differencesName)
  estimateNetworkNames <- estimateNetworkNames[estimateNetworkNames != ""]

  for (nwName in estimateNetworkNames) {
    columnName <- paste0("estimate_", make.names(nwName))
    table$addColumnInfo(name = columnName, title = gettext("Estimate"), type = "number", overtitle = nwName)

    nw <- allNetworks[[nwName]]
    edgeEstimates <- numeric(nEdges)
    for (k in seq_len(nEdges)) {
      i <- upperTriIdx[k, 1]
      j <- upperTriIdx[k, 2]
      edgeEstimates[k] <- nw$graph[i, j]
    }
    df[[columnName]] <- edgeEstimates
  }

  table$addFootnote(gettext("Difference estimates are based on the median probability model: edges with a posterior difference probability \u2264 0.5 are set to zero."))
  table$addFootnote(gettext("Bayes factors with values of infinity indicate that the estimated posterior difference probability is either 1 or 0. Please see the help file for more information."))
  if (!is.null(convergence)) {
    if (as.integer(options[["chains"]]) >= 2L)
      table$addFootnote(gettext("Convergence is the R-hat (Gelman-Rubin) statistic, values greater than about 1.01-1.05 are considered concerning, indicating potential lack of convergence for the estimates of the pairwise interactions. Consider increasing the number of iterations and/or chains to improve convergence."))
    else
      table$addFootnote(gettext("Convergence is the split-chain R-hat (Gelman-Rubin) statistic, computed post hoc by splitting the posterior samples into two halves."))
  }

  table$setData(df)
}

.bayesianNetworkAnalysisFillEdgeOverviewTable <- function(table, nw, threshold, options, isDifferences = FALSE) {

  variables   <- colnames(nw$estimates)
  nVar        <- length(variables)
  nEdges      <- nVar * (nVar - 1L) / 2L
  decodedVars <- decodeColNames(variables)

  # Upper triangle indices in row-major order
  upperTriIdx <- which(upper.tri(nw$estimates), arr.ind = TRUE)
  upperTriIdx <- upperTriIdx[order(upperTriIdx[, 1], upperTriIdx[, 2]), ]

  relation      <- character(nEdges)
  estimate      <- numeric(nEdges)
  inclusionProb <- numeric(nEdges)
  inclusionBF   <- numeric(nEdges)
  category      <- character(nEdges)

  for (k in seq_len(nEdges)) {
    i <- upperTriIdx[k, 1]
    j <- upperTriIdx[k, 2]

    relation[k]      <- paste0(decodedVars[j], "-", decodedVars[i])
    estimate[k]      <- nw$graph[i, j]
    inclusionProb[k] <- nw$inclusionProbabilities[i, j]
    inclusionBF[k]   <- nw$BF[i, j]

    if (inclusionBF[k] >= threshold)
      category[k] <- if (isDifferences) gettext("difference") else gettext("included")
    else if (inclusionBF[k] <= 1 / threshold)
      category[k] <- if (isDifferences) gettext("equal")      else gettext("excluded")
    else
      category[k] <- gettext("inconclusive")
  }

  table$addColumnInfo(name = "relation",      title = gettext("Relation"),              type = "string")
  table$addColumnInfo(name = "estimate",      title = gettext("Estimate"),              type = "number")
  table$addColumnInfo(name = "inclusionProb", title = if (isDifferences) gettext("Posterior Diff. Prob.") else gettext("Posterior Incl. Prob."), type = "number")
  table$addColumnInfo(name = "inclusionBF",   title = if (isDifferences) gettext("Difference BF")         else gettext("Inclusion BF"),          type = "number")
  table$addColumnInfo(name = "category",      title = gettext("Category"),              type = "string")

  df <- data.frame(
    relation      = relation,
    estimate      = estimate,
    inclusionProb = inclusionProb,
    inclusionBF   = inclusionBF,
    category      = category,
    stringsAsFactors = FALSE
  )

  # Try to add convergence from posterior samples
  convergence <- .bayesianNetworkAnalysisComputeEdgeConvergence(nw, upperTriIdx, nEdges, as.integer(options[["chains"]]))
  if (!is.null(convergence)) {
    table$addColumnInfo(name = "convergence", title = gettext("Convergence"), type = "number")
    df$convergence <- convergence
  }

  table$addFootnote(gettext("Estimates are based on the median probability model: edges with a posterior inclusion probability \u2264 0.5 are set to zero."))
  table$addFootnote(gettext("Bayes factors with values of infinity indicate that the estimated posterior inclusion probability is either 1 or 0. Please see the help file for more information."))
  if (!is.null(convergence)) {
    if (as.integer(options[["chains"]]) >= 2L)
      table$addFootnote(gettext("Convergence is the R-hat (Gelman-Rubin) statistic, with values greater than about 1.01-1.05 are considered concerning, indicating potential lack of convergence for the estimates of the pairwise interactions. Consider increasing the number of iterations to improve convergence."))
    else
      table$addFootnote(gettext("Convergence is the split-chain R-hat (Gelman-Rubin) statistic, computed post hoc by splitting the posterior samples into two halves. Values greater than about 1.01-1.05 are considered concerning, indicating potential lack of convergence for the estimates of the pairwise interactions. Consider increasing the number of iterations to improve convergence."))
  }
  table$setData(df)
}

.bayesianNetworkAnalysisInterpretativeScale <- function(mainContainer, network, options) {

  if (!is.null(mainContainer[["interpretativeScaleContainer"]]) ||
      !options[["edgeSpecificOverviewTable"]] ||
      !options[["showInterpretativeScaleEstimates"]])
    return()

  container <- createJaspContainer(
    dependencies = c("edgeSpecificOverviewTable", "showInterpretativeScaleEstimates")
  )
  container$position <- 3
  mainContainer[["interpretativeScaleContainer"]] <- container

  allNetworks <- network[["network"]]
  if (is.null(allNetworks) || mainContainer$getError())
    return()

  nGraphs <- length(allNetworks)

  if (nGraphs > 1L) {
    for (nwName in names(allNetworks)) {
      nwContainer <- createJaspContainer(nwName)
      container[[nwName]] <- nwContainer
      .bayesianNetworkAnalysisAddInterpretativeScaleTables(nwContainer, allNetworks[[nwName]])
    }
  } else {
    .bayesianNetworkAnalysisAddInterpretativeScaleTables(container, allNetworks[[1L]])
  }
}

.bayesianNetworkAnalysisAddInterpretativeScaleTables <- function(container, nw) {

  .bayesianNetworkAnalysisAddInterpretativeScaleMatrix(
    container   = container,
    nw          = nw,
    matrixName  = gettext("Log-odds"),
    matrixKey   = "logOdds",
    position    = 1
  )

  .bayesianNetworkAnalysisAddInterpretativeScaleMatrix(
    container   = container,
    nw          = nw,
    matrixName  = gettext("Precision Matrix"),
    matrixKey   = "precisionMatrix",
    position    = 2
  )

  .bayesianNetworkAnalysisAddInterpretativeScaleMatrix(
    container   = container,
    nw          = nw,
    matrixName  = gettext("Partial Correlations"),
    matrixKey   = "partialCorrelations",
    position    = 3
  )
}

.bayesianNetworkAnalysisAddInterpretativeScaleMatrix <- function(container, nw, matrixName, matrixKey, position) {

  matrix <- nw[[matrixKey]]
  key <- paste0(matrixKey, "Table")

  if (is.null(matrix))
    return()

  matrix <- .bayesianNetworkAnalysisApplyMedianProbabilityMask(matrix, nw$inclusionProbabilities)

  table <- createJaspTable(matrixName)
  table$position <- position

  df <- .bayesianNetworkAnalysisMatrixToDataFrame(matrix)

  table$addColumnInfo(name = "Variable", title = gettext("Variable"), type = "string")

  valueColumns <- setdiff(colnames(df), "Variable")
  for (columnName in valueColumns)
    table$addColumnInfo(name = columnName, title = columnName, type = "number")

  table$setData(df)
  container[[key]] <- table
}

.bayesianNetworkAnalysisApplyMedianProbabilityMask <- function(matrix, inclusionProbabilities) {

  if (is.null(matrix) || is.null(inclusionProbabilities))
    return(matrix)

  maskedMatrix <- as.matrix(matrix)
  inclusionMatrix <- as.matrix(inclusionProbabilities)

  if (all(dim(maskedMatrix) == dim(inclusionMatrix))) {
    mask <- inclusionMatrix <= 0.5
  } else {
    matrixRows <- rownames(maskedMatrix)
    matrixCols <- colnames(maskedMatrix)
    inclusionRows <- rownames(inclusionMatrix)
    inclusionCols <- colnames(inclusionMatrix)

    hasMatchingNames <- !is.null(matrixRows) && !is.null(matrixCols) &&
      !is.null(inclusionRows) && !is.null(inclusionCols)

    if (!hasMatchingNames)
      return(maskedMatrix)

    rowIndex <- match(matrixRows, inclusionRows)
    colIndex <- match(matrixCols, inclusionCols)

    if (any(is.na(rowIndex)) || any(is.na(colIndex)))
      return(maskedMatrix)

    mask <- inclusionMatrix[rowIndex, colIndex, drop = FALSE] <= 0.5
  }

  mask[is.na(mask)] <- FALSE
  maskedMatrix[mask] <- 0
  maskedMatrix
}

.bayesianNetworkAnalysisMatrixToDataFrame <- function(matrix) {

  matrix <- as.matrix(matrix)

  variableNames <- colnames(matrix)
  if (is.null(variableNames))
    variableNames <- rownames(matrix)

  if (is.null(variableNames))
    variableNames <- paste0("V", seq_len(ncol(matrix)))

  decodedNames <- decodeColNames(variableNames)

  rowNames <- rownames(matrix)
  if (is.null(rowNames))
    rowNames <- variableNames

  decodedRowNames <- decodeColNames(rowNames)

  df <- as.data.frame(matrix, stringsAsFactors = FALSE)
  colnames(df) <- decodedNames

  cbind(Variable = decodedRowNames, df, stringsAsFactors = FALSE)
}

.bayesianNetworkAnalysisComputeEdgeConvergence <- function(nw, upperTriIdx, nEdges, nChains = 1L) {

  posteriorSamples <- nw$samplesPosterior
  if (is.null(posteriorSamples) || ncol(posteriorSamples) < nEdges || nrow(posteriorSamples) < 4L)
    return(NULL)

  # Build column-major upper triangle mapping to find the right column in samplesPosterior
  nVar <- nrow(nw$estimates)
  cmIdx <- which(upper.tri(matrix(NA, nVar, nVar)), arr.ind = TRUE)

  cmLookup <- matrix(NA_integer_, nVar, nVar)
  for (pos in seq_len(nrow(cmIdx)))
    cmLookup[cmIdx[pos, 1], cmIdx[pos, 2]] <- pos

  convergence <- numeric(nEdges)
  for (k in seq_len(nEdges)) {
    i <- upperTriIdx[k, 1]
    j <- upperTriIdx[k, 2]
    col <- cmLookup[i, j]

    if (!is.na(col) && col <= ncol(posteriorSamples))
      convergence[k] <- .bayesianNetworkAnalysisRhat(posteriorSamples[, col], nChains)
    else
      convergence[k] <- NA_real_
  }

  convergence
}

.bayesianNetworkAnalysisRhat <- function(samples, nChains = 1L) {
  n <- length(samples)
  if (n < 4L) return(NA_real_)

  # Use nChains segments when nChains >= 2; split single chain in half otherwise
  nSegments     <- if (nChains >= 2L) nChains else 2L
  segmentLength <- n %/% nSegments
  if (segmentLength < 2L) return(NA_real_)

  chainSamples <- lapply(seq_len(nSegments), function(i)
    samples[((i - 1L) * segmentLength + 1L):(i * segmentLength)])

  chainMeans <- vapply(chainSamples, mean, numeric(1L))
  chainVars  <- vapply(chainSamples, var,  numeric(1L))

  W <- mean(chainVars)
  B <- segmentLength * var(chainMeans)

  if (W < .Machine$double.eps) return(NA_real_)

  varhat <- ((segmentLength - 1L) / segmentLength) * W + (1L / segmentLength) * B
  sqrt(varhat / W)
}

.bayesianNetworkAnalysisOneStructurePlot <- function(network, options, layout,
                                                     groups, labels, legend, shape, nodeColor, nodeNames) {

  return(
    qgraph::qgraph(
      input               = network[["structure"]],
      layout              = layout,
      groups              = groups,
      repulsion           = options[["layoutSpringRepulsion"]],
      cut                 = options[["cut"]],
      edge.width          = options[["edgeSize"]],
      node.width          = options[["nodeSize"]],
      details             = options[["details"]],
      labels              = labels,
      palette             = if (options[["manualColor"]]) NULL else options[["nodePalette"]],
      legend              = legend,
      shape               = shape,
      color               = nodeColor,
      nodeNames           = nodeNames,
      label.scale         = options[["labelScale"]],
      label.cex           = options[["labelSize"]],
      GLratio             = 1 / options[["legendToPlotRatio"]],
      edge.labels         = options[["edgeLabels"]],
      edge.label.cex      = options[["edgeLabelSize"]],
      edge.label.position = options[["edgeLabelPosition"]]
    ))
}

# =========================
#  STOCHASTIC BLOCK MODEL OUTPUT
# =========================

.bayesianNetworkAnalysisSbmAllocationsTable <- function(mainContainer, network, options) {

  if (!is.null(mainContainer[["sbmAllocationsTable"]]) || !options[["clusterAllocationsTable"]] || !.bayesianNetworkAnalysisStochasticBlockAllowed(options))
    return()

  type <- options[["clusterAllocationsType"]]
  if (type == "mean") {
    tableTitle <- gettext("Cluster Allocations (Posterior Mean)")
    sbmField   <- "posterior_mean_allocations"
  } else {
    tableTitle <- gettext("Cluster Allocations (Posterior Mode)")
    sbmField   <- "posterior_mode_allocations"
  }

  table <- createJaspTable(tableTitle, dependencies = c("clusterAllocationsTable", "clusterAllocationsType", "edgePrior"))
  table$position <- 10
  table$addColumnInfo(name = "variable",   title = gettext("Variable"),   type = "string")
  table$addColumnInfo(name = "allocation", title = gettext("Cluster"),    type = "integer")

  mainContainer[["sbmAllocationsTable"]] <- table

  if (is.null(network[["network"]]) || mainContainer$getError())
    return()

  allNetworks <- network[["network"]]
  for (i in seq_along(allNetworks)) {
    nw <- allNetworks[[i]]
    if (!is.null(nw$sbm)) {
      allocations <- nw$sbm[[sbmField]]
      variables   <- colnames(nw$estimates)
      df <- data.frame(variable = variables, allocation = as.integer(allocations))
      table$setData(df)
    }
  }
}

.bayesianNetworkAnalysisSbmNumBlocksTable <- function(mainContainer, network, options) {

  if (!is.null(mainContainer[["sbmNumBlocksTable"]]) || !options[["posteriorNumBlocksTable"]] || !.bayesianNetworkAnalysisStochasticBlockAllowed(options))
    return()

  table <- createJaspTable(gettext("Posterior Probabilities for the Number of Clusters"),
                           dependencies = c("posteriorNumBlocksTable", "edgePrior"))
  table$position <- 11
  table$addColumnInfo(name = "numBlocks",   title = gettext("Number of blocks"),    type = "integer")
  table$addColumnInfo(name = "probability", title = gettext("Posterior probability"), type = "number")

  mainContainer[["sbmNumBlocksTable"]] <- table

  if (is.null(network[["network"]]) || mainContainer$getError())
    return()

  allNetworks <- network[["network"]]
  for (i in seq_along(allNetworks)) {
    nw <- allNetworks[[i]]
    if (!is.null(nw$sbm)) {
      posteriorNumBlocks <- nw$sbm$posterior_num_blocks
      # posterior_num_blocks is a single-column data frame (list of length 1)
      df <- data.frame(
        numBlocks   = seq_len(nrow(posteriorNumBlocks)),
        probability = as.numeric(posteriorNumBlocks[[1]])
      )
      table$setData(df)
    }
  }
}

.bayesianNetworkAnalysisSbmCoclusteringTable <- function(mainContainer, network, options) {

  if (!is.null(mainContainer[["sbmCoclusteringTable"]]) || !options[["posteriorCoclusteringMatrixTable"]] || !.bayesianNetworkAnalysisStochasticBlockAllowed(options))
    return()

  variables <- unlist(options[["variables"]])
  nVar <- length(variables)

  table <- createJaspTable(gettext("Posterior Co-clustering Matrix"),
                           dependencies = c("posteriorCoclusteringMatrixTable", "edgePrior"))
  table$position <- 12
  table$addColumnInfo(name = "Variable", title = gettext("Variable"), type = "string")
  for (v in seq_len(nVar))
    table$addColumnInfo(name = variables[v], title = variables[v], type = "number")

  mainContainer[["sbmCoclusteringTable"]] <- table

  if (is.null(network[["network"]]) || mainContainer$getError())
    return()

  allNetworks <- network[["network"]]
  for (i in seq_along(allNetworks)) {
    nw <- allNetworks[[i]]
    if (!is.null(nw$sbm)) {
      coclust <- as.data.frame(nw$sbm$posterior_mean_coclustering_matrix)
      colnames(coclust) <- variables
      coclust <- cbind(Variable = variables, coclust)
      table$setData(coclust)
    }
  }
}

.bayesianNetworkAnalysisSbmClusterBayesFactor <- function(mainContainer, network, options) {

  if (!is.null(mainContainer[["sbmClusterBayesFactorTable"]]) || !options[["clusterBayesFactor"]] || !.bayesianNetworkAnalysisStochasticBlockAllowed(options))
    return()

  bfType <- options[["clusterBayesFactorType"]]

  table <- createJaspTable(gettext("Cluster Bayes Factor"),
                           dependencies = c("clusterBayesFactor", "clusterBayesFactorType",
                                            "clusterBayesFactorB1", "clusterBayesFactorB2", "edgePrior"))
  table$position <- 13

  if (bfType == "complement") {
    table$addColumnInfo(name = "hypothesis", title = gettext("Hypothesis"),   type = "string")
    table$addColumnInfo(name = "bf",         title = gettext("Bayes factor"), type = "number")
  } else {
    table$addColumnInfo(name = "b1",         title = "H\u2081",              type = "integer")
    table$addColumnInfo(name = "b2",         title = "H\u2082",              type = "integer")
    table$addColumnInfo(name = "bf",         title = "BF\u2081\u2082",      type = "number")
  }

  mainContainer[["sbmClusterBayesFactorTable"]] <- table

  if (is.null(network[["network"]]) || mainContainer$getError())
    return()

  allNetworks <- network[["network"]]
  for (i in seq_along(allNetworks)) {
    nw <- allNetworks[[i]]
    if (!is.null(nw$easybgmFit)) {

      if (bfType == "complement") {
        bf <- try(easybgm::clusterBayesfactor(nw$easybgmFit, type = "complement"))
        if (isTryError(bf)) {
          table$setError(gettextf("Could not compute the cluster Bayes factor: %s", .extractErrorMessage(bf)))
          return()
        }
        df <- data.frame(hypothesis = gettext("Clustering vs. no clustering"), bf = as.numeric(bf))
      } else {
        b1 <- options[["clusterBayesFactorB1"]]
        b2 <- options[["clusterBayesFactorB2"]]
        bf <- try(easybgm::clusterBayesfactor(nw$easybgmFit, type = "point", b1 = b1, b2 = b2))
        if (isTryError(bf)) {
          table$setError(gettextf("Could not compute the cluster Bayes factor: %s", .extractErrorMessage(bf)))
          return()
        }
        df <- data.frame(b1 = b1, b2 = b2, bf = as.numeric(bf))
      }
      table$setData(df)
    }
  }
}

.bayesianNetworkAnalysisSbmCoclusteringPlot <- function(plotContainer, network, options) {

  # Accept either plotContainer or mainContainer — the function is called from
  # .bayesianNetworkAnalysisPlotContainer (plotContainer) and also standalone.
  # When called standalone from the main function, plotContainer == mainContainer and
  # we nest into a plot container ourselves.

  if (!is.null(plotContainer[["sbmCoclusteringPlotContainer"]]) || !options[["coclusteringPlot"]] || !.bayesianNetworkAnalysisStochasticBlockAllowed(options))
    return()

  allNetworks <- network[["network"]]
  nGraphs     <- length(allNetworks)

  title <- if (nGraphs == 1L) gettext("Co-clustering Matrix Plot") else gettext("Co-clustering Matrix Plots")

  coclusteringPlotContainer <- createJaspContainer(title = title,
                                                   dependencies = c("coclusteringPlot", "edgePrior"))
  plotContainer[["sbmCoclusteringPlotContainer"]] <- coclusteringPlotContainer

  if (is.null(network[["network"]]) || plotContainer$getError()) {
    coclusteringPlotContainer[["dummyPlot"]] <- createJaspPlot(title = gettext("Co-clustering Matrix Plot"))
    return()
  }

  for (v in names(allNetworks))
    coclusteringPlotContainer[[v]] <- createJaspPlot(title = v, width = 480, height = 400)

  for (v in names(allNetworks)) {
    nw <- allNetworks[[v]]
    if (!is.null(nw$sbm)) {
      coclust   <- nw$sbm$posterior_mean_coclustering_matrix
      variables <- colnames(nw$estimates)
      colnames(coclust) <- variables
      rownames(coclust) <- variables

      # Reshape to long format for ggplot
      dfLong <- expand.grid(Var1 = variables, Var2 = variables)
      dfLong$value <- as.vector(coclust)
      # Preserve variable ordering
      dfLong$Var1 <- factor(dfLong$Var1, levels = variables)
      dfLong$Var2 <- factor(dfLong$Var2, levels = rev(variables))

      plot <- ggplot2::ggplot(dfLong, ggplot2::aes(x = Var1, y = Var2, fill = value)) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::scale_fill_gradient(low = "white", high = "#36648B",
                                     limits = c(0, 1),
                                     name = gettext("Probability")) +
        ggplot2::labs(x = NULL, y = NULL) +
        jaspGraphs::themeJaspRaw() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

      coclusteringPlotContainer[[v]]$plotObject <- plot
    }
  }
}


# =========================
#  ADDITIONAL FUNCTIONS
# =========================

# Turns vector into matrix:
vectorToMatrix <- function(vec, p, diag = FALSE, bycolumn = FALSE) {
  m <- matrix(0, p, p)

  if(bycolumn == F){
    m[lower.tri(m, diag = diag)] <- vec
    m <- t(m)
    m[lower.tri(m)] <- t(m)[lower.tri(m)]
  } else {
    m[upper.tri(m, diag = diag)] <- vec
    m <- t(m)
    m[upper.tri(m)] <- t(m)[upper.tri(m)]
  }
  return(m)
}

# Transform precision into partial correlations for interpretation:
pr2pc <- function(K) {
  R <- diag(2, nrow(K)) - stats::cov2cor(K)
  colnames(R) <- colnames(K)
  rownames(R) <- rownames(K)
  return(R)
}

# BDgraph stores graphs as byte strings for efficiency:
string2graph <- function(Gchar, p) {
  Gvec <- rep(0, p*(p-1)/2)
  edges <- which(unlist(strsplit(as.character(Gchar), "", fixed = TRUE)) == 1)
  Gvec[edges] = 1
  G <- matrix(0, p, p)
  G[upper.tri(G)] <- Gvec
  G <- G + t(G)
  return(G)
}

# BDgraph extract posterior distribution for estimates:
extractposterior <- function(fit, data, method = c("ggm", "gcgm"), nonContVariables, options) {

  m <- length(fit$all_graphs)
  n <- nrow(as.matrix(data))
  p <- ncol(as.matrix(data))

  # Number of samples from posterior:
  k <- as.numeric(options[["iter"]])
  densities <- rep(0, k)
  Rs <- matrix(0, nrow = k, ncol = (p*(p-1))/2)

  if (method == "gcgm") {
    S <- BDgraph::get_S_n_p(data, method = method, n = n, not.cont = nonContVariables)$S
  } else {
    S <- t(as.matrix(data)) %*% as.matrix(data)
  }

  j <- 1
  for (i in seq(1, m, length.out = k)) {
    graph_ix <- fit$all_graphs[i]
    G <- string2graph(fit$sample_graphs[graph_ix], p)
    K <- BDgraph::rgwish(n = 1, adj = G, b = 3 + n, D = diag(p) + S)
    Rs[j,] <- as.vector(pr2pc(K)[upper.tri(pr2pc(K))])
    densities[j] <- sum(sum(G)) / (p*(p-1))
    j <- j + 1
  }

  return(list(Rs, densities))
}

# Samples from the G-wishart distribution:
gwish_samples <- function(G, S, nSamples = 1000) {

  p <- nrow(S)
  Rs = matrix(0, nrow = nSamples, ncol = (p*(p-1))/2)

  for (i in 1:nSamples) {
    K <- BDgraph::rgwish(n = 1, adj = G, b = 3 + n, D = diag(p) + S) * (G + diag(p))
    Rs[i,] <- as.vector(pr2pc(K)[upper.tri(pr2pc(K))])
  }

  return(Rs)
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# Centrality of weighted graphs
centrality <- function(network, measures = c("closeness", "betweenness", "strength", "expectedInfluence"), options) {

  measures <- firstup(measures)

  graph <- qgraph::centralityPlot(unname(as.matrix(network$estimates)),
                                  include = measures,
                                  verbose = FALSE,
                                  print = FALSE,
                                  scale = "z-scores",
                                  labels = colnames(network$estimates))

  centralityOutput <- graph$data[, c("node", "measure", "value")]
  colnames(centralityOutput) <- c("node", "measure", "posteriorMeans")

  if (options[["credibilityInterval"]]) {

    # Compute centrality for each posterior sample:
    for (i in seq_len(nrow(network$samplesPosterior))) {

      graph <- qgraph::centralityPlot(vectorToMatrix(network$samplesPosterior[i, ], as.numeric(nrow(network$estimates)), bycolumn = TRUE),
                                      include = measures,
                                      verbose = FALSE,
                                      print = FALSE,
                                      scale = "z-scores",
                                      labels = colnames(network$estimates))

      # Strength is removed if all values are 0. Here we fix this by setting the value to 0 manually
      # see https://github.com/jasp-stats/jasp-test-release/issues/2298
      if (nrow(graph$data) != nrow(centralityOutput) &&
          "Strength" %in% measures &&
          all(abs(network$samplesPosterior[i, ]) <= .Machine$double.eps)) {

        idx <- centralityOutput$measure %in% graph$data$measure
        value <- numeric(nrow(centralityOutput))
        value[idx] <- graph$data$value
      } else {
        value <- graph$data[["value"]]
      }
      centralityOutput <- cbind(centralityOutput, value)
    }
  }

  centralityOutput$posteriorMeans <- ifelse(is.na(centralityOutput$posteriorMeans), 0, centralityOutput$posteriorMeans)

  return(centralityOutput)
}
