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
  .bayesianNetworkAnalysisBlumeCapelTable    (mainContainer, network, options)
  .bayesianNetworkAnalysisInterpretativeScale(mainContainer, network, options)
  .bayesianNetworkAnalysisParameterHdiTable  (mainContainer, network, options)
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
                                                          "burnin", "iter", "seed", "gPrior",
                                                          "edgePrior",
                                                          "interactionPriorFamily", "interactionScale",
                                                          "interactionAlpha", "interactionBeta",
                                                          "interactionScaleBaseline",
                                                          "differencePriorFamily",
                                                          "betaAlpha", "betaBeta",
                                                          "betaAlpha_between", "betaBeta_between",
                                                          "lambda", "dirichletAlpha",
                                                          "thresholdPriorFamily",
                                                          "thresholdAlpha", "thresholdBeta",
                                                          "thresholdScale",
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

      # centralitySamples is in wide format: node, measure and the centrality of the
      # posterior mean network come first, followed by one column per posterior sample.
      draws <- as.matrix(centralitySamples[, -(1:3), drop = FALSE])

      posteriorMeans <- rowMeans(draws)

      centralityHDIintervals <- apply(draws, MARGIN = 1,
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
  # A comparison of more than two groups has no single difference per edge, so
  # its network does not contribute to the layout.
  weightMatrices <- unname(Filter(function(graph) !is.null(graph) && !anyNA(graph), lapply(networks, `[[`, "graph")))
  jaspBase::.suppressGrDevice(layout <- qgraph::averageLayout(weightMatrices, layout = options[["layout"]], repulsion = options[["layoutSpringRepulsion"]]))
  rownames(layout) <- colnames(networks[[1L]])

  return(layout)
}

# The QML variables list carries a row component, so JASP delivers one entry per
# assigned variable: list(variable = "x", blumeCapel = TRUE, levels = "2"). The rest of
# the analysis works with a plain vector of names plus the Blume-Capel rows.
.bayesianNetworkAnalysisNormalizeVariableOptions <- function(options) {

  variableRows <- options[["variables"]]

  options[["variables"]] <- vapply(variableRows, `[[`, character(1L), "variable")

  blumeCapelRows <- Filter(function(row) isTRUE(row[["blumeCapel"]]), variableRows)

  options[["variablesBlumeCapel"]] <- lapply(blumeCapelRows, function(row) {
    list(
      variable = row[["variable"]],
      levels   = row[["levels"]]
    )
  })

  return(options)
}

.bayesianNetworkAnalysisResolveBaselineCategory <- function(variableName, variableData, baselineValue) {

  if (is.null(baselineValue) || length(baselineValue) == 0L || identical(baselineValue, ""))
    return(1L)

  # The QML supplies the level label, while bgms expects the category index on the
  # 1..k scale it derives from the factor. Match the label first: for a variable
  # with levels "2", "4", "6" the label "4" is category 2, not category 4, and
  # passing 4 makes bgms reject the baseline as outside the observed scores.
  if (is.factor(variableData)) {
    baselineCategory <- match(as.character(baselineValue), levels(variableData))
    if (!is.na(baselineCategory))
      return(baselineCategory)
  }

  baselineCategory <- suppressWarnings(as.integer(baselineValue))
  nCategories      <- if (is.factor(variableData)) nlevels(variableData) else NA_integer_

  if (!is.na(baselineCategory) && baselineCategory >= 1L &&
      (is.na(nCategories) || baselineCategory <= nCategories))
    return(baselineCategory)

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

  variablesBlumeCapel <- options[["variablesBlumeCapel"]]

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

.bayesianNetworkAnalysisAssertInteractionPriorSupported <- function(options, variableSpec) {

  # bgms places a hierarchical prior on the precision graph of continuous
  # variables, which supports only a Normal or Cauchy slab.
  family <- .bayesianNetworkAnalysisNormalizePriorFamily(options[["interactionPriorFamily"]], default = "normal")
  if (family == "beta-prime" && any(variableSpec[["type"]] == "continuous")) {
    .quitAnalysis(gettext(
      "The Beta-prime prior on the partial association parameters is not available when continuous variables are included. Please select Normal or Cauchy."
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

.bayesianNetworkAnalysisBuildParameterPrior <- function(family, scale, alpha, beta, priorRole) {

  family <- .bayesianNetworkAnalysisNormalizePriorFamily(family, default = if (priorRole == "interaction") "normal" else "beta-prime")

  switch(family,
    "normal"     = bgms::normal_prior(scale = scale),
    "cauchy"     = bgms::cauchy_prior(scale = scale),
    "beta-prime" = bgms::beta_prime_prior(alpha = alpha, beta = beta)
  )
}

.bayesianNetworkAnalysisNormalizePriorFamily <- function(value, default) {

  if (is.null(value) || length(value) == 0L)
    return(default)

  token <- trimws(strsplit(as.character(value)[1L], ",", fixed = TRUE)[[1L]][1L])
  if (is.na(token) || token %in% c("", "NA", "NULL"))
    return(default)

  tolower(gsub("[[:space:]_]+", "-", token))
}

.bayesianNetworkAnalysisBuildEdgePrior <- function(options) {

  switch(options[["edgePrior"]],
    "Bernoulli" = bgms::bernoulli_prior(
      inclusion_probability = options[["gPrior"]]
    ),
    "Beta-Bernoulli" = bgms::beta_bernoulli_prior(
      alpha = options[["betaAlpha"]],
      beta  = options[["betaBeta"]]
    ),
    "Stochastic-Block" = bgms::sbm_prior(
      alpha           = options[["betaAlpha"]],
      beta            = options[["betaBeta"]],
      alpha_between   = options[["betaAlpha_between"]],
      beta_between    = options[["betaBeta_between"]],
      dirichlet_alpha = options[["dirichletAlpha"]],
      lambda          = options[["lambda"]]
    )
  )
}

.bayesianNetworkAnalysisBuildDifferencePrior <- function(options) {

  switch(options[["edgePrior"]],
    "Bernoulli" = bgms::bernoulli_prior(
      inclusion_probability = options[["gPrior"]]
    ),
    "Beta-Bernoulli" = bgms::beta_bernoulli_prior(
      alpha = options[["betaAlpha"]],
      beta  = options[["betaBeta"]]
    )
  )
}

.bayesianNetworkAnalysisBuildDifferenceFamily <- function(options) {

  family <- .bayesianNetworkAnalysisNormalizePriorFamily(options[["differencePriorFamily"]], default = "normal")

  # bgmCompare takes the slab on the group differences as a family name rather
  # than as a prior object, and expects it capitalized.
  switch(family,
    "normal" = "Normal",
    "cauchy" = "Cauchy"
  )
}

.bayesianNetworkAnalysisBuildInteractionPrior <- function(options) {

  .bayesianNetworkAnalysisBuildParameterPrior(
    family    = options[["interactionPriorFamily"]],
    scale     = options[["interactionScale"]],
    alpha     = options[["interactionAlpha"]],
    beta      = options[["interactionBeta"]],
    priorRole = "interaction"
  )
}

.bayesianNetworkAnalysisBuildThresholdPrior <- function(options) {

  .bayesianNetworkAnalysisBuildParameterPrior(
    family    = options[["thresholdPriorFamily"]],
    scale     = options[["thresholdScale"]],
    alpha     = options[["thresholdAlpha"]],
    beta      = options[["thresholdBeta"]],
    priorRole = "threshold"
  )
}

.bayesianNetworkAnalysisFitSingleNetwork <- function(data, variableSpec, options, progressLabel = "") {

  updateMethod <- .bayesianNetworkAnalysisNormalizeUpdateMethod(options[["omrfUpdateMethod"]])

  interactionPrior <- .bayesianNetworkAnalysisBuildInteractionPrior(options)
  thresholdPrior   <- .bayesianNetworkAnalysisBuildThresholdPrior(options)
  edgePrior        <- .bayesianNetworkAnalysisBuildEdgePrior(options)

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
    interaction_prior            = interactionPrior,
    threshold_prior              = thresholdPrior,
    edge_prior                   = edgePrior,
    progress_callback            = .bayesianNetworkAnalysisMakeProgressCallback(progressLabel)
  ))

  if (isTryError(easybgmFit)) {
    message <- .extractErrorMessage(easybgmFit)
    .quitAnalysis(gettextf("The analysis failed with the following error message:\n%s", message))
  }

  easybgmFit
}

.bayesianNetworkAnalysisExtractEasybgmResult <- function(easybgmFit, variableSpec, options, keepRawFit = FALSE,
                                                         isDifferenceFit = FALSE) {

  easybgmResult <- list()

  variables <- colnames(easybgmFit$inc_probs)
  if (is.null(variables))
    variables <- colnames(easybgmFit$parameters)

  estimates <- easybgmFit$parameters
  if (is.null(estimates)) {
    # A comparison of more than two groups has no single difference per edge;
    # its pairwise group differences are stored below instead.
    estimates <- matrix(NA_real_, length(variables), length(variables), dimnames = list(variables, variables))
  }

  easybgmResult$inclusionProbabilities <- easybgmFit$inc_probs
  easybgmResult$BF                     <- easybgmFit$inc_BF
  easybgmResult$structure              <- easybgmFit$structure
  easybgmResult$estimates              <- as.matrix(estimates)
  easybgmResult$graph                  <- easybgmResult$estimates * easybgmResult$structure
  easybgmResult$variableType           <- variableSpec[["type"]]
  easybgmResult$baselineCategory       <- variableSpec[["baselineCategory"]]
  easybgmResult$logOdds                <- easybgmFit$log_odds
  easybgmResult$precisionMatrix        <- easybgmFit$precision_matrix
  easybgmResult$partialCorrelations    <- easybgmFit$partial_correlations
  # Only returned by easybgm when at least one variable is of type blume-capel.
  easybgmResult$blumeCapelParameters   <- easybgmFit$blume_capel_parameters
  # R-hat as bgms computes it across chains, put in row-major upper-triangle order.
  easybgmResult$convergence            <- .bayesianNetworkAnalysisCanonicalConvergence(easybgmFit, variables)

  # bgmCompare pads its indicator draws with one main effect indicator per
  # variable, so the sampled structures of a difference fit are wider than the
  # number of edges. Reduce them to the pairwise part before anything counts them.
  reduced <- .bayesianNetworkAnalysisReducePairwiseIndicators(
    sampleGraphs = easybgmFit$sample_graph,
    graphWeights = easybgmFit$graph_weights,
    nVar         = ncol(easybgmResult$estimates)
  )
  easybgmResult$sampleGraphs <- reduced$sampleGraphs
  easybgmResult$graphWeights <- reduced$graphWeights

  samplesPosterior <- .bayesianNetworkAnalysisCanonicalEdgeColumns(easybgmFit$samples_posterior, variables)

  if (isDifferenceFit) {
    # easybgm returns the *baseline* pairwise draws for a group comparison, not
    # the differences. Keeping them under a distinct name stops anything that
    # summarizes the differences from silently reading the baseline instead.
    easybgmResult$baselineSamplesPosterior <- samplesPosterior
    easybgmResult$convergenceIsBaseline    <- TRUE
    # Posterior means of the joint comparison model: one column per group, and
    # one per pair of groups (e.g. "group2 - group1").
    easybgmResult$groupEstimates           <- .bayesianNetworkAnalysisCanonicalEdgeRows(easybgmFit$group_estimates, variables)
    easybgmResult$pairwiseGroupDifferences <- .bayesianNetworkAnalysisCanonicalEdgeRows(easybgmFit$pairwise_group_differences, variables)
  } else {
    easybgmResult$samplesPosterior <- samplesPosterior
  }

  # Store SBM-specific results if Stochastic-Block edge prior was used.
  if (options[["edgePrior"]] == "Stochastic-Block" && .bayesianNetworkAnalysisStochasticBlockAllowed(options)) {
    easybgmResult$sbm <- .bayesianNetworkAnalysisCanonicalSbm(easybgmFit$sbm, variables)
  }

  if (keepRawFit)
    easybgmResult$easybgmFit <- easybgmFit

  easybgmResult
}

# Mixed models return SBM summaries in discrete-then-continuous order. Keep
# every variable-indexed summary in the same order as the network estimates.
.bayesianNetworkAnalysisCanonicalSbm <- function(sbm, variables) {

  matchVariables <- function(variableNames) {
    position <- match(variables, variableNames)
    if (is.null(variableNames) || length(variableNames) != length(variables) ||
        anyNA(position) || anyDuplicated(position))
      stop(gettext("The cluster summaries could not be matched to the variables."))
    position
  }

  coclustering <- sbm$posterior_mean_coclustering_matrix
  rowOrder     <- matchVariables(rownames(coclustering))
  columnOrder  <- matchVariables(colnames(coclustering))

  for (field in c("posterior_mean_allocations", "posterior_mode_allocations")) {
    allocations <- sbm[[field]]
    if (length(allocations) != length(variables))
      stop(gettext("The cluster allocations could not be matched to the variables."))

    # bgms leaves allocations unnamed; their order is the native row order of
    # its co-clustering matrix. Preserve that order until both are mapped.
    allocationNames <- names(allocations) %||% rownames(coclustering)

    sbm[[field]] <- stats::setNames(allocations[matchVariables(allocationNames)], variables)
  }

  sbm$posterior_mean_coclustering_matrix <- coclustering[rowOrder, columnOrder, drop = FALSE]
  sbm
}

# Row-major upper-triangle index (V1-V2, V1-V3, ..., V2-V3, ...), the order in
# which every pairwise quantity is stored after extraction.
.bayesianNetworkAnalysisUpperTriangleIndex <- function(nVar) {

  upperIdx <- which(upper.tri(matrix(0L, nVar, nVar)), arr.ind = TRUE)
  upperIdx[order(upperIdx[, 1L], upperIdx[, 2L]), , drop = FALSE]
}

.bayesianNetworkAnalysisEdgeNames <- function(variables) {

  upperIdx <- .bayesianNetworkAnalysisUpperTriangleIndex(length(variables))
  paste0(variables[upperIdx[, 1L]], "-", variables[upperIdx[, 2L]])
}

# Positions, within edgeNames, of the edges in row-major upper-triangle order.
# bgms orders the pairwise quantities of mixed models by variable type rather
# than by position, and names each after its pair in either orientation (e.g.
# "C-B"), so the order is recovered from the names. NULL when the names do not
# identify every edge exactly once.
.bayesianNetworkAnalysisCanonicalEdgeOrder <- function(edgeNames, variables) {

  nVar <- length(variables)
  if (is.null(edgeNames) || length(edgeNames) != nVar * (nVar - 1L) / 2L)
    return(NULL)

  upperIdx <- .bayesianNetworkAnalysisUpperTriangleIndex(nVar)
  forward  <- paste0(variables[upperIdx[, 1L]], "-", variables[upperIdx[, 2L]])
  reversed <- paste0(variables[upperIdx[, 2L]], "-", variables[upperIdx[, 1L]])

  position <- match(forward, edgeNames)
  position[is.na(position)] <- match(reversed[is.na(position)], edgeNames)

  if (anyNA(position) || anyDuplicated(position))
    return(NULL)

  position
}

.bayesianNetworkAnalysisCanonicalEdgeColumns <- function(samples, variables) {

  if (is.null(samples) || is.null(colnames(samples)))
    return(samples)

  position <- .bayesianNetworkAnalysisCanonicalEdgeOrder(colnames(samples), variables)
  if (is.null(position))
    stop(gettext("The posterior samples could not be matched to the variable pairs."))

  samples <- samples[, position, drop = FALSE]
  colnames(samples) <- .bayesianNetworkAnalysisEdgeNames(variables)
  samples
}

.bayesianNetworkAnalysisCanonicalEdgeRows <- function(x, variables) {

  if (is.null(x))
    return(NULL)

  x <- as.matrix(x)
  position <- .bayesianNetworkAnalysisCanonicalEdgeOrder(rownames(x), variables)
  if (is.null(position))
    stop(gettext("The group estimates could not be matched to the variable pairs."))

  x <- x[position, , drop = FALSE]
  rownames(x) <- .bayesianNetworkAnalysisEdgeNames(variables)
  x
}

.bayesianNetworkAnalysisCanonicalConvergence <- function(easybgmFit, variables) {

  convergence <- easybgmFit$convergence_parameter
  if (is.null(convergence))
    return(NULL)

  # A single network names its R-hat after the pairs; a comparison reports it
  # unnamed, in the edge order of its group estimates.
  edgeNames      <- names(convergence)
  groupEstimates <- easybgmFit$group_estimates
  if (!is.null(groupEstimates) && length(convergence) == nrow(groupEstimates))
    edgeNames <- rownames(groupEstimates)

  # Without identifiable edges the R-hat is dropped rather than attached by
  # position; the edge tables then fall back to a split-chain R-hat.
  position <- .bayesianNetworkAnalysisCanonicalEdgeOrder(edgeNames, variables)
  if (is.null(position))
    return(NULL)

  unname(convergence[position])
}

.bayesianNetworkAnalysisPairwiseIndicatorPositions <- function(nVar) {

  # bgmCompare interleaves one main effect indicator per variable with the
  # pairwise indicators: V1 (main), V1-V2, V1-V3, ..., V2 (main), V2-V3, ...
  # Return the positions of the pairwise entries in that layout.
  positions <- integer(0)
  cursor    <- 0L

  for (i in seq_len(nVar)) {
    cursor <- cursor + 1L                       # main effect of variable i
    nPairs <- nVar - i
    if (nPairs > 0L)
      positions <- c(positions, cursor + seq_len(nPairs))
    cursor <- cursor + nPairs
  }

  positions
}

.bayesianNetworkAnalysisReducePairwiseIndicators <- function(sampleGraphs, graphWeights, nVar) {

  unchanged <- list(sampleGraphs = sampleGraphs, graphWeights = graphWeights)

  if (is.null(sampleGraphs) || length(sampleGraphs) == 0L || is.null(graphWeights))
    return(unchanged)

  nEdges <- (nVar * (nVar - 1L)) %/% 2L

  # Only the group comparison pads the indicators. Anything already at the edge
  # width - every fit of a single network - is left untouched.
  if (!all(nchar(sampleGraphs) == nEdges + nVar))
    return(unchanged)

  keep    <- .bayesianNetworkAnalysisPairwiseIndicatorPositions(nVar)
  reduced <- vapply(strsplit(sampleGraphs, "", fixed = TRUE),
                    function(indicators) paste0(indicators[keep], collapse = ""),
                    character(1L))

  # Dropping the main effect indicators can collapse distinct sampled structures
  # into one, so re-aggregate rather than assuming the strings stay unique.
  weights <- tapply(graphWeights, reduced, sum)

  list(sampleGraphs = names(weights), graphWeights = as.integer(weights))
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
    "nuts"                = "nuts",
    "gibbs"               = "gibbs"
  )

  mapped <- unname(aliases[updateMethod])
  if (length(mapped) == 1L && !is.na(mapped))
    return(mapped)

  .quitAnalysis(gettextf(
    "Unsupported update method '%s'. Please select one of: adaptive-metropolis, nuts, gibbs.",
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

  # Networks are keyed by role and position rather than by label, so a group
  # called "Differences" or "Pooled" cannot overwrite another network. The label
  # shown in the output is stored with each network.
  networks <- list()

  pooledVariableSpec <- .bayesianNetworkAnalysisBuildVariableTypeSpec(options, pooledData)
  useCompare <- .bayesianNetworkAnalysisCompareSupported(options, pooledVariableSpec, nGroups)

  .bayesianNetworkAnalysisAssertInteractionPriorSupported(options, pooledVariableSpec)

  # With a grouping variable, interactionScale is the prior scale on the group
  # differences and interactionScaleBaseline the scale on the partial
  # associations themselves. The group and pooled networks are fits of partial
  # associations, so they take the baseline scale whether or not a difference
  # network is estimated.
  networkOptions <- options
  if (options[["groupingVariable"]] != "")
    networkOptions[["interactionScale"]] <- options[["interactionScaleBaseline"]]

  keepRawFit <- options[["edgePrior"]] == "Stochastic-Block" && .bayesianNetworkAnalysisStochasticBlockAllowed(options)

  if (useCompare) {
    updateMethod <- .bayesianNetworkAnalysisNormalizeUpdateMethod(options[["omrfUpdateMethod"]])
    groupIndicator <- rep(seq_len(nGroups), times = vapply(groupData, nrow, integer(1L)))

    interactionPriorBaseline <- .bayesianNetworkAnalysisBuildInteractionPrior(networkOptions)
    thresholdPrior           <- .bayesianNetworkAnalysisBuildThresholdPrior(options)
    differencePrior          <- .bayesianNetworkAnalysisBuildDifferencePrior(options)
    differenceFamily         <- .bayesianNetworkAnalysisBuildDifferenceFamily(options)

    jaspBase::.setSeedJASP(options)
    compareFit <- try(easybgm::easybgm_compare(
      data    = pooledData,
      package = "bgms",
      group_indicator             = groupIndicator,
      type    = pooledVariableSpec[["type"]],
      baseline_category           = pooledVariableSpec[["baselineCategory"]],
      iter                        = options[["iter"]],
      seed                        = options[["seed"]],
      save                        = TRUE,
      progress                    = FALSE,
      warmup                      = options[["burnin"]],
      chains                      = as.integer(options[["chains"]]),
      update_method               = updateMethod,
      interaction_prior           = interactionPriorBaseline,
      threshold_prior             = thresholdPrior,
      difference_prior            = differencePrior,
      difference_family           = differenceFamily,
      difference_scale            = options[["interactionScale"]],
      progress_callback           = .bayesianNetworkAnalysisMakeProgressCallback(gettext("Estimating group comparison"))
    ))

    if (isTryError(compareFit)) {
      message <- .extractErrorMessage(compareFit)
      .quitAnalysis(gettextf("The group comparison failed with the following error message:\n%s", message))
    }

    differences <- .bayesianNetworkAnalysisExtractEasybgmResult(
      easybgmFit      = compareFit,
      variableSpec    = pooledVariableSpec,
      options         = options,
      keepRawFit      = FALSE,
      isDifferenceFit = TRUE
    )
    differences[c("label", "role")] <- c(gettext("Differences"), "differences")
    networks[["differences"]] <- differences

    pooledFit <- .bayesianNetworkAnalysisFitSingleNetwork(
      data          = pooledData,
      variableSpec  = pooledVariableSpec,
      options       = networkOptions,
      progressLabel = gettext("Estimating pooled network")
    )

    pooled <- .bayesianNetworkAnalysisExtractEasybgmResult(
      easybgmFit   = pooledFit,
      variableSpec = pooledVariableSpec,
      options      = options,
      keepRawFit   = keepRawFit
    )
    pooled[c("label", "role")] <- c(gettext("Pooled"), "pooled")
    networks[["pooled"]] <- pooled
  }

  for (nw in seq_along(groupData)) {
    variableSpec <- .bayesianNetworkAnalysisBuildVariableTypeSpec(options, groupData[[nw]])
    progressLabel <- if (nGroups > 1L) gettextf("Estimating %s", groupNames[[nw]]) else gettext("Estimating network")
    easybgmFit <- .bayesianNetworkAnalysisFitSingleNetwork(groupData[[nw]], variableSpec, networkOptions,
                                                            progressLabel = progressLabel)

    # Group networks are numbered in the order of the group indicator of the comparison.
    groupNetwork <- .bayesianNetworkAnalysisExtractEasybgmResult(
      easybgmFit   = easybgmFit,
      variableSpec = variableSpec,
      options      = options,
      keepRawFit   = keepRawFit
    )
    groupNetwork[c("label", "role")] <- c(groupNames[[nw]], "group")
    networks[[paste0("group", nw)]] <- groupNetwork
  }

  if (!useCompare && options[["groupingVariable"]] != "" && nGroups >= 2L) {
    attr(networks, "compareUnavailableReason") <- gettext(
      "Difference network not estimated: all selected variables must be ordinal or Blume-Capel. Showing separate group estimates instead."
    )
  }

  return(networks)
}

.bayesianNetworkAnalysisNetworkLabels <- function(allNetworks) {

  keys <- names(allNetworks)
  stats::setNames(
    vapply(seq_along(allNetworks), function(i) allNetworks[[i]][["label"]] %||% keys[i], character(1L)),
    keys
  )
}

.bayesianNetworkAnalysisIsDifferences <- function(nw, key) {

  if (!is.null(nw[["role"]]))
    return(identical(nw[["role"]], "differences"))

  # networks stored by an earlier version carry no role and are keyed by label
  identical(key, gettext("Differences"))
}

.bayesianNetworkAnalysisDifferencesKey <- function(allNetworks) {

  keys <- names(allNetworks)
  isDifferences <- vapply(seq_along(allNetworks), function(i)
    .bayesianNetworkAnalysisIsDifferences(allNetworks[[i]], keys[i]), logical(1L))

  keys[isDifferences][1L]
}

.bayesianNetworkAnalysisHasDifferences <- function(allNetworks) {

  !is.null(allNetworks) && !is.na(.bayesianNetworkAnalysisDifferencesKey(allNetworks))
}

.bayesianNetworkAnalysisMainTable <- function(mainContainer, dataset, options, network) {

  if (is.null(network[["network"]]) || mainContainer$getError())
    return()

  tb <- mainContainer[["generalTable"]]
  nGraphs <- length(network[["network"]])

  # Check if group comparison is active and 'Differences' network is present
  groupComparison <- options[["groupingVariable"]] != "" && .bayesianNetworkAnalysisHasDifferences(network[["network"]])

  if (options[["minEdgeStrength"]] != 0) {
    ignored <- logical(nGraphs)
    for (i in seq_along(network[["network"]])) {
      graph      <- network[["network"]][[i]][["graph"]]
      ignored[i] <- !anyNA(graph) && all(abs(graph) <= options[["minEdgeStrength"]])
    }
    if (any(ignored)) {
      if (nGraphs == 1L) {
        text <- gettext("Minimum edge strength ignored in the network plot because it was larger than the absolute value of the strongest edge.")
      } else {
        text <- gettextf("Minimum edge strength ignored in the network plot of group%1$s %2$s because it was larger than the absolute value of the strongest edge.",
                         ifelse(sum(ignored) == 2L, "s", ""),
                         paste0(.bayesianNetworkAnalysisNetworkLabels(network[["network"]])[ignored], collapse = ", ")
        )
      }
      tb$addFootnote(text, symbol = gettext("<em>Warning: </em>"))
    }
  }

  if (groupComparison) {
    # Only show Differences row, rename columns, drop sparsity
    nw <- network[["network"]][[.bayesianNetworkAnalysisDifferencesKey(network[["network"]])]]
    nVar <- ncol(nw[["graph"]])
    nEdges <- (nVar * (nVar - 1L)) %/% 2
    evidence <- .bayesianNetworkAnalysisEvidenceCategory(nw[["BF"]][upper.tri(nw[["BF"]], diag = FALSE)],
                                                         options[["edgeSpecificOverviewInclusionCriteria"]])
    nDifferent    <- sum(evidence == "included")
    nEqual        <- sum(evidence == "excluded")
    nInconclusive <- sum(evidence == "inconclusive")
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
      df[["info"]] <- unname(.bayesianNetworkAnalysisNetworkLabels(network[["network"]]))

    threshold <- options[["edgeSpecificOverviewInclusionCriteria"]]
    nVar <- ncol(network[["network"]][[1L]][["graph"]])
    for (i in seq_len(nGraphs)) {
      nw <- network[["network"]][[i]]
      nEdges <- (nVar * (nVar - 1L)) %/% 2
      evidence      <- .bayesianNetworkAnalysisEvidenceCategory(nw[["BF"]][upper.tri(nw[["BF"]], diag = FALSE)], threshold)
      nIncluded     <- sum(evidence == "included")
      nExcluded     <- sum(evidence == "excluded")
      nInconclusive <- sum(evidence == "inconclusive")
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
  hasDifferences <- .bayesianNetworkAnalysisHasDifferences(allNetworks)
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
    posteriorStructurePlotContainer[[v]] <- createJaspPlot(title = allNetworks[[v]][["label"]] %||% v)

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

                           dependencies = c("centralityTable", "maxEdgeStrength", "minEdgeStrength", "credibilityInterval"))
  table$addColumnInfo(name = "Variable", title = gettext("Variable"), type = "string")

  if (options[["credibilityInterval"]])
    table$addFootnote(gettext("Centrality measures are standardized to z-scores across nodes and averaged over the networks sampled from the posterior."))
  else
    table$addFootnote(gettext("Centrality measures are standardized to z-scores across nodes and computed on the posterior mean network."))

  # shared titles
  overTitles <- if (is.null(network[["network"]])) gettext("Network") else unname(.bayesianNetworkAnalysisNetworkLabels(network[["network"]]))

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

  width <- 200 + 120 * sum(measuresToShow)
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
  hasDifferences  <- .bayesianNetworkAnalysisHasDifferences(allNetworks)
  if (hasDifferences)
    allNetworks <- allNetworks[.bayesianNetworkAnalysisDifferencesKey(allNetworks)]
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
    parameterHdiContainer[[v]] <- createJaspPlot(title = if (nGraphs == 1L) "" else (allNetworks[[v]][["label"]] %||% v), width = width, height = height)

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
    noteHtml$text <- paste0("<p>", gettext("The difference parameter HDI plot shows the posterior mean and highest density interval (HDI) for each pairwise difference in partial association between groups. Differences are the second group minus the first: positive values indicate a stronger association in the second group; negative values indicate a stronger association in the first group."), "</p>")
    noteHtml$position <- 99
    parameterHdiContainer[["differenceHdiNote"]] <- noteHtml
  }
}

.bayesianNetworkAnalysisComputeParameterHdi <- function(network, options, coverage) {

  samplesPosterior <- network[["samplesPosterior"]]
  if (is.null(samplesPosterior))
    stop(gettext("Posterior samples are required for the parameter HDI. Please ensure the model was fitted with 'save = TRUE'."))

  # Construct readable edge labels from decoded variable names.
  # Sort indices in row-major order to match the column order of samplesPosterior (as produced by bgms).
  variables   <- colnames(network[["estimates"]])
  decodedVars <- decodeColNames(variables)
  nVar        <- length(variables)
  upperIdx    <- which(upper.tri(matrix(0L, nVar, nVar)), arr.ind = TRUE)
  upperIdx    <- upperIdx[order(upperIdx[, 1L], upperIdx[, 2L]), , drop = FALSE]

  # The extraction puts the draws in row-major upper-triangle order and names them
  # accordingly. Verify that rather than trust it: a wrong order would attach
  # every interval to the wrong edge.
  sampleNames <- colnames(samplesPosterior)
  if (!is.null(sampleNames)) {
    expectedNames <- paste0(variables[upperIdx[, 1L]], "-", variables[upperIdx[, 2L]])
    if (!identical(sampleNames, expectedNames))
      stop(gettext("The posterior samples are not in the expected edge order. Please report this, as the parameter HDI output cannot be labelled reliably."))
  }

  # Column-then-row, matching the Relation labels of the edge specific overview.
  edgeLabels  <- paste0(decodedVars[upperIdx[, 2L]], "-", decodedVars[upperIdx[, 1L]])

  if (options[["labelAbbreviation"]])
    edgeLabels <- base::abbreviate(edgeLabels, minlength = options[["labelAbbreviationLength"]])

  # Compute HDI and posterior means for each partial association
  hdiIntervals   <- apply(samplesPosterior, MARGIN = 2L, FUN = HDInterval::hdi, credMass = coverage)
  posteriorMeans <- colMeans(samplesPosterior)

  # A spike-and-slab posterior can have an HDI of [0, 0] and a nonzero mean, and a
  # posterior mean need not lie inside the HDI, so the mean is reported as is. The
  # estimate of the median probability model, shown in the edge specific overview,
  # is reported alongside it.
  medianProbabilityEstimates <- network[["graph"]]

  posterior <- data.frame(
    mean  = posteriorMeans,
    lower = hdiIntervals["lower", ],
    upper = hdiIntervals["upper", ],
    medianProbabilityEstimate = if (is.null(medianProbabilityEstimates)) NA_real_ else medianProbabilityEstimates[upperIdx],
    edge  = unname(edgeLabels),
    stringsAsFactors = FALSE
  )

  # Order by posterior mean
  posterior[order(posterior$mean), ]
}

.bayesianNetworkAnalysisMakeParameterHdiPlot <- function(network, options, coverage) {

  posterior      <- .bayesianNetworkAnalysisComputeParameterHdi(network, options, coverage)
  posterior$edge <- factor(posterior$edge, levels = posterior$edge)

  coveragePct <- round(coverage * 100)
  yLabel      <- gettextf("%d%% HDI of Partial Association", coveragePct)

  g <- ggplot2::ggplot(posterior, ggplot2::aes(x = edge, y = mean, ymin = lower, ymax = upper)) +
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
    complexityPlotContainer[[v]] <- createJaspPlot(title = allNetworks[[v]][["label"]] %||% v)

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

    labels <- colnames(allNetworks[[1L]]$graph)

  } else {

    nodeNames <- colnames(allNetworks[[1L]]$graph)
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
    structurePlotContainer[[v]] <- createJaspPlot(title = allNetworks[[v]][["label"]] %||% v, width = width[v], height = height[v])

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
  hasDifferences <- .bayesianNetworkAnalysisHasDifferences(allNetworks)
  if (hasDifferences)
    allNetworks <- allNetworks[.bayesianNetworkAnalysisDifferencesKey(allNetworks)]
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
                                                                                             "legendToPlotRatio", "edgeLabels", "edgeLabelSize", "edgeLabelPosition"
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

    labels <- colnames(allNetworks[[1L]]$graph)

  } else {

    nodeNames <- colnames(allNetworks[[1L]]$graph)
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
    evidencePlotContainer[[v]] <- createJaspPlot(title = if (nGraphs == 1L) "" else (allNetworks[[v]][["label"]] %||% v), width = width[v], height = height[v])

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
  evidence   <- .bayesianNetworkAnalysisEvidenceCategory(network[["BF"]], options[["edgeInclusionCriteria"]])
  graphColor <- matrix(NA, ncol = nrow(network[["graph"]]), nrow = nrow(network[["graph"]]))
  if (options$edgeInclusion) graphColor[evidence == "included"]     <- "#36648b"
  if (options$edgeExclusion) graphColor[evidence == "excluded"]     <- "#eeb004"
  if (options$edgeAbsence)   graphColor[evidence == "inconclusive"] <- "#bfbfbf"
  diag(graphColor) <- NA


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

# One classification shared by the summary table, the edge specific overview and
# the edge evidence plot, so they agree at the boundaries. The categories are
# exclusive by construction: a Bayes factor at or above the threshold is evidence
# for inclusion, one at or below its reciprocal is evidence for exclusion, and
# anything in between is inconclusive. With a threshold of 1, BF = 1 counts as
# evidence for inclusion. A missing Bayes factor is inconclusive.
.bayesianNetworkAnalysisEvidenceCategory <- function(bf, threshold) {

  category <- rep("inconclusive", length(bf))
  category[which(bf <= 1 / threshold)] <- "excluded"
  category[which(bf >= threshold)]     <- "included"

  if (!is.null(dim(bf)))
    dim(category) <- dim(bf)

  category
}

.bayesianNetworkAnalysisEdgeEvidenceTable <- function(mainContainer, network, options) {

  if (!is.null(mainContainer[["edgeEvidenceTable"]]) || !options[["edgeEvidenceTable"]])
    return()

  variables <- unlist(options[["variables"]])
  nVar <- length(variables)
  nGraphs <- max(1L, length(network[["network"]]))

  table <- createJaspTable(gettext("Edge evidence probability table"), dependencies = c("edgeEvidenceTable", "evidenceType")) # , position = 4
  table$addColumnInfo(name = "Variable", title = gettext("Variable"), type = "string")

  overTitles <- if (is.null(network[["network"]])) gettext("Network") else unname(.bayesianNetworkAnalysisNetworkLabels(network[["network"]]))

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
  hasDifferencesNetwork <- .bayesianNetworkAnalysisHasDifferences(allNetworks)

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
      table         <- createJaspTable(allNetworks[[nwName]][["label"]] %||% nwName)
      isDifferences <- .bayesianNetworkAnalysisIsDifferences(allNetworks[[nwName]], nwName)
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

  differencesKey <- .bayesianNetworkAnalysisDifferencesKey(allNetworks)
  nwDiff         <- allNetworks[[differencesKey]]

  variables   <- colnames(nwDiff$estimates)
  nVar        <- length(variables)
  nEdges      <- nVar * (nVar - 1L) / 2L
  decodedVars <- decodeColNames(variables)

  upperTriIdx <- .bayesianNetworkAnalysisUpperTriangleIndex(nVar)

  relation      <- paste0(decodedVars[upperTriIdx[, 2L]], "-", decodedVars[upperTriIdx[, 1L]])
  inclusionProb <- nwDiff$inclusionProbabilities[upperTriIdx]
  inclusionBF   <- nwDiff$BF[upperTriIdx]

  categoryLabels <- c(included = gettext("difference"), excluded = gettext("equal"), inconclusive = gettext("inconclusive"))
  category       <- unname(categoryLabels[.bayesianNetworkAnalysisEvidenceCategory(inclusionBF, threshold)])

  networkLabels <- .bayesianNetworkAnalysisNetworkLabels(allNetworks)
  roles         <- vapply(allNetworks, function(nw) if (is.null(nw[["role"]])) "" else nw[["role"]], character(1L))
  groupLabels   <- unname(networkLabels[roles == "group"])

  # Median probability model: no difference where the posterior difference probability is at most 0.5.
  inModel <- inclusionProb > 0.5

  table$addColumnInfo(name = "relation", title = gettext("Relation"), type = "string")
  df <- data.frame(relation = relation, stringsAsFactors = FALSE)

  groupDifferences <- nwDiff$pairwiseGroupDifferences
  if (is.null(groupDifferences)) {
    # networks stored by an earlier version only carry a difference matrix
    table$addColumnInfo(name = "difference1", title = gettext("Difference Estimate"), type = "number")
    df$difference1 <- ifelse(inModel, nwDiff$estimates[upperTriIdx], 0)
  } else {
    # With more than two groups there is no single difference per edge, so every
    # pair of groups gets its own column. easybgm numbers the groups in the order
    # of the group networks and labels each pair "groupJ - groupI".
    groupPairs <- utils::combn(length(groupLabels), 2L)
    if (!identical(colnames(groupDifferences), paste0("group", groupPairs[2L, ], " - group", groupPairs[1L, ]))) {
      table$setError(gettext("The pairwise group differences could not be matched to the groups."))
      return()
    }

    for (k in seq_len(ncol(groupPairs))) {
      columnName <- paste0("difference", k)
      table$addColumnInfo(name      = columnName,
                          title     = gettextf("%1$s \u2212 %2$s", groupLabels[groupPairs[2L, k]], groupLabels[groupPairs[1L, k]]),
                          type      = "number",
                          overtitle = gettext("Difference"))
      df[[columnName]] <- ifelse(inModel, groupDifferences[, k], 0)
    }
  }

  table$addColumnInfo(name = "inclusionProb", title = gettext("Posterior Diff. Prob."), type = "number")
  table$addColumnInfo(name = "inclusionBF",   title = gettext("Difference BF"),         type = "number")
  table$addColumnInfo(name = "category",      title = gettext("Category"),              type = "string")
  df$inclusionProb <- inclusionProb
  df$inclusionBF   <- inclusionBF
  df$category      <- category

  # The group comparison reports R-hat for the baseline pairwise interactions
  # rather than for the differences, so say so instead of labelling it as the
  # convergence of the difference estimates next to it.
  convergence    <- .bayesianNetworkAnalysisComputeEdgeConvergence(nwDiff, upperTriIdx, nEdges, as.integer(options[["chains"]]))
  isBaselineRhat <- isTRUE(nwDiff[["convergenceIsBaseline"]])
  if (!is.null(convergence)) {
    table$addColumnInfo(name  = "convergence",
                        title = if (isBaselineRhat) gettext("Convergence (baseline)") else gettext("Convergence"),
                        type  = "number")
    df$convergence <- convergence
  }

  # Group estimates come from the joint model, so they agree with the differences.
  groupEstimates <- nwDiff$groupEstimates
  if (!is.null(groupEstimates)) {
    for (g in seq_len(ncol(groupEstimates))) {
      columnName <- paste0("groupEstimate", g)
      table$addColumnInfo(name = columnName, title = gettext("Estimate"), type = "number", overtitle = groupLabels[g])
      df[[columnName]] <- groupEstimates[, g]
    }
  }

  # Networks fitted separately: the pooled data, or every group for a network
  # stored by an earlier version.
  separateKeys <- if (is.null(groupEstimates)) setdiff(names(allNetworks), differencesKey) else names(allNetworks)[roles == "pooled"]
  for (k in seq_along(separateKeys)) {
    columnName <- paste0("separateEstimate", k)
    table$addColumnInfo(name = columnName, title = gettext("Estimate"), type = "number", overtitle = networkLabels[[separateKeys[k]]])
    df[[columnName]] <- allNetworks[[separateKeys[k]]]$graph[upperTriIdx]
  }

  table$addFootnote(gettext("Difference estimates are based on the median probability model: edges with a posterior difference probability \u2264 0.5 are set to zero."))
  if (!is.null(groupEstimates))
    table$addFootnote(gettext("Group estimates are posterior means of the joint model used for the group comparison; a difference that is not set to zero equals the difference between the corresponding group estimates."))
  if (any(roles == "pooled"))
    table$addFootnote(gettext("Pooled estimates come from a separate fit to the pooled data and are based on its median probability model."))
  table$addFootnote(gettext("Bayes factors with values of infinity indicate that the estimated posterior difference probability is either 1 or 0. Please see the help file for more information."))
  if (!is.null(convergence)) {
    if (isBaselineRhat)
      table$addFootnote(gettext("Convergence is the R-hat (Gelman-Rubin) statistic of the baseline pairwise interactions of the joint model; the group comparison does not report a separate R-hat for the differences themselves. Values greater than about 1.01-1.05 are considered concerning. Consider increasing the number of iterations and/or chains to improve convergence."))
    else if (as.integer(options[["chains"]]) >= 2L)
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

  for (k in seq_len(nEdges)) {
    i <- upperTriIdx[k, 1]
    j <- upperTriIdx[k, 2]

    relation[k]      <- paste0(decodedVars[j], "-", decodedVars[i])
    estimate[k]      <- nw$graph[i, j]
    inclusionProb[k] <- nw$inclusionProbabilities[i, j]
    inclusionBF[k]   <- nw$BF[i, j]
  }

  categoryLabels <- if (isDifferences)
    c(included = gettext("difference"), excluded = gettext("equal"),    inconclusive = gettext("inconclusive"))
  else
    c(included = gettext("included"),   excluded = gettext("excluded"), inconclusive = gettext("inconclusive"))
  category <- unname(categoryLabels[.bayesianNetworkAnalysisEvidenceCategory(inclusionBF, threshold)])

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

.bayesianNetworkAnalysisBlumeCapelTable <- function(mainContainer, network, options) {

  if (!is.null(mainContainer[["blumeCapelTable"]]) || !options[["edgeSpecificOverviewTable"]] ||
      length(options[["variablesBlumeCapel"]]) == 0L)
    return()

  # The Blume-Capel assignments travel in the variables option, a dependency of mainContainer.
  dependencies <- "edgeSpecificOverviewTable"
  allNetworks  <- network[["network"]]

  # easybgm only reports these for fits of individual networks; the differences
  # network of a group comparison does not carry them.
  bcNetworks <- Filter(.bayesianNetworkAnalysisHasBlumeCapelParameters, allNetworks)

  if (!is.null(allNetworks) && length(bcNetworks) == 0L)
    return()

  if (length(bcNetworks) > 1L) {
    container <- createJaspContainer(gettext("Blume-Capel Main Effects"), dependencies = dependencies)
    container$position <- 3
    mainContainer[["blumeCapelTable"]] <- container

    for (nwName in names(bcNetworks)) {
      table <- createJaspTable(bcNetworks[[nwName]][["label"]] %||% nwName)
      .bayesianNetworkAnalysisFillBlumeCapelTable(table, bcNetworks[[nwName]], options)
      container[[nwName]] <- table
    }

    return()
  }

  table <- createJaspTable(gettext("Blume-Capel Main Effects"), dependencies = dependencies)
  table$position <- 3
  mainContainer[["blumeCapelTable"]] <- table

  if (is.null(allNetworks) || mainContainer$getError()) {
    .bayesianNetworkAnalysisFillBlumeCapelTable(table, NULL, options)
    return()
  }

  .bayesianNetworkAnalysisFillBlumeCapelTable(table, bcNetworks[[1L]], options)
}

.bayesianNetworkAnalysisHasBlumeCapelParameters <- function(nw) {

  bc <- nw[["blumeCapelParameters"]]

  !is.null(bc) && is.data.frame(bc) && nrow(bc) > 0L
}

.bayesianNetworkAnalysisFillBlumeCapelTable <- function(table, nw, options) {

  ciTitle <- gettextf("%s%% Credible Interval", 95)

  table$addColumnInfo(name = "variable",    title = gettext("Variable"),          type = "string")
  table$addColumnInfo(name = "effect",      title = gettext("Effect"),            type = "string")
  table$addColumnInfo(name = "baseline",    title = gettext("Baseline Category"), type = "integer")
  table$addColumnInfo(name = "estimate",    title = gettext("Estimate"),          type = "number")
  table$addColumnInfo(name = "sd",          title = gettext("Posterior SD"),      type = "number")
  table$addColumnInfo(name = "lower",       title = gettext("Lower"),             type = "number", overtitle = ciTitle)
  table$addColumnInfo(name = "upper",       title = gettext("Upper"),             type = "number", overtitle = ciTitle)
  table$addColumnInfo(name = "convergence", title = gettext("Convergence"),       type = "number")

  table$addFootnote(gettext("The linear (\u03b1) and quadratic (\u03b2) contributions of the Blume-Capel model for the category thresholds, reported relative to the baseline category the variable was fitted with."))

  if (is.null(nw) || !.bayesianNetworkAnalysisHasBlumeCapelParameters(nw))
    return()

  bc <- nw[["blumeCapelParameters"]]

  # Guard against easybgm renaming a column: a missing one becomes all-missing
  # rather than dropping the whole table.
  bcColumn <- function(columnName, default = NA_real_) {
    values <- bc[[columnName]]
    if (is.null(values)) rep(default, nrow(bc)) else values
  }

  effectLabels <- c(linear = gettext("Linear"), quadratic = gettext("Quadratic"))
  effect       <- as.character(bcColumn("Effect", NA_character_))
  labelled     <- unname(effectLabels[effect])
  effect       <- ifelse(is.na(labelled), effect, labelled)

  df <- data.frame(
    variable    = decodeColNames(as.character(bcColumn("Variable", NA_character_))),
    effect      = effect,
    baseline    = as.integer(bcColumn("Baseline Category")),
    estimate    = bcColumn("Estimate"),
    sd          = bcColumn("Posterior SD"),
    lower       = bcColumn("Lower 2.5%"),
    upper       = bcColumn("Upper 97.5%"),
    convergence = bcColumn("Convergence"),
    stringsAsFactors = FALSE
  )

  if (as.integer(options[["chains"]]) >= 2L)
    table$addFootnote(gettext("Convergence is the R-hat (Gelman-Rubin) statistic, values greater than about 1.01-1.05 are considered concerning, indicating potential lack of convergence for the estimates of the main effects. Consider increasing the number of iterations and/or chains to improve convergence."))
  else
    table$addFootnote(gettext("Convergence is the split-chain R-hat (Gelman-Rubin) statistic, computed post hoc by splitting the posterior samples into two halves."))

  table$setData(df)
}

.bayesianNetworkAnalysisParameterHdiTable <- function(mainContainer, network, options) {

  if (!is.null(mainContainer[["parameterHdiTable"]]) || !options[["parameterHdiTable"]] ||
      options[["groupingVariable"]] != "")
    return()

  dependencies <- c("parameterHdiTable", "parameterHdiTableCoverage",
                    "labelAbbreviation", "labelAbbreviationLength")

  coverage    <- options[["parameterHdiTableCoverage"]]
  allNetworks <- network[["network"]]
  nGraphs     <- max(1L, length(allNetworks))

  if (nGraphs > 1L) {
    container <- createJaspContainer(gettext("Parameter HDI Table"), dependencies = dependencies)
    container$position <- 5
    mainContainer[["parameterHdiTable"]] <- container

    if (is.null(allNetworks) || mainContainer$getError())
      return()

    for (nwName in names(allNetworks)) {
      table <- createJaspTable(allNetworks[[nwName]][["label"]] %||% nwName)
      .bayesianNetworkAnalysisFillParameterHdiTable(table, allNetworks[[nwName]], options, coverage)
      container[[nwName]] <- table
    }

    return()
  }

  table <- createJaspTable(gettext("Parameter HDI Table"), dependencies = dependencies)
  table$position <- 5
  mainContainer[["parameterHdiTable"]] <- table

  if (is.null(allNetworks) || mainContainer$getError()) {
    .bayesianNetworkAnalysisFillParameterHdiTable(table, NULL, options, coverage)
    return()
  }

  .bayesianNetworkAnalysisFillParameterHdiTable(table, allNetworks[[1L]], options, coverage)
}

.bayesianNetworkAnalysisFillParameterHdiTable <- function(table, nw, options, coverage) {

  coveragePct <- round(coverage * 100)
  hdiTitle    <- gettextf("%d%% HDI", coveragePct)

  table$addColumnInfo(name = "relation", title = gettext("Relation"),       type = "string")
  table$addColumnInfo(name = "mean",     title = gettext("Posterior Mean"), type = "number")
  table$addColumnInfo(name = "lower",    title = gettext("Lower"),          type = "number", overtitle = hdiTitle)
  table$addColumnInfo(name = "upper",    title = gettext("Upper"),          type = "number", overtitle = hdiTitle)
  table$addColumnInfo(name = "medianProbabilityEstimate", title = gettext("Median Probability Model Estimate"), type = "number")

  if (is.null(nw))
    return()

  posterior <- try(.bayesianNetworkAnalysisComputeParameterHdi(nw, options, coverage))
  if (isTryError(posterior)) {
    table$setError(.extractErrorMessage(posterior))
    return()
  }

  # .bayesianNetworkAnalysisComputeParameterHdi orders ascending for the plot, whose flipped
  # axis puts the smallest at the bottom. Reverse it so the table reads top-to-bottom like the plot.
  posterior <- posterior[order(posterior$mean, decreasing = TRUE), ]

  df <- data.frame(
    relation = posterior$edge,
    mean     = posterior$mean,
    lower    = posterior$lower,
    upper    = posterior$upper,
    medianProbabilityEstimate = posterior$medianProbabilityEstimate,
    stringsAsFactors = FALSE
  )

  table$addFootnote(gettext("The posterior mean averages over all posterior samples, including those in which the edge is excluded, so it can be nonzero when the HDI collapses onto zero. The median probability model estimate sets edges with a posterior inclusion probability \u2264 0.5 to zero, matching the edge specific overview table."))

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
  container$position <- 4
  mainContainer[["interpretativeScaleContainer"]] <- container

  allNetworks <- network[["network"]]
  if (is.null(allNetworks) || mainContainer$getError())
    return()

  nGraphs <- length(allNetworks)

  if (nGraphs > 1L) {
    for (nwName in names(allNetworks)) {
      nwContainer <- createJaspContainer(allNetworks[[nwName]][["label"]] %||% nwName)
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
    matrixName  = gettext("Partial Correlations"),
    matrixKey   = "partialCorrelations",
    position    = 2
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

  # The extraction stores every pairwise quantity in row-major upper-triangle
  # order (V1-V2, V1-V3, ..., V2-V3, ...). Reading it as column-major attaches
  # each statistic to the wrong edge from four variables onwards.
  nVar     <- nrow(nw$estimates)
  rowIdx   <- which(upper.tri(matrix(NA, nVar, nVar)), arr.ind = TRUE)
  rowIdx   <- rowIdx[order(rowIdx[, 1L], rowIdx[, 2L]), , drop = FALSE]
  rowMajor <- matrix(NA_integer_, nVar, nVar)
  rowMajor[rowIdx] <- seq_len(nrow(rowIdx))

  # Prefer the R-hat bgms itself computed across chains over the post hoc
  # split-chain statistic below.
  reported    <- nw$convergence
  useReported <- is.numeric(reported) && length(reported) >= nrow(rowIdx)

  posteriorSamples <- nw$samplesPosterior
  if (!useReported &&
      (is.null(posteriorSamples) || ncol(posteriorSamples) < nEdges || nrow(posteriorSamples) < 4L))
    return(NULL)

  convergence <- numeric(nEdges)
  for (k in seq_len(nEdges)) {
    col <- rowMajor[upperTriIdx[k, 1], upperTriIdx[k, 2]]

    convergence[k] <- if (is.na(col))
      NA_real_
    else if (useReported)
      reported[[col]]
    else if (col <= ncol(posteriorSamples))
      .bayesianNetworkAnalysisRhat(posteriorSamples[, col], nChains)
    else
      NA_real_
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
    coclusteringPlotContainer[[v]] <- createJaspPlot(title = allNetworks[[v]][["label"]] %||% v, width = 480, height = 400)

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
  labels   <- colnames(network$estimates)

  centralityOutput <- .bayesianNetworkAnalysisCentralityGrid(network$estimates, measures, labels)
  colnames(centralityOutput) <- c("node", "measure", "posteriorMeans")

  if (options[["credibilityInterval"]]) {

    nVar <- as.numeric(nrow(network$estimates))

    # Compute centrality for each posterior sample. The samples are stored in
    # row-major upper-triangle order, which is what bycolumn = FALSE reconstructs.
    draws <- vapply(seq_len(nrow(network$samplesPosterior)), function(i) {
      graph <- vectorToMatrix(network$samplesPosterior[i, ], nVar, bycolumn = FALSE)
      .bayesianNetworkAnalysisCentralityGrid(graph, measures, labels)[["value"]]
    }, numeric(nrow(centralityOutput)))

    centralityOutput <- cbind(centralityOutput, matrix(draws, nrow = nrow(centralityOutput)))
  }

  return(centralityOutput)
}

# qgraph drops a measure it cannot compute, e.g. Strength of an empty network
# (see https://github.com/jasp-stats/jasp-test-release/issues/2298). Return every
# node-measure combination so the point estimate and each posterior sample have
# the same rows; a measure that could not be standardized is reported as zero.
.bayesianNetworkAnalysisCentralityGrid <- function(graph, measures, labels) {

  centralityData <- qgraph::centralityPlot(unname(as.matrix(graph)),
                                           include = measures,
                                           verbose = FALSE,
                                           print   = FALSE,
                                           scale   = "z-scores",
                                           labels  = labels)$data

  grid <- expand.grid(node = labels, measure = measures, stringsAsFactors = FALSE)
  grid$value <- centralityData$value[match(paste(grid$node, grid$measure, sep = "\r"),
                                           paste(centralityData$node, centralityData$measure, sep = "\r"))]
  grid$value[!is.finite(grid$value)] <- 0

  grid
}
