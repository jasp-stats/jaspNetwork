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

BayesianNetworkAnalysis <- function(jaspResults, dataset, options) {

  # MissingValues needed for the .networkAnalysisReadData function in the frequentist network module:
  options[["missingValues"]] <- "listwise" # Unfortunately BDgraph does not work with pairwise missing values

  dataset <- .networkAnalysisReadData(dataset, options) # from networkanalysis.R

  mainContainer <- .bayesianNetworkAnalysisSetupMainContainerAndTable(jaspResults, dataset, options)
  .bayesianNetworkAnalysisErrorCheck(mainContainer, dataset, options)

  network <- .bayesianNetworkAnalysisRun(mainContainer, dataset, options)

  .bayesianNetworkAnalysisMainTable        (mainContainer, dataset, options, network)
  .bayesianNetworkAnalysisWeightMatrixTable(mainContainer, network, options)
  .bayesianNetworkAnalysisEdgeEvidenceTable(mainContainer, network, options)
  .bayesianNetworkAnalysisPlotContainer    (mainContainer, network, options)
  .bayesianNetworkAnalysisCentralityTable  (mainContainer, network, options)

  return()
}

.bayesianNetworkAnalysisSetupMainContainerAndTable <- function(jaspResults, dataset, options) {

  mainContainer <- jaspResults[["mainContainer"]]
  if (is.null(mainContainer)) {
    mainContainer <- createJaspContainer(dependencies = c("variables", "groupingVariable", "model",
                                                          "burnin", "iter", "gPrior", "dfPrior", "initialConfiguration",
                                                          "edgePrior", "interactionScale", "betaAlpha", "betaBeta",
                                                          "dirichletAlpha", "thresholdAlpha", "thresholdBeta"))
    jaspResults[["mainContainer"]] <- mainContainer
  }
  .bayesianNetworkAnalysisMainTableMeta(mainContainer, dataset, options)

  return(mainContainer)
}

.bayesianNetworkAnalysisMainTableMeta <- function(mainContainer, dataset, options) {

  if (is.null(mainContainer[["generalTable"]])) {

    tb <- createJaspTable(gettext("Summary of Network"), position = 1, dependencies = "minEdgeStrength")

    if (length(dataset) > 1L) tb$addColumnInfo(name = "info", title = gettext("Network"), type = "string")

    tb$addColumnInfo(name = "nodes",    title = gettext("Number of nodes"),          type = "integer")
    tb$addColumnInfo(name = "nonZero",  title = gettext("Number of non-zero edges"), type = "string")
    tb$addColumnInfo(name = "Sparsity", title = gettext("Sparsity"),                 type = "number")


    if (options[["model"]] == "omrf") {

      # add footnote
      tb$addFootnote(gettext("The Ordinal Markov Random Field may require a substantial amount of time to complete. This time increases with the number of variables and the number of iterations."))
    }

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

    if (is.null(networkList[["centrality"]]) && (options[["centralityTable"]] || options[["centralityPlot"]]))
      networkList[["centrality"]] <- .bayesianNetworkAnalysisComputeCentrality(networkList[["network"]], options)

    names(networkList[["network"]]) <- names(dataset) # <- names(networkList[["centrality"]])

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
      nSamples <- nrow(network$samplesPosterior[[1]])

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

.bayesianNetworkAnalysisComputeNetworks <- function(options, dataset) {

  networks <- vector("list", length(dataset))

  for (nw in seq_along(dataset)) {
    # When method = "gcgm" a vector with binary values is needed:
    if (options[["model"]] == "ggm") {
      # Estimate network
      jaspBase::.setSeedJASP(options)
      easybgmFit <- try(easybgm::easybgm(data       = apply(dataset[[nw]], 2, as.numeric),
                                         type       = "continuous",
                                         package    = "BDgraph" ,
                                         iter       = options[["iter"]],
                                         save       = TRUE,
                                         centrality = FALSE,
                                         burnin     = options[["burnin"]],
                                         g.start    = options[["initialConfiguration"]],
                                         df.prior   = options[["dfPrior"]],
                                         g.prior    = options[["gPrior"]]))

      if (isTryError(easybgmFit)) {
        message <- .extractErrorMessage(easybgmFit)
        .quitAnalysis(gettextf("The analysis failed with the following error message:\n%s", message))
      }



      # Extract results
      easybgmResult <- list()

      easybgmResult$graphWeights           <- easybgmFit$graph_weights
      easybgmResult$inclusionProbabilities <- easybgmFit$inc_probs
      easybgmResult$BF                     <- easybgmFit$inc_BF
      easybgmResult$structure              <- easybgmFit$structure
      easybgmResult$estimates              <- as.matrix(easybgmFit$parameters)
      easybgmResult$graph                  <- easybgmResult$estimates*easybgmResult$structure
      easybgmResult$sampleGraphs           <- easybgmFit$sample_graph
      easybgmResult$samplesPosterior       <- list(easybgmFit$samples_posterior)

      networks[[nw]] <- easybgmResult

    }

    # if model is gcgm
    if (options[["model"]] == "gcgm") {
      nonContVariables <- c()
      for (var in options[["variables"]]) {

        # A 1 indicates noncontinuous variables:
         if (is.factor(dataset[[nw]][[var]])) {
          nonContVariables <- c(nonContVariables, 1)
        } else {
          nonContVariables <- c(nonContVariables, 0)
        }
      }
      # Estimate network
      jaspBase::.setSeedJASP(options)
      easybgmFit <- try(easybgm::easybgm(data       = apply(dataset[[nw]], 2, as.numeric),
                                         type       = "mixed",
                                         package    = "BDgraph",
                                         not_cont   = nonContVariables,
                                         iter       = options[["iter"]],
                                         save       = TRUE,
                                         centrality = FALSE,
                                         burnin     = options[["burnin"]],
                                         g.start    = options[["initialConfiguration"]],
                                         df.prior   = options[["dfPrior"]],
                                         g.prior    = options[["gPrior"]]))


      if (isTryError(easybgmFit)) {
        message <- .extractErrorMessage(easybgmFit)
        .quitAnalysis(gettextf("The analysis failed with the following error message:\n%s", message))
      }


      # Extract results
      easybgmResult <- list()

      easybgmResult$graphWeights           <- easybgmFit$graph_weights
      easybgmResult$inclusionProbabilities <- easybgmFit$inc_probs
      easybgmResult$BF                     <- easybgmFit$inc_BF
      easybgmResult$structure              <- easybgmFit$structure
      easybgmResult$estimates              <- as.matrix(easybgmFit$parameters)
      easybgmResult$graph                  <- easybgmResult$estimates*easybgmResult$structure
      easybgmResult$sampleGraphs           <- easybgmFit$sample_graph
      easybgmResult$samplesPosterior       <- list(easybgmFit$samples_posterior)

      networks[[nw]] <- easybgmResult
    }


    # if model is ordinal Markov random field
    if (options[["model"]] == "omrf") {
      for (var in options[["variables"]]) {
        # Check if variables are binary or ordinal:
         if (!is.factor(dataset[[nw]][[var]])) {
          .quitAnalysis(gettext("Some of the variables you have entered for analysis are not binary or ordinal. Please make sure that all variables are binary or ordinal or change the model to gcgm."))
        }
      }
      # Estimate network
      jaspBase::.setSeedJASP(options)
      easybgmFit <- try(easybgm::easybgm(data       = dataset[[nw]],
                                         type       = "ordinal",
                                         package    = "bgms",
                                         iter       = options[["iter"]],
                                         save       = TRUE,
                                         centrality = FALSE,
                                         burnin     = options[["burnin"]],
                                         inclusion_probability = options[["gPrior"]],
                                         interaction_scale     = options[["interactionScale"]],
                                         edge_prior            = options[["edgePrior"]],
                                         threshold_alpha       = options[["thresholdAlpha"]],
                                         threshold_beta        = options[["thresholdBeta"]],
                                         beta_bernoulli_alpha  = options[["betaAlpha"]],
                                         beta_bernoulli_beta   = options[["betaBeta"]],
                                         dirichlet_alpha       = options[["dirichletAlpha"]]))



      if (isTryError(easybgmFit)) {
        message <- .extractErrorMessage(easybgmFit)
        .quitAnalysis(gettextf("The analysis failed with the following error message:\n%s", message))
      }


      # Extract results
      easybgmResult <- list()

      easybgmResult$graphWeights           <- easybgmFit$graph_weights
      easybgmResult$inclusionProbabilities <- easybgmFit$inc_probs
      easybgmResult$BF                     <- easybgmFit$inc_BF
      easybgmResult$structure              <- easybgmFit$structure
      easybgmResult$estimates              <- as.matrix(easybgmFit$parameters)
      easybgmResult$graph                  <- easybgmResult$estimates*easybgmResult$structure
      easybgmResult$sampleGraphs           <- easybgmFit$sample_graph
      easybgmResult$samplesPosterior       <- list(easybgmFit$samples_posterior)

      networks[[nw]] <- easybgmResult
    }
  }

  return(networks)
}

.bayesianNetworkAnalysisMainTable <- function(mainContainer, dataset, options, network) {

  if (is.null(network[["network"]]) || mainContainer$getError())
    return()

  tb <- mainContainer[["generalTable"]]
  nGraphs <- length(dataset)

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

  df <- data.frame(nodes = integer(nGraphs), nonZero = numeric(nGraphs), Sparsity = numeric(nGraphs))
  if (nGraphs > 1L)
    df[["info"]] <- names(network[["network"]])

  nVar <- ncol(network[["network"]][[1L]][["graph"]])
  for (i in seq_len(nGraphs)) {

    nw <- network[["network"]][[i]]

    df[["nodes"]][i]    <- nrow(nw[["graph"]])
    df[["nonZero"]][i]  <- paste(sum(nw[["graph"]][upper.tri(nw[["graph"]], diag = FALSE)] != 0), "/", (nVar * (nVar-1L)) %/% 2)
    df[["Sparsity"]][i] <- mean(nw[["graph"]][upper.tri(nw[["graph"]], diag = FALSE)] == 0)

  }
  tb$setData(df)
}

.bayesianNetworkAnalysisWeightMatrixTable <- function(mainContainer, network, options) {

  if (!is.null(mainContainer[["weightsTable"]]) || !options[["weightsMatrixTable"]])
    return()

  nGraphs <- max(1L, length(network[["network"]]))

  table <- createJaspTable(gettext("Weights matrix"), dependencies = "weightsMatrixTable") # position = 4
  table$addColumnInfo(name = "Variable", title = gettext("Variable"), type = "string")

  # shared titles
  overTitles <- names(network[["network"]])
  if (is.null(overTitles))
    overTitles <- gettext("Network")

}

.bayesianNetworkAnalysisPlotContainer <- function(mainContainer, network, options) {

  plotContainer <- mainContainer[["plotContainer"]]

  if (is.null(plotContainer)) {
    plotContainer <- createJaspContainer(dependencies = c("labelAbbreviation", "labelAbbreviationLength",
                                                          "legend", "variableNamesShown")) # position = 5
    mainContainer[["plotContainer"]] <- plotContainer
  }

  .networkAnalysisNetworkPlot                    (plotContainer, network, options, method = "Bayesian")
  .bayesianNetworkAnalysisEvidencePlot           (plotContainer, network, options)
  .bayesianNetworkAnalysisPosteriorStructurePlot (plotContainer, network, options)
  .bayesianNetworkAnalysisCentralityPlot         (plotContainer, network, options)
  .bayesianNetworkAnalysisPosteriorComplexityPlot(plotContainer, network, options)
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

  if (!is.null(mainContainer[["centralityTable"]]) || !options[["centralityTable"]])
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

  if (!is.null(plotContainer[["centralityPlot"]]) || !options[["centralityPlot"]])
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
        ggplot2::ylab("Posterior Probability") +
        ggplot2::xlab("Number of edges")  +
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
                                                                                "layout", "layoutSpringRepulsion", "edgeSize", "nodeSize", "colorNodesBy", "cut", "showDetails", "nodePalette",
                                                                                "legendSpecificPlotNumber", "model",
                                                                                "labelScale", "labelSize", "labelAbbreviation", "labelAbbreviationLength",
                                                                                "keepLayoutTheSame", "layoutX", "layoutY",
                                                                                "manualColorGroups", "groupColors", "colorGroupVariables", "groupAssigned", "manualColor",
                                                                                "legendToPlotRatio", "edgeLabels", "edgeLabelCex", "edgeLabelPosition"
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
    colorGroupVariables <- matrix(unlist(options[["colorGroupVariables"]]), ncol = 2L, byrow = TRUE)
    if (length(unique(colorGroupVariables[, 1L])) > 1L) {
      # user has defined groups and there are variables in the groups
      manualColorGroups <- matrix(unlist(options[["manualColorGroups"]]), ncol = 2L, byrow = TRUE)

      nGroups <- nrow(manualColorGroups)

      idx <- match(colorGroupVariables[, 1L], manualColorGroups[, 1L])

      groups <- vector("list", nGroups)
      names(groups) <- manualColorGroups[, 1L]
      for (i in seq_len(nGroups))
        groups[[i]] <- which(idx == i)

      nonEmpty <- lengths(groups) > 0L
      groups <- groups[nonEmpty]

      if (options[["manualColor"]])
        nodeColor <- manualColorGroups[nonEmpty, 2L]
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

    } else if (options[["legend"]] ==  "specificPlot: ") {

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
  nGraphs <- length(allNetworks)

  # we use an empty container without a name if there is only 1 graph. This container is hidden from the output but it
  # enables us to use the same code for a single network plot and for a collection of network plots.
  title <- if (nGraphs == 1L) gettext("Edge Evidence Plot") else gettext("Edge Evidence Plots")

  evidencePlotContainer <- createJaspContainer(title = title, position = 3, dependencies = c("evidencePlot",
                                                                                             "layout", "layoutSpringRepulsion", "edgeSize", "nodeSize", "colorNodesBy", "cut", "showDetails", "nodePalette",
                                                                                             "legendSpecificPlotNumber", "edgeInclusion", "edgeExclusion", "edgeAbsence",
                                                                                             "labelScale", "labelSize", "labelAbbreviation", "labelAbbreviationLength",
                                                                                             "keepLayoutTheSame", "layoutX", "layoutY", "edgeInclusionCriteria",
                                                                                             "manualColorGroups", "groupColors", "colorGroupVariables", "groupAssigned", "manualColor",
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
    labels <- base::abbreviate(labels, options[["labelAbbreviationLength"]])

  # do we need to draw legends?
  if (!is.null(groups) || !is.null(nodeNames)) {
    if (options[["legend"]] ==  "allPlots") {

      allLegends <- rep(TRUE, nGraphs)

    } else if (options[["legend"]] ==  "specificPlot: ") {

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
    evidencePlotContainer[[v]] <- createJaspPlot(title = v, width = width[v], height = height[v])

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
  if (options$edgeExclusion) graphColor[network[["BF"]] < (1 / options[["edgeInclusionCriteria"]])] <- "#990000"
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
      details             = options[["showDetails"]],
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
      edge.label.cex      = options[["edgeLabelCex"]],
      edge.label.position = options[["edgeLabelPosition"]]
    ))
}

.bayesianNetworkAnalysisWeightMatrixTable <- function(mainContainer, network, options) {

  if (!is.null(mainContainer[["weightsTable"]]) || !options[["weightsMatrixTable"]])
    return()

  variables <- unlist(options[["variables"]])
  nVar <- length(variables)
  nGraphs <- max(1L, length(network[["network"]]))

  table <- createJaspTable(gettext("Weights matrix"), dependencies = c("weightsMatrixTable")) #, position = 4
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

      toAdd <- allNetworks[[i]][["graph"]]
      toAdd <- as.data.frame(toAdd)
      names(toAdd) <- paste0(variables, i)

      TBcolumns <- cbind(TBcolumns, toAdd)
    }
    table$setData(TBcolumns)
  }
  mainContainer[["weightsTable"]] <- table
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
    table$addFootnote("Bayes factors with values of infinity indicate that the estimated posterior inclusion probability is either 1 or 0. Please see the help file for more information.")
  }
  mainContainer[["edgeEvidenceTable"]] <- table
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
      details             = options[["showDetails"]],
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
      edge.label.cex      = options[["edgeLabelCex"]],
      edge.label.position = options[["edgeLabelPosition"]]
    ))
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
    for (i in seq_len(nrow(network$samplesPosterior[[1]]))) {

      # TODO: this should call centralityTable rather than centralityPlot
      graph <- qgraph::centralityPlot(vectorToMatrix(network$samplesPosterior[[1]][i, ], as.numeric(nrow(network$estimates)), bycolumn = TRUE),
                                      include = measures,
                                      verbose = FALSE,
                                      print = FALSE,
                                      scale = "z-scores",
                                      labels = colnames(network$estimates))

      # Strength is removed if all values are 0. Here we fix this by setting the value to 0 manually
      # see https://github.com/jasp-stats/jasp-test-release/issues/2298
      if (nrow(graph$data) != nrow(centralityOutput) &&
          "Strength" %in% measures &&
          all(abs(network$samplesPosterior[[1]][i, ]) <= .Machine$double.eps)) {

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
