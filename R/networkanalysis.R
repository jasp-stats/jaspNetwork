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

# This is a temporary fix
# TODO: remove it when R will solve this problem!
gettextf <- function(fmt, ..., domain = NULL)  {
  return(sprintf(gettext(fmt, domain = domain), ...))
}

#' @importFrom jaspBase createJaspColumn createJaspContainer createJaspHtml createJaspPlot createJaspQmlSource createJaspState createJaspTable
#' decodeColNames encodeColNames .extractErrorMessage .hasErrors isTryError progressbarTick .quitAnalysis .readDataSetToEnd startProgressbar

#' @export
NetworkAnalysis <- function(jaspResults, dataset, options) {

  dataset <- .networkAnalysisReadData(dataset, options)

  mainContainer <- .networkAnalysisSetupMainContainerAndTable(jaspResults, dataset, options)
  .networkAnalysisErrorCheck(mainContainer, dataset, options)

  network <- .networkAnalysisRun(mainContainer, dataset, options)

  .networkAnalysisMainTable(mainContainer, dataset, options, network)

  .networkAnalysisCentralityTable  (mainContainer, network, options)
  .networkAnalysisClusteringTable  (mainContainer, network, options)
  .networkAnalysisPlotContainer    (mainContainer, network, options, dataset)
  .networkAnalysisWeightMatrixTable(mainContainer, network, options)

  # done last so that all other results are shown already
  .networkAnalysisBootstrap(mainContainer, network, options)

  return()
}

.networkAnalysisReadData <- function(dataset, options) {

  layoutVariables <- c(options[["layoutX"]], options[["layoutY"]])
  layoutVariables <- layoutVariables[layoutVariables != ""]
  hasLayoutData <- length(layoutVariables) > 0L
  if (hasLayoutData) {
    layoutData <- dataset[layoutVariables]
    layoutData <- jaspBase::excludeNaListwise(layoutData, layoutVariables)
    # remove the two layout columns
    dataset[layoutVariables] <- list(NULL)
  }

  if (options[["missingValues"]] == "listwise") {
    exclude <- c(options[["variables"]], options[["groupingVariable"]])
    dataset <- jaspBase::excludeNaListwise(dataset, exclude[exclude != ""])
  }

  if (options[["groupingVariable"]] == "") { # one network
    dataset <- list(dataset) # for compatability with the split behaviour
    names(dataset) <- "Network"
  } else { # multiple networks
    groupingVariableData <- dataset[[options[["groupingVariable"]]]]
    dataset[[options[["groupingVariable"]]]] <- NULL
    dataset <- split(dataset, groupingVariableData, drop = TRUE)
    attr(dataset, "groupingVariableData") <- groupingVariableData
  }

  if (hasLayoutData)
    attr(dataset, "layoutData") <- layoutData

  return(dataset)

}

.networkAnalysisErrorCheck <- function(mainContainer, dataset, options) {

  # some analyses, such as Sacha's EBIGglasso with cor_auto, completely ignore the missing argument
  # and always use pairwise information even though their documentation says they can do listwise
  if (length(options[["variables"]]) < 3)
    return()

  # check for errors, but only if there was a change in the data (which implies state[["network"]] is NULL)
  if (is.null(mainContainer[["networkState"]])) {

    customChecks <- NULL

    # check if data must be binarized
    if (options[["estimator"]] %in% c("isingFit", "isingSampler")) {

      for (i in seq_along(dataset)) {
        idx <- colnames(dataset[[i]]) != options[["groupingVariable"]]
        dataset[[i]][idx] <- bootnet::binarize(dataset[[i]][idx], split = options[["split"]], verbose = FALSE, removeNArows = FALSE)
      }

      if (options[["estimator"]] == "isingFit") {
        # required check since isingFit removes these variables from the analyses
        customChecks <- c(customChecks,
                          function() {
                            tbs <- apply(dataset, 2, table)
                            if (any(tbs <= 1)) {
                              idx <- which(tbs <= 1, arr.ind = TRUE)
                              return(gettextf(
                                "After binarizing the data too little variance remained in variable(s): %s.",
                                paste0(colnames(tbs[, idx[, 2], drop = FALSE]), collapse = ", ")
                              ))
                            }
                            return(NULL)
                          }
        )
      }
    }

    # default error checks
    # we check for missingRows due to see https://github.com/jasp-stats/jasp-issues/issues/1068
    checks <- c("infinity", "variance", "observations", "varCovData", "missingRows")

    # hasErrors wants the dataset as a single thing instead of as a list of data.frames
    # we could also just ignore the grouping elements and loop manually though
    groupingVariable <- attr(dataset, "groupingVariable")
    dataset <- Reduce(rbind.data.frame, dataset)

    if (options[["groupingVariable"]] != "") {
      # these cannot be chained unfortunately
      groupingVariableName <- options[["groupingVariable"]]
      dfGroup <- data.frame(groupingVariable)
      colnames(dfGroup) <- groupingVariableName
      .hasErrors(dataset = dfGroup,
                 type = c("missingValues", "factorLevels"),
                 missingValues.target = groupingVariableName,
                 factorLevels.target = groupingVariableName,
                 factorLevels.amount = "< 2",
                 exitAnalysisIfErrors = TRUE)
      dataset[[options[["groupingVariable"]]]] <- groupingVariable
      groupingVariable <- options[["groupingVariable"]]
    }

    # estimator 'mgm' requires some additional checks
    categoricalVars <- NULL
    if (options[["estimator"]] == "mgm" && "c" %in% options[["mgmVariableTypeData"]]) {
      categoricalVars <- options[["variables"]][options[["mgmVariableTypeData"]] == "c"]
      checks <- c(checks, "factorLevels")
    }

    # check for errors
    fun <- cor
    if (options[["correlationMethod"]] == "cov")
      fun <- cov

    .hasErrors(dataset = dataset,
               type = checks,
               variance.target = options[["variables"]], # otherwise the grouping variable always has variance == 0
               variance.grouping = groupingVariable,
               factorLevels.target = categoricalVars,
               factorLevels.amount = "> 10", # probably a misspecification of mgmVariableType when this happens.
               factorLevels.grouping = groupingVariable,
               observations.amount = " < 10",
               observations.grouping = groupingVariable,
               varCovData.grouping = groupingVariable,
               varCovData.corFun = fun,
               varCovData.corArgs = list(use = "pairwise"),
               missingRows.target = options[["variables"]],
               custom = customChecks,
               exitAnalysisIfErrors = TRUE)
  }
}

# tables ----
.networkAnalysisSetupMainContainerAndTable <- function(jaspResults, dataset, options) {

  mainContainer <- jaspResults[["mainContainer"]]
  if (is.null(mainContainer)) {
    mainContainer <- createJaspContainer(dependencies = c(
      # data
      "variables", "groupingVariable",
      # what kind of network is estimated
      "estimator",
      # arguments for the estimator
      "correlationMethod", "tuningParameter", "criterion", "isingEstimator",
      "nFolds", "split", "rule", "sampleSize", "thresholdBox", "thresholdString", "thresholdValue",
      "mgmContinuousVariables", "mgmCategoricalVariables", "mgmCountVariables",
      # general arguments
      "weightedNetwork", "signedNetwork", "missingValues"
    ))
    mainContainer$addCitation(.networkAnalysisBootnetGetRefs(options[["estimator"]]))
    jaspResults[["mainContainer"]] <- mainContainer
  }
  .networkAnalysisMainTableMeta(mainContainer, dataset, options)

  return(mainContainer)
}

.networkAnalysisMainTableMeta <- function(mainContainer, dataset, options) {

  if (is.null(mainContainer[["generalTable"]])) {
    tb <- createJaspTable(gettext("Summary of Network"), position = 1, dependencies = c(
      # These are dependencies because specifying them incorrectly is communicated as footnotes on this table
      "computedLayoutX", "computedLayoutY", "bootstrap", "bootstrapSamples", "minEdgeStrength",
      # these trigger recomputation in addition to footnotes
      "mgmContinuousVariables", "mgmCategoricalVariables", "mgmCountVariables"
    ))
    if (length(dataset) > 1L)
      tb$addColumnInfo(name = "info", title = gettext("Network"), type = "string")

    tb$addColumnInfo(name = "nodes",    title = gettext("Number of nodes"),          type = "integer")
    tb$addColumnInfo(name = "nonZero",  title = gettext("Number of non-zero edges"), type = "string")
    tb$addColumnInfo(name = "Sparsity", title = gettext("Sparsity"),                 type = "number")

    mainContainer[["generalTable"]] <- tb
  }
  return()
}

.networkAnalysisMainTable <- function(mainContainer, dataset, options, network) {

  if (is.null(network[["network"]]) || mainContainer$getError())
    return()

  tb <- mainContainer[["generalTable"]]
  nGraphs <- length(dataset)

  # footnotes
  if (options[["estimator"]] %in% c("isingFit", "isingSampler") && !all(unlist(dataset[!is.na(dataset)]) %in% 0:1))
    tb$addFootnote(gettextf("Data was binarized using %s. ",	options[["split"]]))

  if (!is.null(options[["colorNodesByData"]]) && length(options[["colorNodesByData"]]) != length(options[["variables"]])) {
    tb$addFootnote(
      gettextf("Only the first %1$d values of %2$s were used to color nodes (%3$d provided). ",
               length(options[["variables"]]),
               as.character(options[["colorNodesBy"]]),
               length(options[["colorNodesByData"]]))
    )
  }

  if (!is.null(options[["layoutMessage"]]))
    tb$addFootnote(options[["layoutMessage"]], symbol = gettext("<em>Warning: </em>"))

  if (!is.null(options[["colorNodesByDataMessage"]]))
    tb$addFootnote(options[["colorNodesByDataMessage"]], symbol = gettext("<em>Warning: </em>"))

  if (options[["minEdgeStrength"]] != 0) {

    ignored <- logical(nGraphs)
    for (i in seq_along(network[["network"]])) {
      ignored[i] <- all(abs(qgraph::getWmat(network[["network"]][[i]])) <= options[["minEdgeStrength"]])
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
  if (xor(options[["bootstrap"]], options[["bootstrapSamples"]] > 0L)) {
    text <- gettext("Bootstrapping is only done when 'Bootstrap network' is ticked and 'Number of Bootstraps' is larger than 0.")
    tb$addFootnote(text, symbol = gettext("<em>Warning: </em>"))
  }

  # fill in with info from bootnet:::print.bootnet
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

  if (!is.null(network[["layout"]][["message"]]))
    tb$addFootnote(network[["layout"]][["message"]], symbol = gettext("<em>Warning: </em>"))

  tb$setData(df)

}

.networkAnalysisCentralityTable <- function(mainContainer, network, options) {

  if (!is.null(mainContainer[["centralityTable"]]) || !options[["centralityTable"]])
    return()

  nGraphs <- max(1L, length(network[["network"]]))
  table <- createJaspTable(gettext("Centrality measures per variable"), position = 2,
                           dependencies = c("centralityTable", "centralityNormalization", "maxEdgeStrength", "minEdgeStrength"))
  table$addColumnInfo(name = "Variable", title = gettext("Variable"), type = "string")

  # shared titles
  overTitles <- names(network[["network"]])
  if (is.null(overTitles))
    overTitles <- gettext("Network") # paste0("Network", 1:nGraphs)

  for (i in seq_len(nGraphs)) { # three centrality columns per network
    table$addColumnInfo(name = paste0("betweenness", i),        title = gettext("Betweenness"),        type = "number", overtitle = overTitles[i])
    table$addColumnInfo(name = paste0("closeness", i),          title = gettext("Closeness"),          type = "number", overtitle = overTitles[i])
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
    names(toAdd) <- c("Variable", paste0(c("betweenness", "closeness", "Strength", "Expected influence"), i))
    if (i == 1L) {# if more than 1 network drop the first column which indicates the variable
      TBcolumns <- toAdd
    } else {
      toAdd <- toAdd[, -1L]
      TBcolumns <- cbind(TBcolumns, toAdd)
    }
  }

  table$setData(TBcolumns)
}

.networkAnalysisClusteringTable <- function(mainContainer, network, options) {

  if (!is.null(mainContainer[["clusteringTable"]]) || !options[["clusteringTable"]])
    return()

  nGraphs <- max(1L, length(network[["network"]]))
  if (is.null(network[["clustering"]])) {
    measureNms <- list(c(gettext("Barrat"), gettext("Onnela"), gettext("WS"), gettext("Zhang")))
  } else { # TODO: this is probably for backwards compatability... check if this can be removed!
    measureNms <- NULL
    for (i in seq_len(nGraphs))
      measureNms[[i]] <- colnames(network[["clustering"]][[i]])[-1L]

  }
  nMeasures <- lengths(measureNms)

  table <- createJaspTable(gettext("Clustering measures per variable"), position = 3, dependencies = "clusteringTable")
  table$addColumnInfo(name = "Variable", title = gettext("Variable"), type = "string")

  # shared titles
  overTitles <- names(network[["network"]])
  if (is.null(overTitles))
    overTitles <- gettext("Network") # paste0("Network", 1:nGraphs)

  for (i in seq_len(nGraphs))
    for (j in seq_len(nMeasures[i])) # four clustering columns per network
      table$addColumnInfo(name = paste0(measureNms[[i]][j], i), title = measureNms[[i]][j], type = "number", overtitle = overTitles[i])

  mainContainer[["clusteringTable"]] <- table
  if (is.null(network[["clustering"]]) || mainContainer$getError())
    return()

  # fill with results
  TBcolumns <- NULL
  footnotes <- attr(network[["clustering"]], "footnotes")
  for (i in seq_len(nGraphs)) {

    toAdd <- network[["clustering"]][[i]]
    names(toAdd) <- c("Variable", paste0(measureNms[[i]], i))
    if (i == 1L) {# if more than 1 network drop additional variable column
      TBcolumns <- toAdd
    } else {
      toAdd <- toAdd[, -1L]
      TBcolumns <- cbind(TBcolumns, toAdd)
    }
  }

  table$setData(TBcolumns)
  if (!is.null(footnotes)) {
    # ugly, but jaspResults doesn't suupport any other way
    colNames <- NULL
    for (j in footnotes[["column"]])
      colNames <- c(colNames, table$getColumnName(j))
    table$addFootnote(message = footnotes[["message"]], colNames = colNames)
  }
  if (any(nMeasures != 4L)) {
    nms <- names(network[["network"]])[nMeasures != 4L]
    s <- if (length(nms) == 1L) "" else "s"
    text <- gettextf("Clustering measures could not be computed for network%1$s: %2$s",
                     s, paste0(nms, collapse = ", "))
    table$addFootnote(text, symbol = gettext("<em>Warning: </em>"))
  }
}

.networkAnalysisWeightMatrixTable <- function(mainContainer, network, options) {

  if (!is.null(mainContainer[["weightsTable"]]) || !options[["weightsMatrixTable"]])
    return()

  variables <- unlist(options[["variables"]])
  nVar <- length(variables)
  nGraphs <- max(1L, length(network[["network"]]))

  table <- createJaspTable(gettext("Weights matrix"), dependencies = "weightsMatrixTable", position = 4)
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

      # we show by the order of the variables in options, but this need not correspond to the order
      # of the data passed to qgraph. So we need to reorder
      neworder <- match(variables, allNetworks[[i]][["labels"]])
      toAdd <- allNetworks[[i]][["graph"]][neworder, neworder]

      if (!options[["weightedNetwork"]])
        toAdd <- sign(toAdd)
      if (!options[["signedNetwork"]])
        toAdd <- abs(toAdd)

      toAdd <- as.data.frame(toAdd)
      names(toAdd) <- paste0(variables, i)

      TBcolumns <- cbind(TBcolumns, toAdd)
    }
    table$setData(TBcolumns)
  }
  mainContainer[["weightsTable"]] <- table
}

.networkAnalysisBootstrapTable <- function(bootstrapContainer, options, position) {

  # a table to show before bootstrapping to give some feedback to the user.
  # this used to give an ETA but with the progress bars that isn't really necessary anymore.
  if (!(length(options[["variables"]]) > 2L && options[["bootstrap"]] && options[["bootstrapSamples"]] > 0L))
    return()

  bootstrapType <- options[["bootstrapType"]]
  substr(bootstrapType, 1, 1) <- toupper(substr(bootstrapType, 1, 1)) # capitalize first letter

  table <- createJaspTable(title = gettext("Bootstrap summary of Network"), position = position)
  table$addColumnInfo(name = "type",               title = gettext("Type"), type = "string")
  table$addColumnInfo(name = "bootstrapSamples", title = gettext("Number of bootstraps"), type = "integer")
  table[["type"]]               <- bootstrapType
  table[["bootstrapSamples"]] <- options[["bootstrapSamples"]]
  bootstrapContainer[["table"]] <- table

}

# plots ----
.networkAnalysisPlotContainer <- function(mainContainer, network, options, dataset) {

  plotContainer <- mainContainer[["plotContainer"]]
  if (is.null(plotContainer)) {
    plotContainer <- createJaspContainer(position = 5, dependencies = c("labelAbbreviation", "labelAbbreviationLength",
                                                                        "legend", "variableNamesShown"))
    mainContainer[["plotContainer"]] <- plotContainer
  }

  .networkAnalysisNetworkPlot   (plotContainer, network, options, dataset = dataset)
  .networkAnalysisCentralityPlot(plotContainer, network, options)
  .networkAnalysisClusteringPlot(plotContainer, network, options)
}

.networkAnalysisCentralityPlot <- function(plotContainer, network, options) {

  if (!is.null(plotContainer[["centralityPlot"]]) || !options[["centralityPlot"]])
    return()

  measuresToShow <- unlist(options[c("betweenness", "closeness", "strength", "expectedInfluence")], use.names = FALSE)
  hasMeasures <- any(measuresToShow)

  width <- 200 + 120 * sum(measuresToShow)
  plot <- createJaspPlot(title = gettext("Centrality Plot"), position = 52, width = width,
                         dependencies = c("centralityPlot", "betweenness", "closeness", "strength", "expectedInfluence"))
  plotContainer[["centralityPlot"]] <- plot
  if (is.null(network[["centrality"]]) || plotContainer$getError() || !hasMeasures)
    return()

  wide <- network[["centrality"]]

  long <- .networkAnalysisReshapeWideToLong(wide, network, "centrality")

  if (!all(measuresToShow)) {
    measuresToFilter <- c("betweenness", "closeness", "Strength", "Expected Influence")[measuresToShow]
    long <- subset(long, measure %in% measuresToFilter)
  }

  # ensure that the first character is capitalized in the text above the subplots so it matches the centrality table
  levels(long[["measure"]]) <- stringr::str_to_title(levels(long[["measure"]]))

  .networkAnalysisMakePlotFromLong(plot, long, options)

}

.networkAnalysisClusteringPlot <- function(plotContainer, network, options) {

  if (!is.null(plotContainer[["clusteringPlot"]]) || !options[["clusteringPlot"]])
    return()

  plot <- createJaspPlot(title = gettext("Clustering Plot"), position = 53, dependencies = "clusteringPlot", width = 480)
  plotContainer[["clusteringPlot"]] <- plot
  if (is.null(network[["clustering"]]) || plotContainer$getError())
    return()

  wide <- network[["clustering"]]

  len <- lengths(wide)
  idx <- which.max(len)
  if (!all(len == len[idx])) {

    cnms <- colnames(wide[[idx]])[-1]
    for (i in which(len != len[idx])) {

      cnmsToAdd <- cnms[!(cnms %in% colnames(wide[[i]]))]
      for (nms in cnmsToAdd)
        wide[[i]][[nms]] <- NA
      wide[[i]] <- wide[[i]][colnames(wide[[idx]])]

    }
  }

  long <- .networkAnalysisReshapeWideToLong(wide, network, "clustering")
  .networkAnalysisMakePlotFromLong(plot, long, options)

}

.networkAnalysisReshapeWideToLong <- function(wide, network, what = c("centrality", "clustering")) {

  what <- match.arg(what)
  wideDf <- Reduce(rbind, wide)

  if (length(wide) > 1L) {
    wideDf[["type"]] <- rep(names(network[[what]]), each = nrow(wideDf) / length(wide))
    Long <- reshape2::melt(wideDf, id.vars = c("node", "type"))
    colnames(Long)[3L] <- "measure"
    Long[["graph"]] <- Long[["type"]]
    Long[["type"]] <- TRUE # options[["separateCentrality"]]
  } else {
    Long <- reshape2::melt(wideDf, id.vars = "node")
    colnames(Long)[2L] <- "measure"
    Long[["graph"]] <- NA
  }

  return(Long)

}

.networkAnalysisMakePlotFromLong <- function(jaspPlot, Long, options) {

  # "Long" is how qgraph refers to this object. This function transforms the
  # long object for centrality or clustering into a ggplot.

  # code modified from qgraph::centralityPlot(). Type and graph are switched so the legend title says graph
  if (options[["labelAbbreviation"]])
    Long[["node"]] <- base::abbreviate(Long[["node"]], options[["labelAbbreviationLength"]])

  # code modified from qgraph::centralityPlot(). Type and graph are switched so the legend title says graph
  Long <- Long[gtools::mixedorder(Long$node), ]
  Long$node <- factor(as.character(Long$node), levels = unique(gtools::mixedsort(as.character(Long$node))))

  Long$nodeLabel <- NA
  if (options[["variableNamesShown"]] == "inLegend") {
    Long$nodeLabel <- as.character(Long$node)
    Long$node <- factor(match(as.character(Long$node), unique(as.character(Long$node))))
    levels(Long$node) <- rev(levels(Long$node))
    Long$nodeLabel <- paste(as.character(Long$node), "=", Long$nodeLabel)
  }

  if (length(unique(Long$graph)) > 1L) {
    mapping <- ggplot2::aes(x = value, y = node, group = graph, colour = graph)
    guide   <- ggplot2::guides(color = ggplot2::guide_legend(title = options[["groupingVariable"]])) # change the name graph into the variable name for splitting
  } else {
    mapping <- ggplot2::aes(x = value, y = node, group = graph)
    guide   <- NULL
  }

  # add a fill element to the mapping -- this is only used to add a legend for the names of the nodes.
  hasNodeLabels <- !all(is.na(Long[["nodeLabel"]]))
  if (hasNodeLabels)
    mapping$fill <- as.name("nodeLabel")

  g <- ggplot2::ggplot(Long, mapping) + guide

  g <- g + ggplot2::geom_path() + ggplot2::geom_point() +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL)

  if (length(unique(Long$type)) > 1) {
    g <- g + ggplot2::facet_grid(type ~ measure, scales = "free")

  } else {
    g <- g + ggplot2::facet_grid(~measure, scales = "free")
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


.networkAnalysisOneNetworkPlot <- function(network, options, minE, layout, groups, maxE, labels, legend, shape,
                                           nodeColor, edgeColor, nodeNames, method = "frequentist") {


  wMat <- network[["graph"]]

  if (method != "Bayesian") {
    if (!options[["weightedNetwork"]]) {
      wMat <- sign(wMat)
    }
    if (!options[["signedNetwork"]]) {
      wMat <- abs(wMat)
    }
  }

  if (all(abs(wMat) <= minE))
    minE <- NULL

  return(
    qgraph::qgraph(
      input               = wMat,
      layout              = layout,
      groups              = groups,
      repulsion           = options[["layoutSpringRepulsion"]],
      cut                 = options[["cut"]],
      edge.width          = options[["edgeSize"]],
      node.width          = options[["nodeSize"]],
      maximum             = maxE,
      minimum             = minE,
      details             = options[["details"]],
      labels              = labels,
      palette             = if (options[["manualColor"]]) NULL else options[["nodePalette"]],
      theme               = .networkAnalysisJaspToBootnetEdgeColors(options[["edgePalette"]]),
      legend              = legend,
      shape               = shape,
      color               = nodeColor,
      edge.color          = edgeColor,
      nodeNames           = nodeNames,
      label.scale         = options[["labelScale"]],
      label.cex           = options[["labelSize"]],
      GLratio             = 1 / options[["legendToPlotRatio"]],
      edge.labels         = options[["edgeLabels"]],
      edge.label.cex      = options[["edgeLabelSize"]],
      edge.label.position = options[["edgeLabelPosition"]]
    ))
}

.networkAnalysisNetworkPlot <- function(plotContainer, network, options, method = "frequentist", dataset = NULL) {


  # Adjust options based on method
  if (method == "frequentist") {
    estimator <- options[["estimator"]]
  } else if (method == "Bayesian") {
    model <- options[["model"]]
  }

  if (!is.null(plotContainer[["networkPlotContainer"]]) || !options[["networkPlot"]])
    return()

  allNetworks <- network[["network"]]
  nGraphs <- length(allNetworks)

  # we use an empty container without a name if there is only 1 graph. This container is hidden from the output but it
  # enables us to use the same code for a single network plot and for a collection of network plots.
  title <- if (nGraphs == 1L) "" else gettext("Network Plots")

  networkPlotContainer <- createJaspContainer(title = title, position = 51, dependencies = c(
    "layout", "edgePalette", "layoutSpringRepulsion", "edgeSize", "nodeSize", "colorNodesBy",
    "maxEdgeStrength", "minEdgeStrength", "cut", "details", "nodePalette",
    "legendSpecificPlotNumber", "mgmVariableTypeShown",
    "labelScale", "labelSize", "labelAbbreviation", "labelAbbreviationLength",
    "layoutNotUpdated", "layoutX", "layoutY", "networkPlot",
    "manualColorGroups", "color", "colorGroupVariables", "group", "manualColor",
    "legendToPlotRatio", "edgeLabels", "edgeLabelSize", "edgeLabelPosition"
  ))
  plotContainer[["networkPlotContainer"]] <- networkPlotContainer

  if (is.null(network[["network"]]) || plotContainer$getError()) {
    networkPlotContainer[["dummyPlot"]] <- createJaspPlot(title = gettext("Network Plot"))
    return()
  }

  if (method == "Bayesian") {
    layout <- network[["layout"]] # calculated in .bayesianNetworkAnalysisRun()
  } else {
    layout <- network[["layout"]][["layout"]] # calculated in .networkAnalysisRun()
  }

  # ensure minimum/ maximum makes sense or ignore these parameters.
  # TODO: message in general table if they have been reset.
  minE <- options[["minEdgeStrength"]]
  maxE <- options[["maxEdgeStrength"]]

  if (minE == 0)
    minE <- NULL
  if (maxE == 0 || (!is.null(minE) && maxE <= minE))
    maxE <- NULL

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
  if (method == "frequentist" && estimator == "mgm") {

    idx <- integer(length(options[["variables"]]))
    nms <- c("mgmContinuousVariables", "mgmCategoricalVariables", "mgmCountVariables")
    for (i in seq_along(nms))
      idx[options[["variables"]] %in% options[[nms[[i]]]]] <- i
    # idx[i] is 1 if variable[i] %in% mgmContinuousVariables, 2 if in mgmCategoricalVariables, etc.

    # order of variables need not match dataset, and thus the order of types may be wrong
    newOrder <- match(colnames(dataset[[1L]]), options[["variables"]])
    # now we have variables[newOrder] == colnames(dataset[[1L]])
    idx <- idx[newOrder]

    if (options[["mgmVariableTypeShown"]] == "nodeShape") {
      #         gaussian, categorical, poisson
      opts <- c("circle", "square",    "triangle")
      shape <- opts[idx]

    } else if (options[["mgmVariableTypeShown"]] == "nodeColor") {
      #         gaussian,  categorical, poisson
      opts <- c("#00a9e6", "#fb8b00",   "#00ba63")
      nodeColor <- opts[idx]

    }
  }

  # TODO: footnote if legend off and nodenames used
  if (options[["variableNamesShown"]] == "inNodes") {
    nodeNames <- NULL

    if (method == "Bayesian") {
      if (nGraphs == 1) {
        labels <- colnames(allNetworks$Network$graph)
      } else {
        labels <- colnames(allNetworks$`1`$graph)
      }
    } else {
      labels <- allNetworks[[1]][["labels"]]
    }

  } else {

    if (method == "Bayesian") {
      if (nGraphs == 1) {
        nodeNames <- colnames(allNetworks$Network$graph)
      } else {
        nodeNames <- colnames(allNetworks$`1`$graph)
      }
    } else {
      nodeNames <- allNetworks[[1]][["labels"]]
    }

    labels <- seq_along(nodeNames)

  }

  if (method == "Bayesian") labels <- decodeColNames(labels)

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
    networkPlotContainer[[v]] <- createJaspPlot(title = v, width = width[v], height = height[v])

  jaspBase::.suppressGrDevice({

    for (v in names(allNetworks)) {

      networkToPlot <- allNetworks[[v]]
      if (method == "frequentist" && estimator == "mgm") {
        edgeColor <- networkToPlot[["results"]][["edgecolor"]]
        if (is.null(edgeColor)) # compatability issues
          edgeColor <- networkToPlot[["results"]][["pairwise"]][["edgecolor"]]
      }

      legend <- allLegends[[v]]
      networkPlotContainer[[v]]$plotObject <- .networkAnalysisOneNetworkPlot(
        network    = networkToPlot,
        options    = options,
        minE       = minE,
        maxE       = maxE,
        layout     = layout,
        groups     = groups,
        labels     = labels,
        legend     = legend,
        shape      = shape,
        nodeColor  = nodeColor,
        edgeColor  = edgeColor,
        nodeNames  = nodeNames,
        method     = method
      )
    }
  })
}

.networkAnalysisBootstrapPlot <- function(bootstrapContainer, bootstrapResults, options, statistic = "edge", position) {

  isEdge <- "edge" %in% statistic
  if (isEdge) {
    title <- gettext("Edge Stability")
    name  <- "EdgeStabilityPlots"
    opt   <- "statisticsEdges"
  } else {
    title <- gettext("Centrality Stability")
    name  <- "statisticsCentralityPlots"
    opt   <- "statisticsCentrality"
  }

  if (!options[[opt]] || !is.null(bootstrapContainer[[name]]))
    return()

  plotContainer <- createJaspContainer(title = title, position = position, dependencies = opt)

  for (v in names(bootstrapResults))
    plotContainer[[v]] <- createJaspPlot(title = v)

  bootstrapContainer[[name]] <- plotContainer

  for (v in names(bootstrapResults)) {

    bt <- bootstrapResults[[v]]
    p <- try(jaspBase::.suppressGrDevice(plot(bt, statistic = statistic, order = "sample")))

    # sometimes bootnet catches an error and returns a faulty ggplot object.
    # here we ensure that if there was any error, e contains that error.
    e <- p
    if (!isTryError(p))
      e <- try(jaspBase::.suppressGrDevice(print(p)))

    if (isTryError(e)) {
      plotContainer[[v]]$setError(gettextf("bootnet crashed with the following error message:\n%s", .extractErrorMessage(e)))
    } else {
      plotContainer[[v]]$plotObject <- p
    }
  }
}


# single network functions ----
.networkAnalysisRun <- function(mainContainer, dataset, options) {

  # list that contains state or is empty
  networkList <- list(
    network    = mainContainer[["networkState"]]$object,
    centrality = mainContainer[["centralityState"]]$object,
    clustering = mainContainer[["clusteringState"]]$object,
    layout     = mainContainer[["layoutState"]]$object
  )

  if (length(options[["variables"]]) <= 2L)
    return(networkList)

  if (is.null(networkList[["network"]]))
    tryCatch(
      networkList[["network"]] <- .networkAnalysisComputeNetworks(options, dataset),
      mgmError = function(e) {
        mainContainer[["generalTable"]]$addFootnote(e[["message"]])
      },
      error = function(e) {
        mainContainer$setError(.networkAnalysisCheckKnownErrors(e))
      }
    )

  if (!mainContainer$getError() && !is.null(networkList[["network"]])) {
    if (is.null(networkList[["centrality"]]))
      networkList[["centrality"]] <- .networkAnalysisComputeCentrality(networkList[["network"]], options[["centralityNormalization"]], options[["weightedNetwork"]], options[["signedNetwork"]])

    if (is.null(networkList[["clustering"]]))
      networkList[["clustering"]] <- .networkAnalysisComputeClustering(networkList[["network"]])

    if (is.null(networkList[["layout"]]))
      networkList[["layout"]] <- .networkAnalysisComputeLayout(networkList[["network"]], dataset, options)

    names(networkList[["network"]]) <- names(networkList[["centrality"]]) <- names(networkList[["clustering"]]) <-
      names(dataset)

    networkList[["status"]] <- .networkAnalysisNetworkHasErrors(networkList[["network"]])

    mainContainer[["networkState"]]    <- createJaspState(networkList[["network"]])
    mainContainer[["centralityState"]] <- createJaspState(networkList[["centrality"]], dependencies = c("centralityNormalization", "maxEdgeStrength", "minEdgeStrength"))
    mainContainer[["clusteringState"]] <- createJaspState(networkList[["clustering"]])
    mainContainer[["layoutState"]]     <- createJaspState(networkList[["layout"]], dependencies = c("layout", "layoutSpringRepulsion", "layoutX", "layoutY"))

    .networkAnalysisSaveLayout(mainContainer, options, networkList[["layout"]][["layout"]])

  }

  return(networkList)

}

.networkAnalysisMakeBootnetArgs <- function(dataset, options) {

  errorMessage <- NULL
  variables <- unlist(options[["variables"]])
  options[["rule"]] <- toupper(options[["rule"]])
  if (options[["correlationMethod"]] == "auto")
    options[["correlationMethod"]] <- "cor_auto"

  options[["isingEstimator"]] <- switch(options[["isingEstimator"]],
                                        "pseudoLikelihood" = "pl",
                                        "univariateRegressions" = "uni",
                                        "bivariateRegressions" = "bi",
                                        "logLinear" = "ll"
  )

  # additional checks for mgm
  level <- NULL # the levels of categorical variables
  type <- NULL  # the type of each variable
  if (options[["estimator"]] == "mgm") {

    nvar <- length(variables)
    level <- rep(1, nvar)
    type <- character(nvar)

    tempMat <- matrix(ncol = 2, byrow = TRUE, c(
      "mgmContinuousVariables",  "g",
      "mgmCategoricalVariables", "c",
      "mgmCountVariables",       "p"
    ))
    for (i in seq_len(nrow(tempMat))) {
      lookup <- options[[tempMat[i, 1L]]]
      if (length(lookup) > 0L) {
        idx <- match(lookup, variables)
        type[idx] <- tempMat[i, 2L]
      }
    }

    # order of variables need not match dataset, and thus the order of types may be wrong
    newOrder <- match(colnames(dataset[[1L]]), variables)
    # now we have variables[newOrder] == colnames(dataset[[1L]])
    type <- type[newOrder]

    if (any(type == "")) {
      message <- gettext("Please drag all variables to a particular type under \"Analysis options\".")
      e <- structure(class = c("mgmError", "error", "condition"), list(message = message, call = sys.call(-1)))
      stop(e)
    }

    # find out the levels of each categorical variable
    for (i in which(type == "c"))
      level[i] <- max(1L, nlevels(dataset[[1L]][[i]]), length(unique(dataset[[1L]][[i]])))


    # if (is.null(options[["mgmVariableTypeData"]])) {
    #   type <- rep("g", nvar)
    # } else {
    #   type <- options[["mgmVariableTypeData"]]
    #   invalidType <- is.na(type) | !(type %in% c("g", "c", "p"))
    #   type[invalidType] <- "g" # set missing to gaussian. TODO: add to table message.
    #
    #   if (any(invalidType))
    #     networkList[["message"]] <- c(networkList[["message"]],
    #                                   sprintf("The variable types supplied in %s contain missing values or values that do not start with 'g', 'c', or 'p'. These have been reset to gaussian ('g'). They were indices %s.",
    #                                           options[["mgmVariableType"]], paste(which(invalidType), sep = ", ")))
    #
    #   # find out the levels of each categorical variable
    #   for (i in which(type == "c"))
    #     level[i] <- max(1, nlevels(dataset[[1]][[i]]), length(unique(dataset[[1]][[i]])))
    #
    # }
  }

  if (options[["thresholdBox"]] == "value") {
    threshold <- options[["thresholdValue"]]
  } else { # options[["thresholdBox"]] == "method"
    threshold <- options[["thresholdMethod"]]
  }

  # names of .dots must match argument names of bootnet_{estimator name}
  .dots <- list(
    corMethod   = options[["correlationMethod"]],
    tuning      = options[["tuningParameter"]],
    missing     = options[["missingValues"]],
    method      = options[["isingEstimator"]],
    rule        = options[["rule"]],
    nFolds      = options[["nFolds"]],
    weighted    = options[["weightedNetwork"]],
    signed      = options[["signedNetwork"]],
    split       = options[["split"]],
    criterion   = options[["criterion"]],
    sampleSize  = options[["sampleSize"]],
    type        = type,
    lev         = level,
    threshold   = threshold
  )

  # get available arguments for specific network estimation function. Removes unused ones.
  # FOR FUTURE UPDATING: estimator MUST match name of function in bootnet literally (see ?bootnet::bootnet_EBICglasso).
  # the mapping from the jasp name to the bootnet name is done in .networkAnalysisJaspToBootnetEstimator
  estimator <- .networkAnalysisJaspToBootnetEstimator(options[["estimator"]])

  funArgs <- formals(utils::getFromNamespace(paste0("bootnet_", estimator), ns = "bootnet"))

  nms2keep <- names(funArgs)
  .dots <- .dots[names(.dots) %in% nms2keep]

  # for safety, when estimator is changed but missing was pairwise (the default).
  if (!isTRUE("pairwise" %in% eval(funArgs[["missing"]])))
    .dots[["missing"]] <- "listwise"

  # some manual adjustments for these estimators
  if (estimator == "huge") {

    if (.dots[["criterion"]] == "cv")
      .dots[["criterion"]] == "ebic"

  } else if (estimator == "mgm") {

    .dots[["criterion"]] <- toupper(.dots[["criterion"]]) # this function wants capitalized arguments

    if (!(.dots[["criterion"]] %in% eval(funArgs$criterion)))
      .dots[["criterion"]] <- "EBIC"

  } else if (estimator == "adalasso") {

    if (is.na(.dots[["nFolds"]]) || .dots[["nFolds"]] < 2) # estimator crashes otherwise
      .dots[["nFolds"]] <- 2

  }
  return(.dots)
}

.networkAnalysisComputeNetworks <- function(options, dataset) {

  if (!isNamespaceLoaded("bootnet"))
    try(loadNamespace("bootnet"), silent = TRUE)

  # setup all bootnet arguments, then loop over datasets to estimate networks
  .dots <- .networkAnalysisMakeBootnetArgs(dataset, options)

  networks <- vector("list", length(dataset))
  # for every dataset do the analysis
  for (nw in seq_along(dataset)) {

    data <- dataset[[nw]]

    # mgm requires integer instead of factor
    if (options[["estimator"]] == "mgm") {
      for (i in seq_along(data))
        if (!is.numeric(data[[i]]))
          data[[i]] <- as.integer(data[[i]])
    }

    jaspBase::.suppressGrDevice(
      msg <- capture.output(
        network <- bootnet::estimateNetwork(
          data    = data,
          default = .networkAnalysisJaspToBootnetEstimator(options[["estimator"]]),
          .dots   = .dots
        )
        , type = "message"
      )
    )

    network[["corMessage"]] <- msg
    networks[[nw]] <- network

  }
  return(networks)
}

.networkAnalysisComputeCentrality <- function(networks, centralityNormalization, weightedNetwork, signedNetwork) {

  centralities <- vector("list", length(networks))
  for (nw in seq_along(networks)) {

    network <- networks[[nw]]
    cent <- qgraph::centrality(network[["graph"]], weighted = weightedNetwork, signed = signedNetwork, all.shortest.paths = FALSE)

    # note: centrality table is (partially) calculated here so that centralityTable and centralityPlot don't compute the same twice.
    TBcent <- as.data.frame(cent[c("Betweenness", "Closeness", "InDegree", "OutDegree", "InExpectedInfluence", "OutExpectedInfluence")])
    colnames(TBcent) <- .camelCase(colnames(TBcent))

    # adapted from qgraph::centrality_auto
    wmat <- qgraph::getWmat(network$graph)
    if (weightedNetwork)
      wmat <- sign(wmat)

    if (signedNetwork)
      wmat <- abs(wmat)

    directedGraph <- ifelse(base::isSymmetric.matrix(object = wmat, tol = 1e-12), FALSE, TRUE)
    # Always assume we have a weighted graph, otherwise graphs without any edges are interpreted as unweighted
    weightedGraph <- TRUE #ifelse(all(qgraph::mat2vec(wmat) %in% c(0, 1)), FALSE, TRUE)

    if (directedGraph) {

      if (weightedGraph) {

        colnames(TBcent)[3:4] <- c("InStrength", "OutDegree")

      } else { # unweighted

        colnames(TBcent)[3:4] <- c("InDegree", "OutDegree")

      }

    } else { # undirected

      # divide betweenness by 2
      TBcent[, 1] <- TBcent[, 1] / 2
      # remove OutDegree and OutExpectedInfluence, since the network is undirected these are equal
      TBcent <- TBcent[-c(3, 5)]
      colnames(TBcent)[4L] <- "Expected Influence"

      if (weightedGraph) {

        colnames(TBcent)[3] <- "Strength"

        # Modification from the original function! We don't want the names to change and assume strength everywhere
        # we also do not support unweighted graphs anyway, so this would only arise due to a numerical fluke.
      # } else { # unweighted

        # colnames(TBcent)[3] <- "degree"

      }

    }

    # centVars <- apply(TBcent, 2, var)
    if (centralityNormalization == "normalized") { # normalize only if variances are nonzero

      for (i in 1:ncol(TBcent)) { # code modified from base::scale.default

        valid <- !is.na(TBcent[, i])

        if (sum(valid) != 0) {

          obs <- TBcent[valid, i]
          obs <- obs - mean(obs)
          stdev <- sqrt(sum(obs^2) / max(1, length(obs) - 1))
          if (stdev == 0) { # avoid division by zero
            TBcent[valid, i] <- obs
          } else {
            TBcent[valid, i] <- obs / stdev
          }
        }

      }

    } else if (centralityNormalization == "relative") {

      for (i in 1:ncol(TBcent)) { # code modified from qgraph::centralityTable

        mx <- max(abs(TBcent[, i]), na.rm = TRUE)

        if (mx != 0) # avoid division by zero
          TBcent[, i] <- TBcent[, i] / mx

      }

    } # else raw centrality measures -> do nothing

    TBcent[["node"]] <- network[["labels"]]
    nc <- ncol(TBcent)
    TBcent <- TBcent[c(nc, 1:(nc-1))] # put columns in intended order (of schema).
    cent <- TBcent

    centralities[[nw]] <- cent
  }

  return(centralities)
}

.networkAnalysisComputeClustering <- function(networks) {
  clusterings <- vector("list", length(networks))
  footnotes <- NULL
  for (nw in seq_along(networks)) {

    network <- networks[[nw]]
    clust <- qgraph::clusteringTable(network, labels = network[["labels"]], standardized = FALSE)
    clust <- reshape2::dcast(clust, graph + node + type ~ measure, value.var = "value")
    TBclust <- as.data.frame(clust[-c(1, 3)])
    TBclust <- TBclust[c(1L, order(colnames(TBclust)[-1L]) + 1L)] # alphabetical order

    # manually standardize the data. This allows for some extra checks on the standard deviation,
    # which can be very very small. If that happens, the data are pretty shitty or perhaps this clustering
    # statistic is just inappropriate for the particular data set. Either way, it makes the unit tests
    # fail because typically the small differences are just machine precision (and so is the sd).
    for (i in 2:ncol(TBclust)) {
      TBclust[, i] <- TBclust[, i] - mean(TBclust[, i])
      sdx <- sd(TBclust[, i])

      # perform some checks on the standard deviation basically, the data are pretty shitty if this happens, or perhaps this clustering
      # statistic is just inappropriate for the particular data set.
      if (!is.na(sdx) && sdx > sqrt(.Machine$double.eps))
        TBclust[, i] <- TBclust[, i] / sdx
      else if (is.null(footnotes))
        footnotes <- list(column = i, message = gettext("Coefficient could not be standardized because the variance is too small."))
      else
        footnotes$column <- c(footnotes$column, i)

      # this can only be true if the variance check failed. We set this to exactly zero because otherwise
      # the "smart" ggplot2 axis determination will create different axes on different platforms causing unit tests to fail
      if (all(TBclust[, i] < sqrt(.Machine$double.eps)))
        TBclust[, i] <- 0

    }
    clusterings[[nw]] <- TBclust

  }
  if (!is.null(footnotes))
    attr(clusterings, "footnotes") <- footnotes

  return(clusterings)
}

.networkAnalysisComputeLayout <- function(networks, dataset, options) {

  layout <- options[["layout"]]
  userLayout <- .networkAnalysisComputeUserLayout(dataset, options)
  if (layout != "data" || userLayout[["layoutInvalid"]]) {
    if (layout == "data")
      layout <- "circle"

    jaspBase::.suppressGrDevice(layout <- qgraph::averageLayout(networks, layout = layout, repulsion = options[["layoutSpringRepulsion"]]))

    rownames(layout) <- colnames(networks[[1L]])
    layout <- list(layout = layout)
    if (!is.null(userLayout[["message"]]))
      layout[["message"]] <- userLayout[["message"]]

  } else {
    # TODO: this should be done inside .networkAnalysisComputeUserLayout...
    layout <- userLayout
    nms <- networks[[1L]][["labels"]]
    idx <- match(nms, rownames(layout[["layoutData"]]))
    layout[["layoutData"]] <- layout[["layoutData"]][idx, ]
  }
  return(layout)
}

.networkAnalysisComputeUserLayout <- function(dataset, options) {

  # it turns out that we must save the layout in the A1 = ... style.
  if (options[["layoutX"]] != "" && options[["layoutY"]] != "") {

    layoutData <- attr(dataset, "layoutData")
    layoutXData <- layoutData[[1L]]
    layoutYData <- layoutData[[2L]]
    variables <- unlist(options[["variables"]])
    layoutInfo <- .networkAnalysisSanitizeLayoutData(variables, layoutXData, layoutYData, options[["layoutX"]], options[["layoutY"]])

  } else {
    layoutInfo <- list(layoutInvalid = TRUE)
  }

  return(layoutInfo)
}

.networkAnalysisSaveLayout <- function(mainContainer, options, layout) {

  # TODO: bail out if layout computation failed
  if (!options[["layoutSavedToData"]] || options[["layout"]] == "data" || is.null(layout) || mainContainer$getError())
    return()

  if (options[["computedLayoutX"]] == "" || options[["computedLayoutY"]] == "") {
    mainContainer[["generalTable"]]$addFootnote(gettext("Please supply a name for both the x and y-coordinates of the layout."))
    return()
  }

  variables <- unlist(options[["variables"]])
  mainContainer[["layoutXColumn"]] <- createJaspColumn(options[["computedLayoutX"]], c("layout", "layoutSpringRepulsion", "layoutX", "layoutY", "computedLayoutX"))
  mainContainer[["layoutYColumn"]] <- createJaspColumn(options[["computedLayoutY"]], c("layout", "layoutSpringRepulsion", "layoutX", "layoutY", "computedLayoutY"))
  mainContainer[["layoutXColumn"]]$setNominalText(paste(decodeColNames(variables), "=", layout[, 1L]))
  mainContainer[["layoutYColumn"]]$setNominalText(paste(decodeColNames(variables), "=", layout[, 2L]))

}

# bootstrap network functions ----
.networkAnalysisBootstrap <- function(mainContainer, network, options) {

  # this container is always created so that empty placeholds can be shown for the bootstrap plots
  bootstrapContainer <- mainContainer[["bootstrapContainer"]]
  if (is.null(bootstrapContainer)) {
    bootstrapContainer <- createJaspContainer("", position = 9, dependencies = c(
      "bootstrapSamples", "bootstrapType", "bootstrap"
    ))
    mainContainer[["bootstrapContainer"]] <- bootstrapContainer
  }

  .networkAnalysisBootstrapTable(bootstrapContainer, options, position = 91)

  bootstrapResults <- .networkAnalysisComputeBootstrap(bootstrapContainer, network, options)
  if (length(bootstrapResults) > 0L && !bootstrapContainer$getError())
    bootstrapContainer[["bootstrapState"]] <- createJaspState(bootstrapResults)

  .networkAnalysisBootstrapPlot(bootstrapContainer, bootstrapResults, options, statistic = "edge", position = 92)
  .networkAnalysisBootstrapPlot(bootstrapContainer, bootstrapResults, options, statistic = c("strength", "betweenness", "closeness"), position = 93)

}

.networkAnalysisComputeBootstrap <- function(bootstrapContainer, network, options) {

  bootstrapResult <- bootstrapContainer[["bootstrapState"]]$object
  bootstrapReady <- length(options[["variables"]]) > 2L && options[["bootstrap"]] && options[["bootstrapSamples"]] > 0L
  if (!bootstrapReady || is.null(network[["network"]]) || bootstrapContainer$getError() || !is.null(bootstrapResult))
    return(bootstrapResult)

  bootstrapResult <- list()
  allNetworks <- network[["network"]]
  nGraphs <- length(allNetworks)
  nCores <- .networkAnalysisGetNumberOfCores(options)
  noTicks <- if (options[["bootstrapType"]] == "jacknife") network[["network"]][[1L]][["nPerson"]] * nGraphs else options[["bootstrapSamples"]] * nGraphs

  startProgressbar(noTicks * 2L, "Bootstrapping network")

  original <- bootnet::bootnet
  expr0 <- as.expression(body(bootnet::bootnet))
  # replace all instances of setTxtProgressBar with setTxtProgressBarReplacement
  expr1 <- do.call(substitute, list(expr0[[1]], list(setTxtProgressBar = function(...) { progressbarTick() })))
  replacement <- bootnet::bootnet
  body(replacement) <- expr1

  jaspBase:::assignFunctionInPackage(replacement,      "bootnet", "bootnet")
  on.exit(jaspBase:::assignFunctionInPackage(original, "bootnet", "bootnet"))

  tryCatch({
    jaspBase::.suppressGrDevice({
      for (nm in names(allNetworks)) {
        bootstrapResult[[nm]] <- bootnet::bootnet(
          data       = allNetworks[[nm]],
          nBoots     = options[["bootstrapSamples"]],
          type       = options[["bootstrapType"]],
          nCores     = nCores,
          statistics = c("edge", "strength", "closeness", "betweenness"),
          labels     = options[["variables"]]
        )
      }
    })
  }, error = function(e) bootstrapContainer$setError(.extractErrorMessage(e))
  )
  return(bootstrapResult)
}

# helper functions ----
.networkAnalysisAddReferencesToTables <- function(results, options) {

  # get from every .meta element the type and check if it is "table"
  idxOfTables <- sapply(results[[".meta"]], `[[`, "type") == "table"
  # get the names of .meta elements that have type "table".
  namesOfTables <- sapply(results[[".meta"]], `[[`, "name")[idxOfTables]
  for (nm in namesOfTables) { # use names to index
    if (!is.null(results[[nm]])) { # if table is not empty add reference
      # results[[nm]][["citation"]] <- as.list(bootnet:::getRefs(options[["estimator"]]))
      results[[nm]][["citation"]] <- as.list(.networkAnalysisBootnetGetRefs(options[["estimator"]]))
    }
  }

  return(results)

}

.networkAnalysisGetNumberOfCores <- function(options) {

  if (options[["bootstrapParallel"]]) {
    nCores <- parallel::detectCores(TRUE)
    if (is.na(nCores)) # parallel::detectCores returns NA if unknown/ fails
      nCores <- 1
  } else {
    nCores <- 1
  }

  return(nCores)

}

.networkAnalysisNetworkHasErrors <- function(networks) {

  # returns TRUE if network has errors; FALSE otherwise
  if (any(sapply(networks, function(x) anyNA(x[["graph"]]))))
    return("error")

  return("success")

}

.networkAnalysisFindDataType <- function(variables, variableMatch, asNumeric = FALSE, char = "=",
                                         inputCheck = NULL, encodeFirstColumn = FALSE) {
  ## Input:
  # variables: options[["variables"]].
  # asNumeric: check if data can be converted to numeric.
  # char: what to match on, `=`` by default.
  # validInput: a function that returns TRUE if input is valid amd FALSE otherwise.

  ## Output:
  # Todo:

  errors <- list(fatal = FALSE)
  pattern <- sprintf("%s\\s*(?=[^%s]+$)", char, char)
  matches <- stringr::str_split(variableMatch, pattern)#":\\s*(?=[^:]+$)")

  lens <- lengths(matches)
  if (!all(lens == 2)) {
    errors <- c(errors, list(list(
      matches = unlist(matches[lens != 2]),
      type = "missingChar",
      message = gettextf("Some variables did not contain char (%s)", char)
    )))
    matches <- matches[lens == 2]
    if (length(matches) == 0) {# too poor input to continue
      errors[["fatal"]] <- TRUE
      return(list(errors = errors))
    }
  }

  # bind list to matrix
  matches <- do.call(rbind, matches)

  if (encodeFirstColumn) {
    matches[, 1L] <- encodeColNames(trimws(matches[, 1L]))
  }

  # check if matches appear in variables
  dontMatch <- !(matches[, 1] %in% variables)
  # check if matches appear in variables while being robust for whitespace issues
  doMatchWs <- trimws(matches[dontMatch, 1]) %in% variables
  matches[doMatchWs, 1] <- trimws(matches[doMatchWs, 1])

  trouble <- dontMatch & !doMatchWs
  if (any(trouble)) { # TRUE if any variable did not appear in any matches

    errors <-  c(errors, list(list(
      matches = unlist(matches[trouble, ]),
      type = "missingMatch",
      message = gettext("Some variables were not matched in the column names of the dataset.")
    )))

    matches <- matches[!trouble, ]
  }

  # unmatched variables
  unmatched <- variables[!(variables %in% matches[, 1])]

  values <- matches[, 2]
  if (asNumeric) {
    # replace commas by points
    values <- gsub(",", ".",  matches[, 2])
    # convert to numeric
    values <- suppressWarnings(as.numeric(values))

    if (anyNA(values)) {
      errors <-  c(errors, list(list(
        matches = matches[is.na(values), ],
        type = "missingNumeric",
        message = gettext("Some variables were not matched in the column names of the dataset.")
      )))
    }
  }

  # check if input actually is correct
  validInput <- NULL
  if (!is.null(inputCheck)) {
    validInput <- inputCheck(matches[, 2])
    errors[["fatal"]] <- !any(validInput)
  }

  return(list(matches = matches, values = values, unmatched = unmatched,
              validInput = validInput, errors = errors))

}

.networkAnalysisSanitizeLayoutData <- function(variables, layoutXData, layoutYData,
                                               nameX, nameY) {

  checksX <- .networkAnalysisFindDataType(variables = variables, variableMatch = layoutXData, char = "=",
                                          inputCheck = Negate(is.na), asNumeric = TRUE, encodeFirstColumn = TRUE)
  checksY <- .networkAnalysisFindDataType(variables = variables, variableMatch = layoutYData, char = "=",
                                          inputCheck = Negate(is.na), asNumeric = TRUE, encodeFirstColumn = TRUE)


  message <- NULL
  defMsg <- gettext("Supplied data for layout was not understood and instead a circle layout was used.")
  if (checksX[["errors"]][["fatal"]] || checksY[["errors"]][["fatal"]]) {

    if (checksX[["errors"]][["fatal"]] && checksY[["errors"]][["fatal"]]) {
      firstLine <- gettextf("Data supplied in %1$s AND %2$s could not be used to determine node locations.", nameX, nameY)
    } else if (checksX[["errors"]][["fatal"]]) {
      firstLine <- gettextf("Data supplied in %s could not be used to determine node locations.", nameX)
    } else {
      firstLine <- gettextf("Data supplied in %s could not be used to determine node locations.", nameY)
    }
    message <- gettextf("%1$s %2$s Data should only contain numeric:
                 -start with the column name of the variable.
                 -contain an '=' to distinguish between column name and coordinate.",
                        defMsg, firstLine)
  } else if (length(checksX[["unmatched"]]) > 0 || length(checksY[["unmatched"]]) > 0) {

    unmatchedX <- paste(checksX[["unmatched"]], collapse = ", ")
    unmatchedY <- paste(checksY[["unmatched"]], collapse = ", ")
    message <- defMsg
    if (unmatchedX != "")
      message <- sprintf(ngettext(length(checksX[["unmatched"]]), "%1$s X-Coordinates for variable %2$s was not understood.", "%1$s X-Coordinates for variables %2$s were not understood."), message, unmatchedX)

    if (unmatchedY != "")
      message <- sprintf(ngettext(length(checksY[["unmatched"]]), "%1$s Y-Coordinates for variable %2$s was not understood.", "%1$s Y-Coordinates for variables %2$s were not understood."), message, unmatchedY)
  }

  matchX <- checksX[["matches"]]
  matchY <- checksY[["matches"]]
  orderX <- match(variables, matchX[, 1])
  orderY <- match(variables, matchY[, 1])
  layoutData <- cbind(x = checksX[["values"]][orderX], y = checksY[["values"]][orderY])
  if (!is.null(layoutData)) {
    if (is.null(nrow(layoutData)))
      layoutData <- matrix(layoutData, nrow = 1)
    rownames(layoutData) <- variables
  }
  out <- list(layoutData = layoutData, message = message, layoutInvalid = !is.null(message))

  return(out)

}

.networkAnalysisCheckKnownErrors <- function(e) {

  # NOTE: this fails if glmnet decides to translate their error messages one day.
  # Unfortunately, these error appear for particular subsets of the data (from cross validation),
  # so it's difficult to traceable particular errors to the complete data.

  errmsg <- .extractErrorMessage(e[["message"]])
  # possibly add other checks here in the future
  dataIssue <- startsWith(errmsg, "y is constant") || endsWith(errmsg, "standardization step")

  ans <- if (dataIssue) {
    gettextf("bootnet crashed with the following error message:\n%s. <ul><li>Please check if there are variables in the network with little variance or few distinct observations.</li></ul>", errmsg)
  } else {
    gettextf("bootnet crashed with the following error message:\n%s", errmsg)
  }

  return(ans)

}

.camelCase <- function(x) {
  substr(x, 1L, 1L) <- tolower(substr(x, 1L, 1L))
  x
}

# option renaming helpers Jasp => bootnet ----
.networkAnalysisJaspToBootnetEstimator <- function(estimator) {
  switch(estimator,
         "ebicGlasso"   = "EBICglasso",
         "isingFit"     = "IsingFit",
         "isingSampler" = "IsingSampler",
         estimator
  )
}

.networkAnalysisJaspToBootnetEdgeColors <- function(edgePalette) {
  switch(edgePalette,
         "hollywood"    = "Hollywood",
         "borkulo"      = "Borkulo",
         "teamFortress" = "TeamFortress",
         "reddit"       = "Reddit",
         "fried"        = "Fried",
         edgePalette
  )
}


# functions modified from bootnet ----

# TODO: there is no init anymore. Does the remark below still apply???
# direct copy of bootnet:::getRefs. Otherwise, bootnet namespace gets loaded on init which takes pretty long.
.networkAnalysisBootnetGetRefs <- function (x) {
  citation <- switch(x,
                     none = "",
                     ebicGlasso = c("Friedman, J. H., Hastie, T., & Tibshirani, R. (2008). Sparse inverse covariance estimation with the graphical lasso. Biostatistics, 9 (3), 432-441.",
                                    "Foygel, R., & Drton, M. (2010). Extended Bayesian information criteria for Gaussian graphical models. , 23 , 2020-2028.",
                                    "Friedman, J. H., Hastie, T., & Tibshirani, R. (2014). glasso: Graphical lasso estimation of gaussian graphical models. Retrieved from https://CRAN.R-project.org/package=glasso",
                                    "Epskamp, S., Cramer, A., Waldorp, L., Schmittmann, V. D., & Borsboom, D. (2012). qgraph: Network visualizations of relationships in psychometric data. Journal of Statistical Software, 48 (1), 1-18."),
                     glasso = c("Friedman, J. H., Hastie, T., & Tibshirani, R. (2008). Sparse inverse covariance estimation with the graphical lasso. Biostatistics, 9 (3), 432-441.",
                                "Foygel, R., & Drton, M. (2010). Extended Bayesian information criteria for Gaussian graphical models. , 23 , 2020-2028.",
                                "Friedman, J. H., Hastie, T., & Tibshirani, R. (2014). glasso: Graphical lasso estimation of gaussian graphical models. Retrieved from https://CRAN.R-project.org/package=glasso",
                                "Epskamp, S., Cramer, A., Waldorp, L., Schmittmann, V. D., & Borsboom, D. (2012). qgraph: Network visualizations of relationships in psychometric data. Journal of Statistical Software, 48 (1), 1-18."),
                     isingFit = "van Borkulo, C. D., Borsboom, D., Epskamp, S., Blanken, T. F., Boschloo, L., Schoevers, R. A., & Waldorp, L. J. (2014). A new method for constructing networks from binary data. Scientific reports, 4 (5918), 1-10.",
                     isingSampler = c("Epskamp, S., Maris, G., Waldorp, L., & Borsboom, D. (in press). Network psychometrics. In P. Irwing, D. Hughes, & T. Booth (Eds.), Handbook of psychometrics. New York, NY, USA: Wiley.",
                                      "Epskamp, S. (2014). IsingSampler: Sampling methods and distribution functions for the Ising model. Retrieved from github.com/SachaEpskamp/IsingSampler"),
                     huge = "Zhao, T., Li, X., Liu, H., Roeder, K., Lafferty, J., & Wasserman, L. (2015). huge: High-dimensional undirected graph estimation. Retrieved from https://CRAN.R-project.org/package=huge",
                     adalasso = "Kraeamer, N., Schaeafer, J., & Boulesteix, A.-L. (2009). Regularized estimation of large-scale gene association networks using graphical gaussian models. BMC Bioinformatics, 10 (1), 1-24.",
                     mgm = "Jonas M. B. Haslbeck, Lourens J. Waldorp (2016). mgm: Structure Estimation for Time-Varying Mixed Graphical Models in high-dimensional Data arXiv preprint:1510.06871v2 URL http://arxiv.org/abs/1510.06871v2.")
  citation <- c(citation, "Epskamp, S., Borsboom, D., & Fried, E. I. (2016). Estimating psychological networks and their accuracy: a tutorial paper. arXiv preprint, arXiv:1604.08462.")
  citation
}
