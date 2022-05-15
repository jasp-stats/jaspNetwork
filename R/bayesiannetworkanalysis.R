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
  
  dataset <- .networkAnalysisReadData(dataset, options)
  # dataset <- scale(dataset, center = TRUE, scale = FALSE)
  
  mainContainer <- .bayesianNetworkAnalysisSetupMainContainerAndTable(jaspResults, dataset, options)
  .bayesianNetworkAnalysisErrorCheck(mainContainer, dataset, options)
  
  network <- .bayesianNetworkAnalysisRun(mainContainer, dataset, options)
  
  # Tables: 
  .bayesianNetworkAnalysisMainTable(mainContainer, dataset, options, network)
  .bayesianNetworkAnalysisWeightMatrixTable(mainContainer, network, options)
  .bayesianNetworkAnalysisEdgeEvidenceTable(mainContainer, network, options)
  
  .bayesianNetworkAnalysisPlotContainer(mainContainer, network, options)
  
  return()
}

.bayesianNetworkAnalysisSetupMainContainerAndTable <- function(jaspResults, dataset, options) {
  
  mainContainer <- jaspResults[["mainContainer"]]
  if (is.null(mainContainer)) {
    mainContainer <- createJaspContainer(dependencies = c("variables", "groupingVariable", 
                                                          "burnin", "iter", "gprior", "dfprior"))
    jaspResults[["mainContainer"]] <- mainContainer
  }
  .bayesianNetworkAnalysisMainTableMeta(mainContainer, dataset, options)
  
  return(mainContainer)
}

.bayesianNetworkAnalysisMainTableMeta <- function(mainContainer, dataset, options) {
  
  if (is.null(mainContainer[["generalTable"]])) {
    
    tb <- createJaspTable(gettext("Summary of Network"), position = 1, dependencies = c(
      "minEdgeStrength")) 
    
    if (length(dataset) > 1L) tb$addColumnInfo(name = "info", title = gettext("Network"), type = "string")
    
    tb$addColumnInfo(name = "nodes",    title = gettext("Number of nodes"),          type = "integer")
    tb$addColumnInfo(name = "nonZero",  title = gettext("Number of non-zero edges"), type = "string")
    tb$addColumnInfo(name = "Sparsity", title = gettext("Sparsity"),                 type = "number")
    
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
      # these cannot be chained unfortunately
      groupingVariableName <- options[["groupingVariable"]]
      dfGroup <- data.frame(groupingVariable)
      colnames(dfGroup) <- .v(groupingVariableName)
      .hasErrors(dataset = dfGroup,
                 type = c("missingValues", "factorLevels"),
                 missingValues.target = groupingVariableName,
                 factorLevels.target = groupingVariableName,
                 factorLevels.amount = "< 2",
                 exitAnalysisIfErrors = TRUE)
      dataset[[.v(options[["groupingVariable"]])]] <- groupingVariable
      groupingVariable <- options[["groupingVariable"]]
    }
  }
  
}

.bayesianNetworkAnalysisRun <- function(mainContainer, dataset, options) {
  
  # list that contains state or is empty
  networkList <- list(
    network    = mainContainer[["networkState"]]$object,
    layout     = mainContainer[["layoutState"]]$object
  )
  
  if (length(options[["variables"]]) <= 2L)
    return(networkList)
  
  if (is.null(networkList[["network"]]))
    tryCatch(
      networkList[["network"]] <- .bayesianNetworkAnalysisComputeNetworks(options, dataset),
      error = function(e) {
        mainContainer$setError(.extractErrorMessage(e))
      }
    )
  
  if (!mainContainer$getError() && !is.null(networkList[["network"]])) { #  if (!mainContainer$getError() && !is.null(networkList[["network"]])) {
    if (is.null(networkList[["layout"]]))
      networkList[["layout"]] <- .bayesianNetworkAnalysisComputeLayout(networkList[["network"]], dataset, options)
    
    names(networkList[["network"]]) <- names(dataset)
    
    mainContainer[["networkState"]]    <- createJaspState(networkList[["network"]])
    mainContainer[["layoutState"]]     <- createJaspState(networkList[["layout"]], 
                                                          dependencies = c("layout", "repulsion", "layoutX", "layoutY"))
    
    .networkAnalysisSaveLayout(mainContainer, options, networkList[["layout"]])
    
  }
  
  return(networkList)
}

.bayesianNetworkAnalysisComputeLayout <- function(networks, dataset, options) {
  
  layout <- options[["layout"]]
  userLayout <- .networkAnalysisComputeUserLayout(dataset, options)
  if (layout != "data" || userLayout[["layoutInvalid"]]) {
    #if (layout == "data")
      layout <- "circle"
    
    # CAN'T GET AVERAGE LAYOUT TO WORK WITH WEIGHT MATRICES
    #jaspBase::.suppressGrDevice(layout <- qgraph::averageLayout(networks, layout = layout, repulsion = options[["repulsion"]]))
    rownames(layout) <- .unv(colnames(networks[[1L]]))
    
  } else {
    layout <- userLayout[["layoutData"]]
    nms <- .unv(colnames(networks[[1L]]))
    idx <- match(nms, rownames(layout))
    layout <- layout[idx, ]
  }
  return(layout)
}

.bayesianNetworkAnalysisComputeNetworks <- function(options, dataset) {
  
  networks <- vector("list", length(dataset))
  
  # Detect network type: 
  method <- "ggm"
  
  # For every dataset (given the splitting variable) estimate a network:
  for (nw in seq_along(dataset)) {
    
    # Estimate network: 
    bdgraph_fit <- BDgraph::bdgraph(data       = as.data.frame(dataset[[nw]]),
                                    method     = method,
                                    algorithm  = "rjmcmc",
                                    iter       = as.numeric(options[["iter"]]),
                                    save       = TRUE,
                                    burnin     = as.numeric(options[["burnin"]]),
                                    g.start    = "empty",
                                    df.prior   = as.numeric(options[["dfprior"]]),
                                    g.prior    = as.numeric(options[["gprior"]]),
                                    cores      = 1)
    # Extract results:
    bdgraph_res <- list()
    
    bdgraph_res$graph_weights <- bdgraph_fit$graph_weights
    bdgraph_res$inc_probs <- as.matrix(BDgraph::plinks(bdgraph_fit))
    bdgraph_res$inc_probs  <- bdgraph_res$inc_probs + t(bdgraph_res$inc_probs)
    bdgraph_res$BF <- bdgraph_res$inc_probs / (1 - bdgraph_res$inc_probs)
    bdgraph_res$structure <- 1*(bdgraph_res$inc_probs > 0.5)
    bdgraph_res$estimates <- pr2pc(bdgraph_fit$K_hat)
    diag(bdgraph_res$estimates) <- 0
    bdgraph_res$graph <- bdgraph_res$estimates*bdgraph_res$structure
    #bdgraph_res$samples_posterior <- extractposterior(bdgraph_fit, as.data.frame(dataset[[nw]])[[1]]
    #bdgraph_res$centrality_strength <- centrality_strength(bdgraph_res)
  
    networks[[nw]] <- bdgraph_res
  }
  
  return(networks)
}

.bayesianNetworkAnalysisMainTable <- function(mainContainer, dataset, options, network) {
  
  if (is.null(network[["network"]]) || mainContainer$getError())  
    return()
  
  tb <- mainContainer[["generalTable"]]
  nGraphs <- length(dataset)
  
  # if (!is.null(options[["colorNodesByData"]]) && length(options[["colorNodesByData"]]) != length(options[["variables"]])) {
  #   tb$addFootnote(
  #     gettextf("Only the first %d values of %s were used to color nodes (%d provided). ",
  #              length(options[["variables"]]),
  #              as.character(options[["colorNodesBy"]]),
  #              length(options[["colorNodesByData"]]))
  #   )
  # }
  # 
  # if (!is.null(options[["layoutMessage"]]))
  #   tb$addFootnote(options[["layoutMessage"]], symbol = gettext("<em>Warning: </em>"))
  # 
  # if (!is.null(options[["colorNodesByDataMessage"]]))
  #   tb$addFootnote(options[["colorNodesByDataMessage"]], symbol = gettext("<em>Warning: </em>"))
  # 
  
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

.bayesianNetworkAnalysisPlotContainer <- function(mainContainer, network, options) {
  
  plotContainer <- mainContainer[["plotContainer"]]
  if (is.null(plotContainer)) {
    plotContainer <- createJaspContainer(position = 5, dependencies = c("abbreviateLabels", "abbreviateNoChars",
                                                                        "showLegend", "showVariableNames"))
    mainContainer[["plotContainer"]] <- plotContainer
  }
  
  .bayesianNetworkAnalysisNetworkPlot(plotContainer, network, options)
  .bayesianNetworkAnalysisEvidencePlot(plotContainer, network, options)
  .bayesianNetworkAnalysisStructurePlot(plotContainer, network, options)
  .bayesianNetworkAnalysisPosteriorStructurePlot(plotContainer, network, options)
  
}

.bayesianNetworkAnalysisPosteriorStructurePlot <- function(plotContainer, network, options) {
  
  if (!is.null(plotContainer[["posteriorStructurePlotContainer"]]) || !options[["plotPosteriorStructure"]])
    return()
  
  allNetworks <- network[["network"]]
  nGraphs <- length(allNetworks)
  
  title <- if (nGraphs == 1L) "" else gettext("Posterior Probability Structure Plots")
  
  posteriorStructurePlotContainer <- createJaspContainer(title = title, position = 51, dependencies = c("plotPosteriorStructure"))
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
      
      plot <- plot(sort(networkToPlot$graph_weights/sum(networkToPlot$graph_weights), decreasing = TRUE), bty = "n")
    
      posteriorStructurePlotContainer[[v]]$plotObject <- plot

    }
  })
  
}

.bayesianNetworkAnalysisStructurePlot <- function(plotContainer, network, options) {
  
  if (!is.null(plotContainer[["structurePlotContainer"]]) || !options[["plotStructure"]])
    return()
  
  allNetworks <- network[["network"]]
  nGraphs <- length(allNetworks)
  
  title <- if (nGraphs == 1L) "" else gettext("Structure Plots")
  
  structurePlotContainer <- createJaspContainer(title = title, position = 51, dependencies = c(
    "layout", "repulsion", "edgeSize", "nodeSize", "colorNodesBy", "cut", "showDetails", "nodePalette",
    "legendNumber",
    "scaleLabels", "labelSize", "abbreviateLabels", "abbreviateNoChars",
    "keepLayoutTheSame", "layoutX", "layoutY", "plotNetwork",
    "groupNames", "groupColors", "variablesForColor", "groupAssigned", "manualColors",
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
  
  if (length(options[["variablesForColor"]]) > 1L) {
    variablesForColor <- matrix(unlist(options[["variablesForColor"]]), ncol = 2L, byrow = TRUE)
    if (length(unique(variablesForColor[, 1L])) > 1L) {
      # user has defined groups and there are variables in the groups
      groupNames <- matrix(unlist(options[["groupNames"]]), ncol = 2L, byrow = TRUE)
      nGroups <- nrow(groupNames)
      
      idx <- match(variablesForColor[, 1L], groupNames[, 1L])
      
      groups <- vector("list", nGroups)
      names(groups) <- groupNames[, 1L]
      for (i in seq_len(nGroups))
        groups[[i]] <- which(idx == i)
      
      nonEmpty <- lengths(groups) > 0L
      groups <- groups[nonEmpty]
      
      if (options[["manualColors"]])
        nodeColor <- groupNames[nonEmpty, 2L]
    }
  }
  
  # defaults
  shape <- "circle"
  edgeColor <- NULL
  
  # TODO: footnote if legend off and nodenames used
  if (options[["showVariableNames"]] == "In nodes") {
    nodeNames <- NULL
    labels <- .unv(colnames(allNetworks$Network$graph))
    
  } else {
    
    nodeNames <- .unv(colnames(allNetworks$Network$graph))
    labels <- seq_along(nodeNames)
    
  }
  
  if (options[["abbreviateLabels"]])
    labels <- base::abbreviate(labels, options[["abbreviateNoChars"]]) # THIS DOESN'T WORK (ALSO NOT IN NETWORKANALYSIS)
  
  # do we need to draw legends?
  if (!is.null(groups) || !is.null(nodeNames)) {
    if (options[["showLegend"]] ==  "All plots") {
      
      allLegends <- rep(TRUE, nGraphs)
      
    } else if (options[["showLegend"]] ==  "In plot number: ") {
      
      if (options[["legendNumber"]] > nGraphs) {
        
        allLegends[nGraphs] <- TRUE
        
      } else if (options[["legendNumber"]] < 1L) {
        
        allLegends[1L] <- TRUE
        
      } else {
        
        allLegends[options[["legendNumber"]]] <- TRUE
        
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

.bayesianNetworkAnalysisNetworkPlot <- function(plotContainer, network, options) {
  
  if (!is.null(plotContainer[["networkPlotContainer"]]) || !options[["plotNetwork"]])
    return()
  
  allNetworks <- network[["network"]]
  nGraphs <- length(allNetworks)
  
  # we use an empty container without a name if there is only 1 graph. This container is hidden from the output but it
  # enables us to use the same code for a single network plot and for a collection of network plots.
  title <- if (nGraphs == 1L) "" else gettext("Network Plots")
  
  networkPlotContainer <- createJaspContainer(title = title, position = 51, dependencies = c(
    "layout", "edgeColors", "repulsion", "edgeSize", "nodeSize", "colorNodesBy",
    "maxEdgeStrength", "minEdgeStrength", "cut", "showDetails", "nodePalette",
    "legendNumber",
    "scaleLabels", "labelSize", "abbreviateLabels", "abbreviateNoChars",
    "keepLayoutTheSame", "layoutX", "layoutY", "plotNetwork",
    "groupNames", "groupColors", "variablesForColor", "groupAssigned", "manualColors",
    "legendToPlotRatio", "edgeLabels", "edgeLabelCex", "edgeLabelPosition"
  ))
  plotContainer[["networkPlotContainer"]] <- networkPlotContainer
  
  if (is.null(network[["network"]]) || plotContainer$getError()) {
    networkPlotContainer[["dummyPlot"]] <- createJaspPlot(title = gettext("Network Plot"))
    return()
  }
  
  layout <- network[["layout"]] # calculated in .bayesianNetworkAnalysisRun()
  
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
  
  if (length(options[["variablesForColor"]]) > 1L) {
    variablesForColor <- matrix(unlist(options[["variablesForColor"]]), ncol = 2L, byrow = TRUE)
    if (length(unique(variablesForColor[, 1L])) > 1L) {
      # user has defined groups and there are variables in the groups
      groupNames <- matrix(unlist(options[["groupNames"]]), ncol = 2L, byrow = TRUE)
      nGroups <- nrow(groupNames)
      
      idx <- match(variablesForColor[, 1L], groupNames[, 1L])
      
      groups <- vector("list", nGroups)
      names(groups) <- groupNames[, 1L]
      for (i in seq_len(nGroups))
        groups[[i]] <- which(idx == i)
      
      nonEmpty <- lengths(groups) > 0L
      groups <- groups[nonEmpty]
      
      if (options[["manualColors"]])
        nodeColor <- groupNames[nonEmpty, 2L]
    }
  }
  
  # defaults
  shape <- "circle"
  edgeColor <- NULL
  
  # TODO: footnote if legend off and nodenames used
  if (options[["showVariableNames"]] == "In nodes") {
    nodeNames <- NULL
    labels <- .unv(colnames(allNetworks$Network$graph))
    
  } else {
    
    nodeNames <- .unv(colnames(allNetworks$Network$graph))
    labels <- seq_along(nodeNames)
    
  }
  
  if (options[["abbreviateLabels"]])
    labels <- base::abbreviate(labels, options[["abbreviateNoChars"]]) # THIS DOESN'T WORK (ALSO NOT IN NETWORKANALYSIS)
  
  # do we need to draw legends?
  if (!is.null(groups) || !is.null(nodeNames)) {
    if (options[["showLegend"]] ==  "All plots") {
      
      allLegends <- rep(TRUE, nGraphs)
      
    } else if (options[["showLegend"]] ==  "In plot number: ") {
      
      if (options[["legendNumber"]] > nGraphs) {
        
        allLegends[nGraphs] <- TRUE
        
      } else if (options[["legendNumber"]] < 1L) {
        
        allLegends[1L] <- TRUE
        
      } else {
        
        allLegends[options[["legendNumber"]]] <- TRUE
        
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
      
      legend <- allLegends[[v]]
      networkPlotContainer[[v]]$plotObject <- .bayesianNetworkAnalysisOneNetworkPlot(
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
        nodeNames  = nodeNames
      )
    }
  })
}

.bayesianNetworkAnalysisOneNetworkPlot <- function(network, options, minE, layout, groups, maxE, labels, legend, shape,
                                                   nodeColor, edgeColor, nodeNames) {
  
  wMat <- network$graph
  
  if (all(abs(wMat) <= minE)) 
    minE <- NULL
  
  return(
    qgraph::qgraph(
      input               = wMat,
      layout              = layout,
      groups              = groups,
      repulsion           = options[["repulsion"]],
      cut                 = options[["cut"]],
      edge.width          = options[["edgeSize"]],
      node.width          = options[["nodeSize"]],
      maximum             = maxE,
      minimum             = minE,
      details             = options[["showDetails"]],
      labels              = labels,
      palette             = if (options[["manualColors"]]) NULL else options[["nodePalette"]],
      theme               = options[["edgeColors"]],
      legend              = legend,
      shape               = shape,
      color               = nodeColor,
      edge.color          = edgeColor,
      nodeNames           = nodeNames,
      label.scale         = options[["scaleLabels"]],
      label.cex           = options[["labelSize"]],
      GLratio             = 1 / options[["legendToPlotRatio"]],
      edge.labels         = options[["edgeLabels"]],
      edge.label.cex      = options[["edgeLabelCex"]],
      edge.label.position = options[["edgeLabelPosition"]]
    ))
}

.bayesianNetworkAnalysisEvidencePlot <- function(plotContainer, network, options) {
  
  if (!is.null(plotContainer[["evidencePlotContainer"]]) || !options[["plotEvidence"]])
    return()
  
  allNetworks <- network[["network"]]
  nGraphs <- length(allNetworks)
  
  # we use an empty container without a name if there is only 1 graph. This container is hidden from the output but it
  # enables us to use the same code for a single network plot and for a collection of network plots.
  title <- if (nGraphs == 1L) "" else gettext("Edge Evidence Plots")
  
  evidencePlotContainer <- createJaspContainer(title = title, position = 52, dependencies = c(
    "layout", "repulsion", "edgeSize", "nodeSize", "colorNodesBy", "cut", "showDetails", "nodePalette",
    "legendNumber",
    "scaleLabels", "labelSize", "abbreviateLabels", "abbreviateNoChars",
    "keepLayoutTheSame", "layoutX", "layoutY", "plotNetwork",
    "groupNames", "groupColors", "variablesForColor", "groupAssigned", "manualColors",
    "legendToPlotRatio"
  ))
  plotContainer[["evidencePlotContainer"]] <- evidencePlotContainer
  
  if (is.null(network[["network"]]) || plotContainer$getError()) {
    evidencePlotContainer[["dummyPlot"]] <- createJaspPlot(title = gettext("Edge Evidence Plot"))
    return()
  }
  
  layout <- network[["layout"]] # calculated in .bayesianNetworkAnalysisRun()
  
  groups <- NULL
  nodeColor <- NULL
  allLegends <- rep(FALSE, nGraphs) # no legends
  
  if (length(options[["variablesForColor"]]) > 1L) {
    variablesForColor <- matrix(unlist(options[["variablesForColor"]]), ncol = 2L, byrow = TRUE)
    if (length(unique(variablesForColor[, 1L])) > 1L) {
      # user has defined groups and there are variables in the groups
      groupNames <- matrix(unlist(options[["groupNames"]]), ncol = 2L, byrow = TRUE)
      nGroups <- nrow(groupNames)
      
      idx <- match(variablesForColor[, 1L], groupNames[, 1L])
      
      groups <- vector("list", nGroups)
      names(groups) <- groupNames[, 1L]
      for (i in seq_len(nGroups))
        groups[[i]] <- which(idx == i)
      
      nonEmpty <- lengths(groups) > 0L
      groups <- groups[nonEmpty]
      
      if (options[["manualColors"]])
        nodeColor <- groupNames[nonEmpty, 2L]
    }
  }
  
  # defaults
  shape <- "circle"
  
  # TODO: footnote if legend off and nodenames used
  if (options[["showVariableNames"]] == "In nodes") {
    nodeNames <- NULL
    labels <- .unv(colnames(allNetworks$Network$graph))
    
  } else {
    
    nodeNames <- .unv(colnames(allNetworks$Network$graph))
    labels <- seq_along(nodeNames)
    
  }
  
  if (options[["abbreviateLabels"]])
    labels <- base::abbreviate(labels, options[["abbreviateNoChars"]]) # THIS DOESN'T WORK (ALSO NOT IN NETWORKANALYSIS)
  
  # do we need to draw legends?
  if (!is.null(groups) || !is.null(nodeNames)) {
    if (options[["showLegend"]] ==  "All plots") {
      
      allLegends <- rep(TRUE, nGraphs)
      
    } else if (options[["showLegend"]] ==  "In plot number: ") {
      
      if (options[["legendNumber"]] > nGraphs) {
        
        allLegends[nGraphs] <- TRUE
        
      } else if (options[["legendNumber"]] < 1L) {
        
        allLegends[1L] <- TRUE
        
      } else {
        
        allLegends[options[["legendNumber"]]] <- TRUE
        
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
  
  graph_color <- network[["BF"]]
  graph_color <-  ifelse(network[["BF"]] < 10 & network[["BF"]] > .1, graph_color <- "#bfbfbf", graph_color <- "#36648b")
  graph_color[network[["BF"]] < .1] <- "#990000"
  
  return(
    qgraph::qgraph(
      input               = matrix(1, ncol = nrow(network[["graph"]]), nrow = nrow(network[["graph"]])),
      layout              = layout,
      groups              = groups,
      repulsion           = options[["repulsion"]],
      cut                 = options[["cut"]],
      edge.width          = options[["edgeSize"]],
      node.width          = options[["nodeSize"]],
      details             = options[["showDetails"]],
      labels              = labels,
      palette             = if (options[["manualColors"]]) NULL else options[["nodePalette"]],
      legend              = legend,
      shape               = shape,
      color               = nodeColor,
      edge.color          = graph_color,
      nodeNames           = nodeNames,
      label.scale         = options[["scaleLabels"]],
      label.cex           = options[["labelSize"]],
      GLratio             = 1 / options[["legendToPlotRatio"]],
      edge.labels         = options[["edgeLabels"]],
      edge.label.cex      = options[["edgeLabelCex"]],
      edge.label.position = options[["edgeLabelPosition"]]
    ))
}

.bayesianNetworkAnalysisWeightMatrixTable <- function(mainContainer, network, options) {
  
  if (!is.null(mainContainer[["weightsTable"]]) || !options[["tableWeightsMatrix"]])
    return()
  
  variables <- unlist(options[["variables"]])
  nVar <- length(variables)
  nGraphs <- max(1L, length(network[["network"]]))
  
  table <- createJaspTable(gettext("Weights matrix"), dependencies = c("tableWeightsMatrix"), position = 4)
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
  
  if (!is.null(mainContainer[["edgeEvidenceTable"]]) || !options[["tableEdgeEvidence"]])
    return()
  
  variables <- unlist(options[["variables"]])
  nVar <- length(variables)
  nGraphs <- max(1L, length(network[["network"]]))
  
  table <- createJaspTable(gettext("Edge evidence probability table"), dependencies = c("tableEdgeEvidence"), position = 4)
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
      
      toAdd <- allNetworks[[i]][["inc_probs"]]
      toAdd <- as.data.frame(toAdd)
      names(toAdd) <- paste0(variables, i)
      
      TBcolumns <- cbind(TBcolumns, toAdd)
    }
    table$setData(TBcolumns)
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
      repulsion           = options[["repulsion"]],
      cut                 = options[["cut"]],
      edge.width          = options[["edgeSize"]],
      node.width          = options[["nodeSize"]],
      details             = options[["showDetails"]],
      labels              = labels,
      palette             = if (options[["manualColors"]]) NULL else options[["nodePalette"]],
      legend              = legend,
      shape               = shape,
      color               = nodeColor,
      nodeNames           = nodeNames,
      label.scale         = options[["scaleLabels"]],
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

# Turns vector into matrix
vector_to_matrix <- function(vec, p, diag = F, bycolumn = F) {
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

# Transform precision into partial correlations for interpretation
pr2pc <- function(K) {
  D.Prec = diag(diag(K)^(-.5))
  R <- diag(2,dim(K)[1])-D.Prec%*%K%*%D.Prec
  colnames(R) <- colnames(K)
  rownames(R) <- rownames(K)
  return(R)
}

# BDgraph stores graphs as byte strings for efficiency
string2graph <- function(Gchar, p) {
  Gvec = rep(0, p*(p-1)/2)
  edges <- which(unlist(strsplit(as.character(Gchar), "")) == 1)
  Gvec[edges] = 1
  G <- matrix(0, p, p)
  G[upper.tri(G)] <- Gvec
  G = G + t(G)
  return(G) 
}

# BDgraph extract posterior distribution for estimates
extractposterior <- function(fit, data){
  m <- length(fit$all_graphs)
  k <- 5000 
  n <- nrow(data)
  p <- ncol(data)
  j <- 1
  densities <- rep(0, k)
  #Rs = array(0, dim=c(k, p, p))
  Rs = matrix(0, nrow = k, ncol = (p*(p-1))/2)
  S <- t(data) %*% data
  for (i in seq(1, m, length.out=k)) {
    graph_ix <- fit$all_graphs[i]
    G <- string2graph(fit$sample_graphs[graph_ix], p)
    K <- BDgraph::rgwish(n=1, adj=G, b=3+n, D=diag(p) + S)
    
    #Rs[j,,] <- pr2pc(K)
    Rs[j,] <- as.vector(pr2pc(K)[upper.tri(pr2pc(K))])
    densities[j] <- sum(sum(G)) / (p*(p-1))
    j <- j + 1
  }
  return(list(Rs, densities))
}

# Samples from the G-wishart distribution
gwish_samples <- function(G, S, nsamples=1000) {
  p <- nrow(S)
  #Rs <- array(0, dim=c(nsamples, p, p))
  Rs = matrix(0, nrow = nsamples, ncol = (p*(p-1))/2)
  
  for (i in 1:nsamples) {
    K <- BDgraph::rgwish(n=1, adj=G, b=3+n, D=diag(p) + S)*(G + diag(p))
    Rs[i,] <- as.vector(pr2pc(K)[upper.tri(pr2pc(K))])
    #Rs[i,,] <- .pr2pc(K)
  }
  return(Rs)
}


# Centrality of weighted graphs
centrality_strength <- function(res){
  Nsamples <- nrow(res$samples_posterior)
  p <- nrow(res$graph)
  strength_samples <- matrix(0, nrow = Nsamples, ncol = p)
  for(i in 1:Nsamples){
    strength_samples[i, ] <- rowSums(abs(vector_to_matrix(res$samples_posterior[i,], p, bycolumn = T)))
  }
  strength_mean <- colMeans(strength_samples)
  return(list(centrality_strength_samples = strength_samples, centrality_strength_mean = strength_mean))
}

# Centrality of unweighted graphs 
centrality_graph <- function(fit, include = c("degree", "closeness", "betweenness") ){
  # amount of visited structures
  len <- length(fit$sample_graphs)
  
  # objects to store graph centrality measures
  degree <- matrix(0, nrow = len, ncol = p)
  betweenness <- matrix(0, nrow = len, ncol = p)
  closeness <- matrix(0, nrow = len, ncol = p)
  
  # Obtain centrality measures for each graph
  for (i in 1:len){
    graph_matrix <- vector_to_matrix(as.numeric(unlist(strsplit(fit$sample_graphs[1], ""))), p , bycolumn = T)
    graph_graph <- igraph::as.undirected(graph.adjacency(graph_matrix, weighted = T))
    
    degree[i, ] <- igraph::degree(graph_graph)
    betweenness[i, ] <- igraph::betweenness(graph_graph)
    closeness[i, ] <- igraph::closeness(graph_graph)
  }
  # save centrality measures of interest
  centrality_graph <- list()
  if("degree" %in% include){
    degree_samples <- degree[rep(1:nrow(degree), fit$graph_weights),]
    centrality_graph[["degree_mean"]] <- colMeans(degree_samples)
    centrality_graph[["degree_samples"]] <- degree_samples
  }  
  if("betweenness" %in% include) {
    betweenness_samples <- betweenness[rep(1:nrow(betweenness), fit$graph_weights),]
    centrality_graph[["betweenness_mean"]] <- colMeans(betweenness_samples)
    centrality_graph[["betweenness_samples"]] <- betweenness_samples
  }  
  if  ("closeness" %in% include){
    closeness_samples <- closeness[rep(1:nrow(closeness), fit$graph_weights),]
    centrality_graph[["closeness_mean"]] <- colMeans(closeness_samples)
    centrality_graph[["closeness_samples"]] <- closeness_samples
  }
  return(centrality_graph)
}
