#
# Copyright (C) 2013-2025 University of Amsterdam
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

# This is a generated file. Don't change it!

#' Network Analysis
#'
NetworkAnalysis <- function(
          data = NULL,
          version = "0.97.1",
          betweenness = TRUE,
          bootstrap = FALSE,
          bootstrapParallel = FALSE,
          bootstrapSamples = 0,
          bootstrapType = "nonparametric",
          centralityNormalization = "normalized",
          centralityPlot = FALSE,
          centralityTable = FALSE,
          closeness = TRUE,
          clusteringPlot = FALSE,
          clusteringTable = FALSE,
          colorGroupVariables = list(optionKey = "variable", types = list(), value = list()),
          computedLayoutX = "",
          computedLayoutY = "",
          correlationMethod = "auto",
          criterion = "ebic",
          cut = 0,
          details = FALSE,
          edgeLabelPosition = 0.5,
          edgeLabelSize = 1,
          edgeLabels = FALSE,
          edgePalette = "colorblind",
          edgeSize = 1,
          estimator = "ebicGlasso",
          expectedInfluence = TRUE,
          groupingVariable = list(types = list(), value = ""),
          isingEstimator = "pseudoLikelihood",
          labelAbbreviation = FALSE,
          labelAbbreviationLength = 4,
          labelScale = TRUE,
          labelSize = 1,
          layout = "spring",
          layoutNotUpdated = FALSE,
          layoutSavedToData = FALSE,
          layoutSpringRepulsion = 1,
          layoutX = list(types = list(), value = ""),
          layoutY = list(types = list(), value = ""),
          legend = "allPlots",
          legendSpecificPlotNumber = 1,
          legendToPlotRatio = 0.4,
          manualColor = FALSE,
          manualColorGroups = list(list(name = "Group 1"), list(name = "Group 2")),
          maxEdgeStrength = 0,
          mgmCategoricalVariables = list(types = list(), value = list()),
          mgmContinuousVariables = list(types = list(), value = list()),
          mgmCountVariables = list(types = list(), value = list()),
          mgmVariableTypeShown = "nodeShape",
          minEdgeStrength = 0,
          missingValues = "pairwise",
          nFolds = 3,
          networkPlot = FALSE,
          nodePalette = "colorblind",
          nodeSize = 1,
          plotHeight = 320,
          plotWidth = 480,
          rule = "and",
          sampleSize = "maximum",
          signedNetwork = TRUE,
          split = "median",
          statisticsCentrality = TRUE,
          statisticsEdges = TRUE,
          strength = TRUE,
          thresholdBox = "value",
          thresholdMethod = "sig",
          thresholdValue = 0,
          tuningParameter = 0.5,
          variableNamesShown = "inNodes",
          variables = list(types = list(), value = list()),
          weightedNetwork = TRUE) {

   defaultArgCalls <- formals(jaspNetwork::NetworkAnalysis)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("colorGroupVariables", "edgePalette", "estimator", "groupingVariable", "layoutX", "layoutY", "manualColorGroups", "mgmCategoricalVariables", "mgmContinuousVariables", "mgmCountVariables", "nodePalette", "thresholdMethod", "variables")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspNetwork", "NetworkAnalysis", "NetworkAnalysis.qml", options, version, TRUE))
}