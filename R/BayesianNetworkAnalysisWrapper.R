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

#' Bayesian Network Analysis
#'
#' Bayesian Network Analysis estimates the structure and strength of conditional associations among variables in a graphical model.
#' The module supports the Gaussian graphical model (for continuous variables), the ordinal Markov random field, for ordinal (and binary) variables including Blume-Capel parameterizations when the ordinal variables with more than two categories have a meaningful neutral category, as well as a mixed graphical model of continuous and ordinal (Blume-Capel) variables.
#' When a grouping factor variable is supplied, one network is estimated per group. A statistical comparison of network differences is available if all selected variables are ordinal or Blume-Capel; otherwise, only group-specific networks are estimated.
#' Posterior summaries include the edge weight estimates, along with the edge inclusion probabilities and inclusion Bayes factors, that can be inspected using the different tables and plots.
#' ## Assumptions
#' - Variables are measured on a single occasion (no time dependency is modeled).
#' - Continuous variables are assumed to follow a Gaussian distribution.
#' - Ordinal variables are treated as ordered-categorical; select Blume-Capel if a neutral category is meaningful.
#' - Cases with missing values are excluded listwise.
#'
#' @param betaAlpha, First shape parameter (α1) of the Beta prior on the edge inclusion probability; under the Stochastic block model it applies to the within-cluster inclusion probability. Together with shape parameter 2 it controls the prior mean (α1 / (α1 + α2)) and concentration. Both parameters equal to 1 give a uniform prior.
#' @param betaAlpha_between, First shape parameter (α1) of the Beta prior on the between-cluster edge inclusion probability. The prior mean of that probability is α1 / (α1 + α2), so to encourage sparsity between clusters (block structure), choose shape parameter 2 larger than shape parameter 1.
#' @param betaBeta, Second shape parameter (α2) of the Beta prior on the edge inclusion probability; under the Stochastic block model it applies to the within-cluster inclusion probability. Both parameters equal to 1 give a uniform prior.
#' @param betaBeta_between, Second shape parameter (α2) of the Beta prior on the between-cluster edge inclusion probability.
#' @param betweenness, Number of shortest paths between other node pairs that pass through this node. Nodes with high betweenness act as bridges in the network.
#'    Defaults to \code{FALSE}.
#' @param burnin, Number of warmup iterations discarded from the start of each chain to allow the Markov chain to converge before collecting posterior samples.
#' @param centralityPlot, Displays centrality for the selected measures (betweenness, closeness, strength, expected influence), standardized to z-scores across nodes. Centrality is computed on the posterior mean network, or averaged over the networks sampled from the posterior when the credibility interval is requested. Measures are plotted side by side per node. Only available when no grouping variable is selected.
#'    Defaults to \code{FALSE}.
#' @param centralityTable, Shows the betweenness, closeness, strength, and expected influence of each node, standardized to z-scores across nodes. Centrality is computed on the posterior mean network, or averaged over the networks sampled from the posterior when the credibility interval of the centrality plot is requested. Only available when no grouping variable is selected.
#'    Defaults to \code{FALSE}.
#' @param chains, Number of independent MCMC chains. With 2 or more chains, the Gelman-Rubin R-hat convergence statistic is computed across all chains. With 1 chain, a split-chain version is used instead. R-hat values above approximately 1.05 suggest the chains have not converged.
#' @param closeness, Inverse of the average shortest path length from this node to all other nodes. Nodes with high closeness can efficiently reach all other nodes.
#'    Defaults to \code{FALSE}.
#' @param clusterAllocationsTable, Shows the estimated cluster membership for each node. The allocation is summarized by either the posterior mean or the posterior mode allocation.
#'    Defaults to \code{FALSE}.
#' @param clusterAllocationsType, Posterior mean is the sampled allocation that best represents the posterior co-clustering matrix (Dahl, 2009), a representative partition rather than an average of cluster indices. Posterior mode uses the most frequently sampled cluster assignment.
#' @param clusterBayesFactor, Computes a Bayes factor comparing hypotheses about the clustering structure.
#'    Defaults to \code{FALSE}.
#' @param clusterBayesFactorB1, Number of clusters under hypothesis H₁.
#' @param clusterBayesFactorB2, Number of clusters under hypothesis H₂.
#' @param clusterBayesFactorType, Clustering vs. no clustering compares the hypothesis of a multi-cluster solution against the hypothesis of a single-cluster (no clustering) model. Point hypotheses compare two specific numbers of clusters.
#' \itemize{
#'   \item \code{"complement"}: The BF tests whether any clustered structure is more probable than a single-cluster network.
#'   \item \code{"point"}: The BF compares the posterior probability of exactly H₁ clusters versus exactly H₂ clusters.
#' }
#' @param coclusteringPlot, Displays a heatmap where each cell shows the posterior proportion of the corresponding pairs of nodes belonging to the same cluster. Only available with the Stochastic block model edge prior (see Prior Specification) and no grouping variable.
#'    Defaults to \code{FALSE}.
#' @param colorGroupVariables, Assign each network variable to one of the color groups defined on the left. All variables start in Group 1 by default.
#' @param complexityPlot, Plots the summed posterior probability (y-axis) against the number of edges (x-axis). Useful for assessing which network complexities are most supported by the data.
#'    Defaults to \code{FALSE}.
#' @param credibilityInterval, Adds 95% highest density intervals (HDI) to centrality summaries.
#'    Defaults to \code{FALSE}.
#' @param cut, Edges with absolute weight below this value are drawn thin and desaturated; above it they scale to full width and saturation. Set to 0 to scale all edges continuously without a cut threshold.
#' @param details, Overlays the minimum edge strength, maximum edge strength, and cut values as text on the network plot.
#'    Defaults to \code{FALSE}.
#' @param differencePriorFamily, Family of the prior on the differences in partial association between the groups. Normal is the default and bounds a difference more tightly; Cauchy is heavy-tailed and leaves more room for large differences. Only used when a difference network is estimated, which requires a grouping variable and all selected variables to be ordinal or Blume-Capel.
#' @param dirichletAlpha, Symmetric Dirichlet concentration parameter for cluster membership probabilities. The default of 1 is roughly uninformative about cluster sizes. Values less than 1 favor unequal cluster sizes.
#' @param edgeAbsence, Show edges where BF₁₀ falls between the two thresholds, colored gray.
#'    Defaults to \code{TRUE}.
#' @param edgeEvidenceTable, Displays a symmetric matrix of edge-wise evidence values. All edges are listed regardless of evidence strength.
#'    Defaults to \code{FALSE}.
#' @param edgeExclusion, Show edges with BF₁₀ ≤ 1/threshold, colored yellow.
#'    Defaults to \code{TRUE}.
#' @param edgeInclusion, Show edges with BF₁₀ ≥ threshold, colored blue.
#'    Defaults to \code{TRUE}.
#' @param edgeInclusionCriteria, BF threshold for categorizing evidence. An edge is evidence for inclusion if BF₁₀ ≥ threshold, evidence for exclusion if BF₁₀ ≤ 1/threshold, and absence of evidence otherwise.
#' @param edgeLabelPosition, Position of edge labels along each edge (0 to 1).
#' @param edgeLabelSize, Multiplier for edge label size.
#' @param edgeLabels, Overlay the numerical edge weight on each edge in the network plot.
#'    Defaults to \code{FALSE}.
#' @param edgePalette, Color scheme applied to positive and negative edges. Classic uses blue for positive and red for negative associations. Colorblind-safe and grayscale options are also available.
#' @param edgePrior, Bernoulli: each edge is independently included with a fixed prior probability. Beta-binomial: edges share a random inclusion probability drawn from a Beta distribution, allowing borrowing of information. Stochastic block: inclusion probabilities depend on latent cluster membership, with separate within- and between-cluster parameters. The Stochastic block option is only available without a grouping variable.
#' @param edgeSize, Multiplier applied to all edge widths. A value of 2 doubles the edge width relative to the default.
#' @param edgeSpecificOverviewInclusionCriteria, BF threshold used to categorize the edges in the overview table: included if BF₁₀ ≥ threshold, excluded if BF₁₀ ≤ 1/threshold, and inconclusive otherwise. All edges remain listed. When comparing groups the categories are labelled difference, equal, and inconclusive. The same threshold drives the edge counts in the summary table.
#' @param edgeSpecificOverviewTable, Shows one row per edge, listed by variable pair. Columns include the posterior estimate, inclusion probability, inclusion Bayes factor, an evidence category, and the R-hat convergence statistic. When Blume-Capel variables are selected, an additional table reports the posterior mean, standard deviation, credible interval and R-hat of their linear and quadratic main effects.
#'    Defaults to \code{FALSE}.
#' @param evidencePlot, Displays the network with edges colored by the evidence category they belong to. Blue edges have BF₁₀ at or above the threshold (evidence for inclusion); yellow edges have BF₁₀ at or below the reciprocal threshold (evidence for exclusion); gray edges fall in between (absence of evidence).
#'    Defaults to \code{FALSE}.
#' @param evidenceType, Choose which evidence quantity to display for each edge.
#' \itemize{
#'   \item \code{"inclusionProbability"}: Posterior probability that an edge is present, ranging from 0 (no evidence for the edge) to 1 (certain inclusion).
#'   \item \code{"BF10"}: Bayes factor quantifying evidence for edge inclusion relative to exclusion. BF₁₀ > 1 favors inclusion; BF₁₀ < 1 favors exclusion.
#'   \item \code{"BF01"}: Reciprocal Bayes factor quantifying evidence for edge exclusion relative to inclusion. BF₀₁ = 1 / BF₁₀.
#'   \item \code{"log(BF)"}: Natural logarithm of BF₁₀.
#' }
#' @param expectedInfluence, Sum of signed edge weights connected to this node. Unlike strength, negative edges reduce the value, so nodes with mixed positive and negative connections may have low expected influence.
#'    Defaults to \code{FALSE}.
#' @param gPrior, Prior probability that any given edge is present under the Bernoulli prior. The default of 0.5 assigns equal prior probability to inclusion and exclusion. Values closer to 0 impose a sparser prior. The probability must lie strictly between 0 and 1.
#' @param groupingVariable, Select a nominal variable to estimate one network per group. A difference network is estimated only when all selected variables are ordinal or Blume-Capel; if scale variables are included, only separate group networks are estimated. At least 2 groups and 3 observations per group are required.
#' @param interactionAlpha, First shape parameter (α1) of the Beta-prime prior on the partial association parameters.
#' @param interactionBeta, Second shape parameter (α2) of the Beta-prime prior on the partial association parameters.
#' @param interactionPriorFamily, Family of the prior on the partial association parameters. Normal is the default and applies lighter-tailed shrinkage. Cauchy is a heavy-tailed shrinkage prior. Beta-prime is parameterized via two shape parameters on the logistic scale and is only available when all selected variables are ordinal or Blume-Capel. When a grouping variable is selected this family applies to the baseline partial associations; the prior on the group differences is set separately below.
#' @param interactionScale, Scale parameter of the Normal or Cauchy prior on the partial association parameters. When a grouping variable is selected, this is instead the scale of the prior on the differences between groups, which is only used when a difference network is estimated (all selected variables ordinal or Blume-Capel).
#' @param interactionScaleBaseline, Scale of the Normal or Cauchy prior on the partial association parameters when a grouping variable is selected. It applies to the separate group and pooled networks, and to the baseline partial associations of the difference network. The scale of the prior on the group differences is set separately below.
#' @param iter, Number of posterior samples retained per chain, drawn after the burn-in iterations. The total length of each chain is the burn-in plus this number. Increase for more stable estimates, especially for complex models.
#' @param labelAbbreviation, Abbreviate variable names in plots to reduce label clutter.
#'    Defaults to \code{FALSE}.
#' @param labelAbbreviationLength, Target character length for abbreviated labels.
#' @param labelScale, When enabled, label font size is automatically scaled relative to node size so that labels fit inside nodes.
#'    Defaults to \code{TRUE}.
#' @param labelSize, Multiplier for node label font size. A value of 2 doubles the label size relative to the default.
#' @param lambda, Rate parameter of the truncated Poisson prior on the number of clusters. Smaller values favor fewer clusters.
#' @param layout, Determines how nodes are positioned in network plots. The same layout is applied to all networks in a multi-network analysis, computed from the average of estimated edge weights.
#' \itemize{
#'   \item \code{"spring"}: Nodes are positioned using the Fruchterman-Reingold force-directed algorithm. Strongly connected nodes are placed closer together.
#'   \item \code{"circle"}: Nodes are evenly spaced on a circle. Useful for comparing relative edge patterns without the layout reflecting association strength.
#' }
#' @param layoutSpringRepulsion, Controls how strongly nodes repel each other. Larger values spread nodes further apart.
#' @param legend, Controls legend placement across network plots. When multiple networks are shown (e.g., with grouping), this setting applies globally.
#' \itemize{
#'   \item \code{"hide"}: No legend is shown in any plot.
#'   \item \code{"allPlots"}: A legend is added to every network plot.
#'   \item \code{"specificPlot"}: A legend is added to only the plot with the specified number, and other plots are shown without a legend.
#' }
#' @param legendSpecificPlotNumber, 1-based index of the plot in which the legend should appear.
#' @param legendToPlotRatio, Width of the legend panel relative to the network plot. A value of 0.4 means the legend is 40% as wide as the plot.
#' @param manualColor, When enabled, node colors are taken from the group color assignments above. When disabled, a predefined palette is applied uniformly.
#'    Defaults to \code{FALSE}.
#' @param manualColorGroups, Define named groups for manual node coloring. Each group can be assigned a color that will appear in the network plot when Manual colors is enabled.
#' @param maxEdgeStrength, Sets the edge weight that corresponds to the maximum line width. Edges stronger than this value are clipped to the maximum width. Set to 0 to scale all edges relative to the strongest edge.
#' @param minEdgeStrength, Edges with absolute weight below this value are hidden in the network plot. If the threshold exceeds the strongest edge, it is ignored and a warning is shown in the summary table.
#' @param networkPlot, Displays the posterior mean partial association network. Edge width and color saturation reflect association strength; blue edges are positive and red edges are negative. Edges whose BF₁₀ falls below the inclusion threshold are hidden. Only available when no grouping variable is selected.
#'    Defaults to \code{FALSE}.
#' @param networkPlotInclusionCriteria, Edges with BF₁₀ below this value are set to zero in the network plot.
#' @param nodePalette, Color palette applied to node groups when Manual colors is disabled. Colorblind-safe options are recommended for accessible figures.
#' @param nodeSize, Multiplier applied to all node sizes. A value of 1 uses the default size; values above 1 create larger nodes.
#' @param omrfUpdateMethod, MCMC algorithm used to sample from the posterior. NUTS is the gradient-based No-U-Turn sampler and is available for every variable type. Adaptive Metropolis is a componentwise random-walk sampler. Gibbs draws the precision matrix in conjugate row blocks and is only available for the Gaussian graphical model, that is, when all variables are continuous. With a grouping variable it can still be used for the separate group networks, but not for the estimation of a difference network.
#' @param parameterHdiPlot, Displays the posterior mean and highest density interval (HDI) for every pairwise partial association, ordered from smallest to largest posterior mean. Only available when no grouping variable is selected.
#'    Defaults to \code{FALSE}.
#' @param parameterHdiPlotCoverage, Coverage of the highest density interval (e.g., 0.95 for a 95% HDI).
#' @param parameterHdiTable, Shows the posterior mean and highest density interval (HDI) for every pairwise partial association, ordered from largest to smallest posterior mean so that it reads like the parameter HDI plot. The posterior mean averages over all posterior samples, so an edge that is mostly excluded can have a nonzero mean while its HDI collapses onto zero. The estimate of the median probability model, which sets edges with a posterior inclusion probability of at most 0.5 to zero, is shown in a separate column. Only available when no grouping variable is selected.
#'    Defaults to \code{FALSE}.
#' @param parameterHdiTableCoverage, Coverage of the highest density interval (e.g., 0.95 for a 95% HDI).
#' @param posteriorCoclusteringMatrixTable, Shows the posterior probability that each pair of nodes belongs to the same cluster.
#'    Defaults to \code{FALSE}.
#' @param posteriorNumBlocksTable, Shows the posterior probability for every possible number of clusters.
#'    Defaults to \code{FALSE}.
#' @param posteriorStructurePlot, Plots the posterior probability of each sampled graph structure (y-axis) against a structure index (x-axis), sorted in decreasing order of probability. Useful for assessing whether a single graph structure dominates the posterior.
#'    Defaults to \code{FALSE}.
#' @param showInterpretativeScaleEstimates, Shows log-odds and partial correlations when available.
#'    Defaults to \code{FALSE}.
#' @param strength, Sum of absolute edge weights connected to this node. Reflects how strongly a node is associated with its neighbors.
#'    Defaults to \code{TRUE}.
#' @param thresholdAlpha, First shape parameter (α1) of the Beta-prime prior on the main effects.
#' @param thresholdBeta, Second shape parameter (α2) of the Beta-prime prior on the main effects.
#' @param thresholdPriorFamily, Family of the prior on the main effects (thresholds). Beta-prime is the default; Cauchy and Normal are alternative parameterizations on the original scale.
#' @param thresholdScale, Scale parameter of the Cauchy or Normal prior on the main effects.
#' @param variableNamesShown, Choose where variable names are displayed in network plots.
#' \itemize{
#'   \item \code{"inNodes"}: Variable names are shown as labels directly on the nodes.
#'   \item \code{"inLegend"}: Nodes are labeled with numbers; a legend maps each number to the variable name. Useful when variable names are long.
#' }
#' @param variables, Select variables to include as nodes in the network. Continuous (scale) and ordinal variables are accepted. When a grouping variable is present, a difference network is estimated only if all selected variables are ordinal or Blume-Capel; otherwise separate group networks are estimated.
BayesianNetworkAnalysis <- function(
          data = NULL,
          version = "1",
          betaAlpha = 1,
          betaAlpha_between = 1,
          betaBeta = 1,
          betaBeta_between = 1,
          betweenness = FALSE,
          burnin = 2000,
          centralityPlot = FALSE,
          centralityTable = FALSE,
          chains = "4",
          closeness = FALSE,
          clusterAllocationsTable = FALSE,
          clusterAllocationsType = "mean",
          clusterBayesFactor = FALSE,
          clusterBayesFactorB1 = 1,
          clusterBayesFactorB2 = 1,
          clusterBayesFactorType = "complement",
          coclusteringPlot = FALSE,
          colorGroupVariables = list(optionKey = "variable", types = list(), value = list()),
          complexityPlot = FALSE,
          credibilityInterval = FALSE,
          cut = 0,
          details = FALSE,
          differencePriorFamily = "normal",
          dirichletAlpha = 1,
          edgeAbsence = TRUE,
          edgeEvidenceTable = FALSE,
          edgeExclusion = TRUE,
          edgeInclusion = TRUE,
          edgeInclusionCriteria = 10,
          edgeLabelPosition = 0.5,
          edgeLabelSize = 1,
          edgeLabels = FALSE,
          edgePalette = "colorblind",
          edgePrior = "Bernoulli",
          edgeSize = 1,
          edgeSpecificOverviewInclusionCriteria = 10,
          edgeSpecificOverviewTable = FALSE,
          evidencePlot = FALSE,
          evidenceType = "inclusionProbability",
          expectedInfluence = FALSE,
          gPrior = 0.5,
          groupingVariable = list(types = list(), value = ""),
          interactionAlpha = 0.5,
          interactionBeta = 0.5,
          interactionPriorFamily = "normal",
          interactionScale = 1,
          interactionScaleBaseline = 1,
          iter = 2000,
          labelAbbreviation = FALSE,
          labelAbbreviationLength = 4,
          labelScale = TRUE,
          labelSize = 1,
          lambda = 1,
          layout = "spring",
          layoutSpringRepulsion = 1,
          legend = "allPlots",
          legendSpecificPlotNumber = 1,
          legendToPlotRatio = 0.4,
          manualColor = FALSE,
          manualColorGroups = list(list(name = "Group 1"), list(name = "Group 2")),
          maxEdgeStrength = 0,
          minEdgeStrength = 0,
          networkPlot = FALSE,
          networkPlotInclusionCriteria = 10,
          nodePalette = "colorblind",
          nodeSize = 1,
          omrfUpdateMethod = "nuts",
          parameterHdiPlot = FALSE,
          parameterHdiPlotCoverage = 0.95,
          parameterHdiTable = FALSE,
          parameterHdiTableCoverage = 0.95,
          plotHeight = 320,
          plotWidth = 480,
          posteriorCoclusteringMatrixTable = FALSE,
          posteriorNumBlocksTable = FALSE,
          posteriorStructurePlot = FALSE,
          seed = 1,
          setSeed = FALSE,
          showInterpretativeScaleEstimates = FALSE,
          strength = TRUE,
          thresholdAlpha = 0.5,
          thresholdBeta = 0.5,
          thresholdPriorFamily = "beta-prime",
          thresholdScale = 1,
          variableNamesShown = "inNodes",
          variables = list(optionKey = "variable", types = list(), value = list())) {

   defaultArgCalls <- formals(jaspNetwork::BayesianNetworkAnalysis)
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

   optionsWithFormula <- c("chains", "clusterAllocationsType", "colorGroupVariables", "differencePriorFamily", "edgePalette", "edgePrior", "groupingVariable", "interactionPriorFamily", "manualColorGroups", "nodePalette", "omrfUpdateMethod", "thresholdPriorFamily", "variables")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspNetwork", "BayesianNetworkAnalysis", "BayesianNetworkAnalysis.qml", options, version, TRUE))
}