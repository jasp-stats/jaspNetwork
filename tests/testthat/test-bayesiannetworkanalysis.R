context("Bayesian Network Analysis")

# jaspTools cannot read a default from a DropDown whose values are conditional, so
# omrfUpdateMethod (like edgePrior) arrives as NA and every analysis test sets it.

# The QML variables list carries a row component, so JASP sends one row per assigned
# variable. jaspTools does not build those rows from a plain character vector, so the
# tests assemble them here; it picks up the column type from a variable.types entry
# next to the name within the row, which the analysis itself ignores.
setNetworkVariables <- function(options, types, blumeCapel = character(0)) {

  options$variables <- lapply(names(types), function(variable) {
    isBlumeCapel <- variable %in% names(blumeCapel)
    list(
      variable       = variable,
      variable.types = unname(types[[variable]]),
      blumeCapel     = isBlumeCapel,
      levels         = if (isBlumeCapel) unname(blumeCapel[[variable]]) else ""
    )
  })

  options
}

testthat::test_that("Variable rows are split into names and Blume-Capel entries", {
  options <- list(variables = list(
    list(variable = "a", blumeCapel = FALSE, levels = ""),
    list(variable = "b", blumeCapel = TRUE,  levels = "2")
  ))

  normalized <- jaspNetwork:::.bayesianNetworkAnalysisNormalizeVariableOptions(options)

  testthat::expect_identical(normalized$variables, c("a", "b"))
  testthat::expect_identical(normalized$variablesBlumeCapel, list(list(variable = "b", levels = "2")))

  empty <- jaspNetwork:::.bayesianNetworkAnalysisNormalizeVariableOptions(list(variables = list()))
  testthat::expect_identical(empty$variables, character(0))
  testthat::expect_identical(empty$variablesBlumeCapel, list())
})

testthat::test_that("Variable type specification supports Blume-Capel baselines", {
  dataset <- data.frame(
    ordinalOne = factor(c("low", "mid", "high"), ordered = TRUE),
    ordinalTwo = factor(c("A", "B", "C"), ordered = TRUE),
    continuous = c(1, 2, 3)
  )

  options <- list(
    variables           = c("ordinalOne", "ordinalTwo", "continuous"),
    variablesBlumeCapel = list(list(variable = "ordinalTwo", levels = "B"))
  )

  variableSpec <- jaspNetwork:::.bayesianNetworkAnalysisBuildVariableTypeSpec(options, dataset)

  testthat::expect_equal(variableSpec[["type"]], c("ordinal", "blume-capel", "continuous"))
  testthat::expect_equal(variableSpec[["baselineCategory"]], c(1L, 2L, 1L))
})

testthat::test_that("Blume-Capel baselines resolve by level label, not by numeric value", {
  # bgms codes the factor as 1..k and expects the baseline on that scale, so the
  # label "4" of a variable with levels "2", "4", "6" is category 2. Passing 4
  # makes bgms reject the baseline as outside the observed category scores.
  dataset <- data.frame(
    spacedLevels = factor(c(2, 4, 6), levels = c(2, 4, 6), ordered = TRUE),
    plainLevels  = factor(c(1, 2, 3), levels = c(1, 2, 3), ordered = TRUE)
  )

  options <- list(
    variables           = c("spacedLevels", "plainLevels"),
    variablesBlumeCapel = list(
      list(variable = "spacedLevels", levels = "4"),
      list(variable = "plainLevels",  levels = "2")
    )
  )

  variableSpec <- jaspNetwork:::.bayesianNetworkAnalysisBuildVariableTypeSpec(options, dataset)

  testthat::expect_equal(variableSpec[["baselineCategory"]], c(2L, 2L))

  # An out-of-range baseline is reported against the variable instead of reaching bgms
  bad <- list(
    variables           = "spacedLevels",
    variablesBlumeCapel = list(list(variable = "spacedLevels", levels = "99"))
  )
  testthat::expect_error(
    jaspNetwork:::.bayesianNetworkAnalysisBuildVariableTypeSpec(bad, dataset)
  )
})

testthat::test_that("Compare mode is enabled for ordinal and Blume-Capel variables", {
  options <- list(groupingVariable = "group")
  variableSpec <- list(type = c("ordinal", "blume-capel"))

  supported <- jaspNetwork:::.bayesianNetworkAnalysisCompareSupported(
    options      = options,
    variableSpec = variableSpec,
    nGroups      = 3L
  )

  testthat::expect_true(supported)
})

testthat::test_that("The prior family on the group differences maps to what bgmCompare expects", {
  buildFamily <- jaspNetwork:::.bayesianNetworkAnalysisBuildDifferenceFamily

  testthat::expect_identical(buildFamily(list(differencePriorFamily = "normal")), "Normal")
  testthat::expect_identical(buildFamily(list(differencePriorFamily = "cauchy")), "Cauchy")

  # Missing or metadata-carrying option values fall back to the bgms default
  testthat::expect_identical(buildFamily(list()), "Normal")
  testthat::expect_identical(buildFamily(list(differencePriorFamily = "Cauchy, QtObject")), "Cauchy")
})

testthat::test_that("Sampled difference structures are reduced to their pairwise part", {
  # bgmCompare interleaves one main effect indicator per variable with the
  # pairwise ones: V1 (main), V1-V2, V1-V3, V1-V4, V2 (main), V2-V3, ...
  positions <- jaspNetwork:::.bayesianNetworkAnalysisPairwiseIndicatorPositions(4L)
  testthat::expect_identical(positions, c(2L, 3L, 4L, 6L, 7L, 9L))

  reduce <- jaspNetwork:::.bayesianNetworkAnalysisReducePairwiseIndicators

  # Width nEdges + nVar: the main effect indicators (positions 1, 5, 8, 10) are dropped
  reduced <- reduce(c("1101100111", "1000100101"), c(3L, 7L), nVar = 4L)
  testthat::expect_identical(reduced[["sampleGraphs"]], c("000000", "101001"))
  testthat::expect_identical(reduced[["graphWeights"]], c(7L, 3L))

  # Dropping indicators can merge structures, so the weights are re-aggregated
  merged <- reduce(c("1101100111", "0101000010"), c(3L, 7L), nVar = 4L)
  testthat::expect_identical(merged[["sampleGraphs"]], "101001")
  testthat::expect_identical(merged[["graphWeights"]], 10L)

  # A fit of a single network is already at the edge width and is left alone
  untouched <- reduce(c("101010", "111000"), c(3L, 7L), nVar = 4L)
  testthat::expect_identical(untouched[["sampleGraphs"]], c("101010", "111000"))
  testthat::expect_identical(untouched[["graphWeights"]], c(3L, 7L))

  testthat::expect_null(reduce(NULL, NULL, nVar = 4L)[["sampleGraphs"]])
})

testthat::test_that("Edge convergence is read in the row-major order bgms reports", {
  # Column-major reading attaches V2-V3 to the V1-V4 statistic from four
  # variables onwards, so the order is asserted rather than assumed.
  nVar        <- 4L
  upperTriIdx <- which(upper.tri(matrix(0, nVar, nVar)), arr.ind = TRUE)
  upperTriIdx <- upperTriIdx[order(upperTriIdx[, 1], upperTriIdx[, 2]), ]

  reportedRhat <- c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6)
  nw <- list(estimates = matrix(0, nVar, nVar), convergence = reportedRhat)

  convergence <- jaspNetwork:::.bayesianNetworkAnalysisComputeEdgeConvergence(nw, upperTriIdx, 6L, 2L)
  testthat::expect_equal(convergence, reportedRhat)

  # Without a reported R-hat the samples are read in the same order
  samples <- matrix(rep(reportedRhat, each = 40L), nrow = 40L)
  nwSamples <- list(estimates = matrix(0, nVar, nVar), samplesPosterior = samples)
  testthat::expect_length(
    jaspNetwork:::.bayesianNetworkAnalysisComputeEdgeConvergence(nwSamples, upperTriIdx, 6L, 2L), 6L
  )

  # Nothing to report at all
  testthat::expect_null(
    jaspNetwork:::.bayesianNetworkAnalysisComputeEdgeConvergence(
      list(estimates = matrix(0, nVar, nVar)), upperTriIdx, 6L, 2L
    )
  )
})

testthat::test_that("Compare mode is disabled when continuous variables are included", {
  options <- list(groupingVariable = "group")
  variableSpec <- list(type = c("ordinal", "continuous"))

  supported <- jaspNetwork:::.bayesianNetworkAnalysisCompareSupported(
    options      = options,
    variableSpec = variableSpec,
    nGroups      = 2L
  )

  testthat::expect_false(supported)
})

# does not test
# - error handling
# - bootstrapping
# - plots or graphical options


# based on https://github.com/jasp-stats/jasp-test-release/issues/2300
testthat::test_that("Analysis handles too many missing values errors", {
  options <- jaspTools::analysisOptions("BayesianNetworkAnalysis")
  options$estimator <- "gcgm"
  options$omrfUpdateMethod <- "nuts"
  options <- setNetworkVariables(options, c(contNormal = "scale", contGamma = "scale", debMiss99 = "scale"))
  results <- jaspTools::runAnalysis("BayesianNetworkAnalysis", "test.csv", options)

  errorMessage <- results[["results"]][["errorMessage"]]
  testthat::expect_true(results[["results"]][["error"]])
  testthat::expect_identical(results[["status"]], "validationError")
  testthat::expect_true(is.character(errorMessage) && length(errorMessage) == 1L && nzchar(errorMessage))
})

testthat::test_that("Analysis handles too many missing values errors with grouping variable", {
  options <- jaspTools::analysisOptions("BayesianNetworkAnalysis")
  options$estimator <- "gcgm"
  options$omrfUpdateMethod <- "nuts"
  options <- setNetworkVariables(options, c(contNormal = "scale", contGamma = "scale", debMiss80 = "scale"))
  options$groupingVariable <- "facFifty"
  options$groupingVariable.types <- "nominal"
  options$dfprior <- 3
  options$gprior  <- "0.5"
  results <- jaspTools::runAnalysis("BayesianNetworkAnalysis", "test.csv", options)

  errorMessage <- results[["results"]][["errorMessage"]]
  testthat::expect_true(results[["results"]][["error"]])
  testthat::expect_identical(results[["status"]], "validationError")
  testthat::expect_true(is.character(errorMessage) && length(errorMessage) == 1L && nzchar(errorMessage))

})

# based on https://github.com/jasp-stats/jasp-test-release/issues/2298
testthat::test_that("Centrality keeps every measure for an empty network", {
  # qgraph drops Strength for an all-zero network, which broke the centrality table
  variables <- c("A", "B", "C")
  network <- list(
    estimates        = matrix(0, 3L, 3L, dimnames = list(variables, variables)),
    samplesPosterior = matrix(0, nrow = 20L, ncol = 3L)
  )

  summary <- jaspNetwork:::.bayesianNetworkAnalysisComputeCentrality(list(network), list(credibilityInterval = TRUE))[[1L]]

  testthat::expect_setequal(unique(summary$measure), c("Betweenness", "Closeness", "Strength", "ExpectedInfluence"))
  testthat::expect_equal(summary$posteriorMeans[summary$measure == "Strength"], rep(0, 3L))
  testthat::expect_true(all(c("lower", "upper") %in% colnames(summary)))

  pointEstimate <- jaspNetwork:::centrality(network, options = list(credibilityInterval = FALSE))
  wide <- stats::reshape(pointEstimate, idvar = "node", timevar = "measure", direction = "wide")
  testthat::expect_true("posteriorMeans.Strength" %in% names(wide))
})

testthat::test_that("Centrality summaries use the posterior samples only", {
  # The wide centrality output holds node, measure and the posterior mean network
  # before the samples; the summary must not include that point estimate.
  variables <- c("A", "B", "C")
  estimates <- matrix(0.9, 3L, 3L, dimnames = list(variables, variables))
  diag(estimates) <- 0
  draws <- rbind(c(0.1, 0.5, 0.3), c(0.4, 0.2, 0.6))
  network <- list(estimates = estimates, samplesPosterior = draws)

  summary <- jaspNetwork:::.bayesianNetworkAnalysisComputeCentrality(list(network), list(credibilityInterval = TRUE))[[1L]]

  measures <- c("Closeness", "Betweenness", "Strength", "ExpectedInfluence")
  perDraw  <- sapply(1:2, function(i) jaspNetwork:::.bayesianNetworkAnalysisCentralityGrid(
    jaspNetwork:::vectorToMatrix(draws[i, ], 3), measures, variables)$value)
  testthat::expect_equal(summary$posteriorMeans, rowMeans(perDraw))
})

testthat::test_that("Parameter HDI plot works", {
  options <- jaspTools::analysisOptions("BayesianNetworkAnalysis")
  options <- setNetworkVariables(options, c(contNormal = "scale", contcor1 = "scale", contcor2 = "scale"))
  options$burnin <- 100
  options$iter   <- 500
  options$chains <- "1"
  options$omrfUpdateMethod <- "adaptive-metropolis"
  options$parameterHdiPlot <- TRUE
  options$parameterHdiPlotCoverage <- 0.95
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianNetworkAnalysis", "test.csv", options)

  hdiCollection <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_parameterHdiPlotContainer"]][["collection"]]
  testthat::expect_true(length(hdiCollection) >= 1L)

  firstPlot <- hdiCollection[[1L]]
  plotName <- firstPlot[["data"]]
  testthat::expect_true(is.character(plotName) && length(plotName) == 1L && nzchar(plotName))

  # bgms is seeded through options$seed, but the draws still differ across platforms and bgms
  # builds, so a plot snapshot is not portable
  testthat::skip("Not reproducible")

  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "parameter-hdi-plot")
})

testthat::test_that("Default interaction prior is Normal and thresholds Beta-prime", {
  interaction <- jaspNetwork:::.bayesianNetworkAnalysisBuildParameterPrior(
    family = NULL, scale = 1, alpha = 0.5, beta = 0.5, priorRole = "interaction"
  )
  threshold <- jaspNetwork:::.bayesianNetworkAnalysisBuildParameterPrior(
    family = NULL, scale = 1, alpha = 0.5, beta = 0.5, priorRole = "threshold"
  )

  testthat::expect_equal(interaction, bgms::normal_prior(scale = 1))
  testthat::expect_equal(threshold, bgms::beta_prime_prior(alpha = 0.5, beta = 0.5))
})

testthat::test_that("Parameter HDI relations match the edge specific overview convention", {
  variables <- c("alpha", "beta", "gamma")
  estimates <- matrix(0, 3L, 3L, dimnames = list(variables, variables))

  # Three edges, in the row-major upper-triangle order bgms uses for its samples
  samples <- cbind(rep(0.10, 20L), rep(0.20, 20L), rep(0.30, 20L))

  network <- list(estimates = estimates, samplesPosterior = samples)
  options <- list(labelAbbreviation = FALSE)

  posterior <- jaspNetwork:::.bayesianNetworkAnalysisComputeParameterHdi(network, options, 0.95)

  # Column-then-row, identical to .bayesianNetworkAnalysisFillEdgeOverviewTable
  testthat::expect_equal(sort(posterior$edge), sort(c("beta-alpha", "gamma-alpha", "gamma-beta")))

  # Ordered ascending by posterior mean
  testthat::expect_equal(posterior$mean, sort(posterior$mean))

  # Named samples in the expected order are accepted
  colnames(samples) <- c("alpha-beta", "alpha-gamma", "beta-gamma")
  network$samplesPosterior <- samples
  testthat::expect_silent(
    jaspNetwork:::.bayesianNetworkAnalysisComputeParameterHdi(network, options, 0.95)
  )

  # A reordering upstream must fail loudly rather than mislabel the intervals
  colnames(network$samplesPosterior) <- c("beta-gamma", "alpha-beta", "alpha-gamma")
  testthat::expect_error(
    jaspNetwork:::.bayesianNetworkAnalysisComputeParameterHdi(network, options, 0.95)
  )
})

testthat::test_that("Blume-Capel main effects are extracted into a table", {
  options <- jaspTools::analysisOptions("BayesianNetworkAnalysis")
  options <- setNetworkVariables(options,
                                 c(facFive = "ordinal", contBinom = "ordinal", facGender = "ordinal"),
                                 blumeCapel = c(facFive = "2"))
  options$omrfUpdateMethod <- "nuts"
  options$burnin <- 100
  options$iter   <- 200
  options$chains <- "1"
  options$edgeSpecificOverviewTable <- TRUE

  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianNetworkAnalysis", "test.csv", options)

  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_blumeCapelTable"]]
  testthat::expect_false(is.null(table))

  testthat::expect_equal(
    sapply(table[["schema"]][["fields"]], `[[`, "name"),
    c("variable", "effect", "baseline", "estimate", "sd", "lower", "upper", "convergence")
  )

  rows <- do.call(rbind, lapply(table[["data"]], as.data.frame))
  testthat::expect_equal(rows$variable, c("facFive", "facFive"))
  testthat::expect_equal(rows$effect,   c("Linear", "Quadratic"))
  testthat::expect_equal(rows$baseline, c(2L, 2L))

  # bgms is seeded through options$seed, but estimates differ across platforms and bgms builds,
  # so only structure is checked
  testthat::expect_true(all(is.finite(rows$estimate)))
  testthat::expect_true(all(rows$lower <= rows$upper))
})

testthat::test_that("Parameter HDI table reports one row per edge", {
  options <- jaspTools::analysisOptions("BayesianNetworkAnalysis")
  options <- setNetworkVariables(options, c(contNormal = "scale", contcor1 = "scale", contcor2 = "scale"))
  options$omrfUpdateMethod <- "nuts"
  options$burnin <- 100
  options$iter   <- 200
  options$chains <- "1"
  options$parameterHdiTable         <- TRUE
  options$parameterHdiTableCoverage <- 0.95

  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianNetworkAnalysis", "test.csv", options)

  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_parameterHdiTable"]]
  testthat::expect_false(is.null(table))

  testthat::expect_equal(
    sapply(table[["schema"]][["fields"]], `[[`, "name"),
    c("relation", "mean", "lower", "upper", "medianProbabilityEstimate")
  )

  rows <- do.call(rbind, lapply(table[["data"]], as.data.frame))
  testthat::expect_equal(nrow(rows), 3L)

  # Ordered largest-first so the table reads top-to-bottom like the (flipped) HDI plot.
  testthat::expect_equal(rows$mean, sort(rows$mean, decreasing = TRUE))

  testthat::expect_true(all(is.finite(rows$mean)))

  testthat::expect_true(all(rows$lower <= rows$upper))
  testthat::expect_true(all(is.finite(rows$lower) & is.finite(rows$upper)))
})

testthat::test_that("Pairwise quantities are put in row-major order by name", {
  canonicalOrder <- jaspNetwork:::.bayesianNetworkAnalysisCanonicalEdgeOrder
  variables <- c("A", "B", "C", "D")

  # bgms groups the pairs of mixed models by variable type and may reverse a pair
  testthat::expect_identical(canonicalOrder(c("A-C", "B-D", "A-B", "A-D", "C-B", "C-D"), variables),
                             c(3L, 1L, 4L, 5L, 2L, 6L))
  testthat::expect_null(canonicalOrder(c("A-C", "B-D", "A-B", "A-D", "C-B", "X-Y"), variables))
  testthat::expect_null(canonicalOrder(c("A-B", "A-B", "A-D", "B-C", "B-D", "C-D"), variables))
})

testthat::test_that("Extraction reorders samples and R-hat of a mixed network", {
  variables  <- c("A", "B", "C", "D")
  edgeMatrix <- matrix(0, 4L, 4L, dimnames = list(variables, variables))
  bgmsOrder  <- c("A-C", "B-D", "A-B", "A-D", "C-B", "C-D")

  fit <- list(
    inc_probs             = edgeMatrix,
    inc_BF                = edgeMatrix,
    structure             = edgeMatrix,
    parameters            = edgeMatrix,
    samples_posterior     = matrix(rep(1:6, each = 5L), nrow = 5L, dimnames = list(NULL, bgmsOrder)),
    convergence_parameter = stats::setNames(1 + (1:6) / 10, bgmsOrder)
  )
  spec    <- list(type = c("ordinal", "continuous", "ordinal", "continuous"), baselineCategory = rep(1L, 4L))
  options <- list(edgePrior = "Bernoulli", groupingVariable = "")

  result <- jaspNetwork:::.bayesianNetworkAnalysisExtractEasybgmResult(fit, spec, options)

  testthat::expect_identical(colnames(result$samplesPosterior), c("A-B", "A-C", "A-D", "B-C", "B-D", "C-D"))
  testthat::expect_equal(unname(result$samplesPosterior[1L, ]), c(3, 1, 4, 5, 2, 6))
  testthat::expect_equal(result$convergence, 1 + c(3, 1, 4, 5, 2, 6) / 10)
})

testthat::test_that("A comparison of more than two groups keeps its pairwise differences", {
  variables  <- c("A", "B", "C", "D")
  edgeMatrix <- matrix(0, 4L, 4L, dimnames = list(variables, variables))
  groupOrder <- c("A-C", "A-B", "A-D", "B-C", "B-D", "C-D") # deliberately not row-major

  groupEstimates <- matrix(c(1:6, 11:16, 21:26), 6L, dimnames = list(groupOrder, c("group1", "group2", "group3")))
  differences    <- cbind(groupEstimates[, 2] - groupEstimates[, 1],
                          groupEstimates[, 3] - groupEstimates[, 1],
                          groupEstimates[, 3] - groupEstimates[, 2])
  dimnames(differences) <- list(groupOrder, c("group2 - group1", "group3 - group1", "group3 - group2"))

  compareFit <- list(
    inc_probs                  = edgeMatrix,
    inc_BF                     = edgeMatrix,
    structure                  = edgeMatrix,
    parameters                 = NULL,
    convergence_parameter      = stats::setNames(1 + (1:6) / 10, as.character(1:6)),
    group_estimates            = groupEstimates,
    pairwise_group_differences = differences
  )
  spec    <- list(type = rep("ordinal", 4L), baselineCategory = rep(1L, 4L))
  options <- list(edgePrior = "Bernoulli", groupingVariable = "group")

  result <- jaspNetwork:::.bayesianNetworkAnalysisExtractEasybgmResult(compareFit, spec, options, isDifferenceFit = TRUE)

  testthat::expect_identical(dim(result$estimates), c(4L, 4L))
  testthat::expect_true(all(is.na(result$estimates)))
  testthat::expect_identical(rownames(result$groupEstimates), c("A-B", "A-C", "A-D", "B-C", "B-D", "C-D"))
  testthat::expect_equal(unname(result$groupEstimates[, "group1"]), c(2, 1, 3, 4, 5, 6))
  testthat::expect_equal(unname(result$pairwiseGroupDifferences[, "group3 - group2"]), rep(10, 6L))
  # the comparison reports R-hat unnamed, in the order of its group estimates
  testthat::expect_equal(result$convergence, 1 + c(2, 1, 3, 4, 5, 6) / 10)
})

testthat::test_that("Evidence categories are exclusive at the boundaries", {
  category <- jaspNetwork:::.bayesianNetworkAnalysisEvidenceCategory

  testthat::expect_identical(category(c(10, 0.1, 1, NA, 0.05, 20), 10),
                             c("included", "excluded", "inconclusive", "inconclusive", "excluded", "included"))
  # A threshold of 1 would otherwise count BF = 1 as both included and excluded
  testthat::expect_identical(category(c(1, 0.5, 2), 1), c("included", "excluded", "included"))
  testthat::expect_identical(dim(category(matrix(c(0, 12, 12, 0), 2L), 10)), c(2L, 2L))
})

testthat::test_that("The parameter HDI table keeps the posterior mean", {
  # 99 zero draws and one draw of 10: the 95% HDI is [0, 0] but the mean is 0.1
  variables <- c("alpha", "beta", "gamma")
  samples   <- cbind(c(rep(0, 99L), 10), rep(0.2, 100L), rep(0.3, 100L))
  colnames(samples) <- c("alpha-beta", "alpha-gamma", "beta-gamma")
  graph   <- matrix(0, 3L, 3L, dimnames = list(variables, variables))
  network <- list(estimates = graph, graph = graph, samplesPosterior = samples)

  posterior <- jaspNetwork:::.bayesianNetworkAnalysisComputeParameterHdi(network, list(labelAbbreviation = FALSE), 0.95)
  edge      <- posterior[posterior$edge == "beta-alpha", ]

  testthat::expect_equal(edge$mean, 0.1)
  testthat::expect_equal(c(edge$lower, edge$upper), c(0, 0))
  testthat::expect_equal(edge$medianProbabilityEstimate, 0)
})

testthat::test_that("Group labels cannot take the place of the difference or pooled network", {
  allNetworks <- list(
    differences = list(role = "differences", label = "Differences"),
    pooled      = list(role = "pooled",      label = "Pooled"),
    group1      = list(role = "group",       label = "Differences"),
    group2      = list(role = "group",       label = "Pooled")
  )

  testthat::expect_identical(jaspNetwork:::.bayesianNetworkAnalysisDifferencesKey(allNetworks), "differences")
  testthat::expect_identical(unname(jaspNetwork:::.bayesianNetworkAnalysisNetworkLabels(allNetworks)),
                             c("Differences", "Pooled", "Differences", "Pooled"))
  # a split analysis with a group called "Differences" is not a comparison
  testthat::expect_false(jaspNetwork:::.bayesianNetworkAnalysisHasDifferences(allNetworks[c("group1", "group2")]))

  # networks stored by an earlier version carry no role and are keyed by label
  testthat::expect_true(jaspNetwork:::.bayesianNetworkAnalysisHasDifferences(list(Differences = list(), `Group 1` = list())))
})

testthat::test_that("Prior settings bgms rejects are reported as validation errors", {
  assertPriors <- jaspNetwork:::.bayesianNetworkAnalysisAssertSupportedPriors
  testthat::expect_error(assertPriors(list(edgePrior = "Stochastic-Block", groupingVariable = "group")))
  testthat::expect_silent(assertPriors(list(edgePrior = "Stochastic-Block", groupingVariable = "")))

  assertInteraction <- jaspNetwork:::.bayesianNetworkAnalysisAssertInteractionPriorSupported
  testthat::expect_error(assertInteraction(list(interactionPriorFamily = "beta-prime"), list(type = c("ordinal", "continuous"))))
  testthat::expect_silent(assertInteraction(list(interactionPriorFamily = "beta-prime"), list(type = c("ordinal", "blume-capel"))))
  testthat::expect_silent(assertInteraction(list(interactionPriorFamily = "normal"), list(type = c("continuous", "continuous"))))
})

testthat::test_that("SBM extraction keeps clusters attached to their variables", {
  variables <- c("A", "B", "C", "D")
  nativeOrder <- c("A", "C", "B", "D")
  edgeMatrix <- matrix(0, 4L, 4L, dimnames = list(variables, variables))
  coclustering <- matrix(c(1, .1, .8, .2,
                          .1, 1, .3, .4,
                          .8, .3, 1, .5,
                          .2, .4, .5, 1), 4L, byrow = TRUE,
                        dimnames = list(nativeOrder, nativeOrder))
  fit <- list(inc_probs = edgeMatrix, inc_BF = edgeMatrix, structure = edgeMatrix,
              parameters = edgeMatrix, sbm = list(
                posterior_mean_allocations = c(2L, 1L, 2L, 3L),
                posterior_mode_allocations = c(1L, 2L, 2L, 3L),
                posterior_num_blocks = data.frame(probability = c(0, .2, .8, 0)),
                posterior_mean_coclustering_matrix = coclustering
              ))
  spec <- list(type = c("ordinal", "continuous", "ordinal", "continuous"), baselineCategory = rep(1L, 4L))
  options <- list(edgePrior = "Stochastic-Block", groupingVariable = "")
  result <- jaspNetwork:::.bayesianNetworkAnalysisExtractEasybgmResult(fit, spec, options, keepRawFit = TRUE)

  testthat::expect_identical(result$sbm$posterior_mean_coclustering_matrix, coclustering[variables, variables])
  testthat::expect_identical(result$sbm$posterior_mean_allocations, c(A = 2L, B = 2L, C = 1L, D = 3L))
  testthat::expect_identical(result$sbm$posterior_mode_allocations, c(A = 1L, B = 2L, C = 2L, D = 3L))
  testthat::expect_identical(result$sbm$posterior_num_blocks, fit$sbm$posterior_num_blocks)
  testthat::expect_identical(result$easybgmFit, fit)

  # Named allocations may have their own order, independent of either matrix axis.
  sbm <- fit$sbm
  sbm$posterior_mean_allocations <- c(D = 3L, B = 2L, A = 2L, C = 1L)
  sbm$posterior_mode_allocations <- c(C = 2L, D = 3L, B = 2L, A = 1L)
  sbm$posterior_mean_coclustering_matrix <- coclustering[, rev(nativeOrder)]
  testthat::expect_identical(jaspNetwork:::.bayesianNetworkAnalysisCanonicalSbm(sbm, variables), result$sbm)

  # Already aligned summaries retain the same variable identities.
  testthat::expect_identical(jaspNetwork:::.bayesianNetworkAnalysisCanonicalSbm(result$sbm, variables), result$sbm)
})

testthat::test_that("SBM summaries reject missing or ambiguous variable identities", {
  variables <- c("A", "B", "C")
  sbm <- list(posterior_mean_allocations = c(1L, 1L, 2L),
              posterior_mode_allocations = c(1L, 2L, 2L),
              posterior_mean_coclustering_matrix = matrix(1, 3L, 3L, dimnames = list(variables, variables)))
  normalize <- jaspNetwork:::.bayesianNetworkAnalysisCanonicalSbm

  for (badNames in list(NULL, c("A", "B", "X"), c("A", "B", "B"))) {
    bad <- sbm
    rownames(bad$posterior_mean_coclustering_matrix) <- badNames
    testthat::expect_error(normalize(bad, variables), "could not be matched")
    bad <- sbm
    colnames(bad$posterior_mean_coclustering_matrix) <- badNames
    testthat::expect_error(normalize(bad, variables), "could not be matched")
  }

  for (field in c("posterior_mean_allocations", "posterior_mode_allocations")) {
    bad <- sbm
    bad[[field]] <- c(A = 1L, B = 1L, B = 2L)
    testthat::expect_error(normalize(bad, variables), "could not be matched")
    bad[[field]] <- c(1L, 2L)
    testthat::expect_error(normalize(bad, variables), "could not be matched")
  }
})

testthat::test_that("Mixed SBM tables and plot agree with the named bgms output", {
  set.seed(33)
  dataset <- data.frame(A = ordered(sample(1:3, 120, TRUE)), B = rnorm(120),
                        C = ordered(sample(1:3, 120, TRUE)), D = rnorm(120))
  options <- jaspTools::analysisOptions("BayesianNetworkAnalysis")
  options <- setNetworkVariables(options, stats::setNames(c("ordinal", "scale", "ordinal", "scale"), names(dataset)))
  options$edgePrior <- "Stochastic-Block"
  options$betaAlpha <- 9
  options$betaBeta <- 1
  options$betaAlpha_between <- 1
  options$betaBeta_between <- 9
  options$iter <- 80
  options$burnin <- 40
  options$chains <- "1"
  options$omrfUpdateMethod <- "adaptive-metropolis"
  options$setSeed <- TRUE
  options$seed <- 5
  options$clusterAllocationsTable <- TRUE
  options$clusterAllocationsType <- "mean"
  options$posteriorCoclusteringMatrixTable <- TRUE
  options$coclusteringPlot <- TRUE

  results <- jaspTools::runAnalysis("BayesianNetworkAnalysis", dataset, options, view = FALSE)
  testthat::expect_identical(results$status, "complete")

  networks <- Filter(function(state) is.list(state) && !is.null(state$group1$sbm), results$state$other)
  testthat::expect_length(networks, 1L)
  network <- networks[[1L]]$group1
  # The raw fit retains bgms's native order, so the reference does not reuse
  # JASP's normalization. No particular sampled partition is assumed.
  native <- bgms::extract_sbm(network$easybgmFit$packagefit)
  expectedMatrix <- native$posterior_mean_coclustering_matrix[names(dataset), names(dataset)]
  allocationOrder <- match(names(dataset), rownames(native$posterior_mean_coclustering_matrix))

  tables <- results$results$mainContainer$collection
  coclustering <- do.call(rbind, lapply(tables$mainContainer_sbmCoclusteringTable$data, as.data.frame))
  testthat::expect_identical(coclustering$Variable, names(dataset))
  testthat::expect_equal(unname(as.matrix(coclustering[, names(dataset)])), unname(expectedMatrix))

  allocations <- do.call(rbind, lapply(tables$mainContainer_sbmAllocationsTable$data, as.data.frame))
  testthat::expect_identical(allocations$variable, names(dataset))
  testthat::expect_equal(allocations$allocation, unname(native$posterior_mean_allocations[allocationOrder]))
  testthat::expect_equal(unname(network$sbm$posterior_mode_allocations), unname(native$posterior_mode_allocations[allocationOrder]))

  plots <- Filter(function(figure) inherits(figure$obj, "ggplot") &&
                    all(c("Var1", "Var2", "value") %in% names(figure$obj$data)), results$state$figures)
  testthat::expect_length(plots, 1L)
  plotData <- plots[[1L]]$obj$data
  pairs <- cbind(as.character(plotData$Var1), as.character(plotData$Var2))
  testthat::expect_equal(plotData$value, unname(expectedMatrix[pairs]))
})
