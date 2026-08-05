context("Bayesian Network Analysis")

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
  options$variables <- c("contNormal", "contGamma", "debMiss99")
  options$variables.types <- rep("scale", length(options$variables))
  results <- jaspTools::runAnalysis("BayesianNetworkAnalysis", "test.csv", options)

  errorMessage <- results[["results"]][["errorMessage"]]
  testthat::expect_true(results[["results"]][["error"]])
  testthat::expect_identical(results[["status"]], "validationError")
  testthat::expect_true(is.character(errorMessage) && length(errorMessage) == 1L && nzchar(errorMessage))
})

testthat::test_that("Analysis handles too many missing values errors with grouping variable", {
  options <- jaspTools::analysisOptions("BayesianNetworkAnalysis")
  options$estimator <- "gcgm"
  options$variables <- c("contNormal", "contGamma", "debMiss80")
  options$variables.types <- rep("scale", length(options$variables))
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
testthat::test_that("Centrality plot works with empty graphs", {

  testthat::skip("Not reproducible")

  sleep <- structure(list(extra = c(0.7, -1.6, -0.2, -1.2, -0.1, 3.4, 3.7, 0.8, 0, 2, 1.9, 0.8, 1.1, 0.1, -0.1, 4.4, 5.5, 1.6, 4.6, 3.4),
                          group = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L),
                          ID = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L)),
                     class = "data.frame", row.names = c(NA, -20L))
  sleep$group <- factor(sleep$group)
  sleep$ID    <- factor(sleep$ID, ordered = TRUE)

  options <- analysisOptions("BayesianNetworkAnalysis")
  options$estimator <- "gcgm"
  options$variables <- c("extra", "group", "ID")
  options$variables.types <- rep("scale", length(options$variables))
  options$dfprior <- 3
  options$gprior  <- "0.5"
  options$manualColorGroups <- list(list(color = "red", name = "Group 1"), list(color = "red", name = "Group 2"))
  options$centralityPlot <- TRUE
  options$credibilityInterval <- TRUE
  options$burnin <- 100
  options$iter   <- 500
  options$group  <- ""
  options$initialConfiguration <- "empty"
  results <- jaspTools::runAnalysis("BayesianNetworkAnalysis", sleep, options)

  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_generalTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "2 / 3", 0, 3, 0.333333333333333))

  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_centralityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "centrality-plot")

})

testthat::test_that("Parameter HDI plot works", {
  options <- jaspTools::analysisOptions("BayesianNetworkAnalysis")
  options$variables <- c("contNormal", "contcor1", "contcor2")
  options$variables.types <- rep("scale", length(options$variables))
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

  # HDI bounds come from MCMC sampling that set.seed() does not govern, so the plot is not reproducible
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

testthat::test_that("Gibbs is rejected for non-continuous variables and group comparisons", {
  options <- list(omrfUpdateMethod = "gibbs")

  # Accepted: all-continuous, no comparison
  testthat::expect_silent(
    jaspNetwork:::.bayesianNetworkAnalysisAssertUpdateMethodSupported(
      options, list(type = c("continuous", "continuous")), useCompare = FALSE
    )
  )

  testthat::expect_error(
    jaspNetwork:::.bayesianNetworkAnalysisAssertUpdateMethodSupported(
      options, list(type = c("continuous", "ordinal")), useCompare = FALSE
    )
  )

  testthat::expect_error(
    jaspNetwork:::.bayesianNetworkAnalysisAssertUpdateMethodSupported(
      options, list(type = c("ordinal", "ordinal")), useCompare = TRUE
    )
  )

  # NUTS is unaffected by either restriction
  testthat::expect_silent(
    jaspNetwork:::.bayesianNetworkAnalysisAssertUpdateMethodSupported(
      list(omrfUpdateMethod = "nuts"), list(type = c("ordinal", "ordinal")), useCompare = TRUE
    )
  )
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
  options$variables       <- c("facFive", "contBinom", "facGender")
  options$variables.types <- rep("ordinal", 3L)
  options$variablesBlumeCapel <- list(list(variable = "facFive", levels = "2"))
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

  # Estimates come from MCMC that set.seed() does not govern, so only structure is checked
  testthat::expect_true(all(is.finite(rows$estimate)))
  testthat::expect_true(all(rows$lower <= rows$upper))
})

testthat::test_that("Parameter HDI table reports one row per edge", {
  options <- jaspTools::analysisOptions("BayesianNetworkAnalysis")
  options$variables       <- c("contNormal", "contcor1", "contcor2")
  options$variables.types <- rep("scale", 3L)
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
    c("relation", "mean", "lower", "upper")
  )

  rows <- do.call(rbind, lapply(table[["data"]], as.data.frame))
  testthat::expect_equal(nrow(rows), 3L)
  testthat::expect_equal(rows$mean, sort(rows$mean))
  testthat::expect_true(all(rows$lower <= rows$mean & rows$mean <= rows$upper))
})
