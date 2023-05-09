context("Bayesian Network Analysis")

# does not test
# - error handling
# - bootstrapping
# - plots or graphical options


# based on https://github.com/jasp-stats/jasp-test-release/issues/2300
testthat::test_that("Analysis handles too many missing values errors", {
  options <- jaspTools::analysisOptions("BayesianNetworkAnalysis")
  options$estimator <- "gcgm"
  options$variables <- c("contNormal", "contGamma", "debMiss99")
  results <- jaspTools::runAnalysis("BayesianNetworkAnalysis", "test.csv", options)

  testthat::expect_true(results[["results"]][["error"]])
  testthat::expect_true(grepl("observations", results[["results"]][["errorMessage"]], fixed = TRUE))
})

testthat::test_that("Analysis handles too many missing values errors with grouping variable", {
  options <- jaspTools::analysisOptions("BayesianNetworkAnalysis")
  options$estimator <- "gcgm"
  options$variables <- c("contNormal", "contGamma", "debMiss80")
  options$groupingVariable <- "facFifty"
  options$dfprior <- 3
  options$gprior  <- "0.5"
  results <- jaspTools::runAnalysis("BayesianNetworkAnalysis", "test.csv", options)

  testthat::expect_true(results[["results"]][["error"]])
  testthat::expect_true(grepl("observations", results[["results"]][["errorMessage"]], fixed = TRUE))

})

testthat::test_that("Centrality plot works with empty graphs", {

  sleep <- structure(list(extra = c(0.7, -1.6, -0.2, -1.2, -0.1, 3.4, 3.7, 0.8, 0, 2, 1.9, 0.8, 1.1, 0.1, -0.1, 4.4, 5.5, 1.6, 4.6, 3.4),
                          group = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L),
                          ID = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L)),
                     class = "data.frame", row.names = c(NA, -20L))
  sleep$group <- factor(sleep$group)
  sleep$ID    <- factor(sleep$ID, ordered = TRUE)

  options <- analysisOptions("BayesianNetworkAnalysis")
  options$estimator <- "gcgm"
  options$variables <- c("extra", "group", "ID")
  options$dfprior <- 3
  options$gprior  <- "0.5"
  options$manualColorGroups <- list(list(color = "red", name = "Group 1"), list(color = "red", name = "Group 2"))

  results <- jaspTools::runAnalysis("BayesianNetworkAnalysis", sleep, options)

  dput(options2)
  dput(options)
  all.equal(options, options2)

  setdiff(names(options2), names(options))
})
