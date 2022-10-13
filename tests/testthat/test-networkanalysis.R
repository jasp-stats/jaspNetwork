context("Network Analysis")

# does not test
# - error handling
# - bootstrapping
# - plots or graphical options

options <- jaspTools::analysisOptions("NetworkAnalysis")
options$estimator <- "ebicGlasso"
options$variables <- c("contNormal", "contcor1", "contcor2")
options$centralityTable <- TRUE
options$clusteringTable <- TRUE
options$weightsMatrixTable <- TRUE
options$tableLayout <- TRUE
results <- jaspTools::runAnalysis("NetworkAnalysis", "test.csv", options)

test_that("generalTB table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_generalTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(0.333333333333333, 3, "2 / 3")
  )
})

test_that("centralityTB table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_centralityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(-0.577350269189626, -1.12003079401266, -1.14291478283658, -1.14291478283658,
                           "contNormal", 1.15470053837925, 0.803219560925303, 0.713968266021615,
                           0.713968266021615, "contcor1", -0.577350269189626, 0.316811233087358,
                           0.428946516814968, 0.428946516814968, "contcor2")
  )
})

test_that("clusteringTB table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_clusteringTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("contNormal", 0, 0, 0, 0, "contcor1", 0, 0, 0, 0, "contcor2",
                           0, 0, 0, 0)
  )
})

test_that("weightmatrixTB table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_weightsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("contNormal", 0, 0.0939476582188346, 0, "contcor1", 0.0939476582188346,
                           0, 0.612057902640958, "contcor2", 0, 0.612057902640958, 0)
  )
})


options <- jaspTools::analysisOptions("NetworkAnalysis")
options$bootstrap <- TRUE
options$bootstrapSamples <- 2
options$centralityPlot <- TRUE
options$clusteringPlot <- TRUE
options$networkPlot <- TRUE
options$legend <- "allPlots"
options$variableNamesShown <- "inNodes"
options$centralityTable <- TRUE
options$clusteringTable <- TRUE
options$weightsMatrixTable <- TRUE
options$variables <- list("A1", "A2", "A3", "A4", "A5")
estimators <- c("ebicGlasso","cor","pcor","isingFit","isingSampler","huge","adalasso")
file <- testthat::test_path("networkResults.rds")

# run the code below to create the .rds object
# clearEverythingButData <- function(x) {
#  # a small helper to remove everything but the data from the jaspResults objects to save.
#   if (is.list(x) && !is.null(x[["data"]])) {
#     return(x["data"])
#   } else {
#     x <- Filter(is.list, x)
#     return(lapply(x, clearEverythingButData))
#   }
# }
#
# results <- vector("list", length(estimators))
# names(results) <- names(estimators)
# for (e in estimators) {
#   options$estimator <- e
#   set.seed(1)
#   results[[e]] <- jaspTools::runAnalysis(options = options, data = "BFI Network.csv", view = FALSE)["results"]
#   results[[e]] <- clearEverythingButData(results[[e]])
# }
# saveRDS(results, file = file)
storedResults <- readRDS(file)

skip_if_adalasso <- function(estimator) {
  skip_if(estimator == "adalasso", r"(dependency "parcor" was removed from CRAN so this test is skipped)")
}

for (estimator in estimators) {


  options$estimator <- estimator
  set.seed(1)
  results <- jaspTools::runAnalysis("NetworkAnalysis", "BFI Network.csv", options)

  test_that(paste0(estimator, ": Centrality measures per variable table results match"), {
    skip_if_adalasso(estimator)
    table    <- results                   [["results"]][["mainContainer"]][["collection"]][["mainContainer_centralityTable"]][["data"]]
    oldTable <- storedResults[[estimator]][["results"]][["mainContainer"]][["collection"]][["mainContainer_centralityTable"]][["data"]]
    jaspTools::expect_equal_tables(table, jaspTools:::collapseTestTable(oldTable))
  })

  test_that(paste0(estimator, ": Clustering measures per variable table results match"), {
    skip_if_adalasso(estimator)
    table    <- results                   [["results"]][["mainContainer"]][["collection"]][["mainContainer_clusteringTable"]][["data"]]
    oldTable <- storedResults[[estimator]][["results"]][["mainContainer"]][["collection"]][["mainContainer_clusteringTable"]][["data"]]
    jaspTools::expect_equal_tables(table, jaspTools:::collapseTestTable(oldTable))
  })

  test_that(paste0(estimator, ": Summary of Network table results match"), {
    skip_if_adalasso(estimator)
    table    <- results                   [["results"]][["mainContainer"]][["collection"]][["mainContainer_generalTable"]][["data"]]
    oldTable <- storedResults[[estimator]][["results"]][["mainContainer"]][["collection"]][["mainContainer_generalTable"]][["data"]]
    jaspTools::expect_equal_tables(table, jaspTools:::collapseTestTable(oldTable))
  })

  test_that(paste0(estimator, ": Weights matrix table results match"), {
    skip_if_adalasso(estimator)
    table    <- results                   [["results"]][["mainContainer"]][["collection"]][["mainContainer_weightsTable"]][["data"]]
    oldTable <- storedResults[[estimator]][["results"]][["mainContainer"]][["collection"]][["mainContainer_weightsTable"]][["data"]]
    jaspTools::expect_equal_tables(table, jaspTools:::collapseTestTable(oldTable))
  })

  test_that(paste0(estimator, ": Bootstrapped edge plot matches"), {
    if (estimator == "isingSampler")
      skip("Cannot reliably test Bootstrapped edge plot for isingSampler")
    skip_if_adalasso(estimator)
    plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_bootstrapContainer"]][["collection"]][["mainContainer_bootstrapContainer_EdgeStabilityPlots"]][["collection"]][["mainContainer_bootstrapContainer_EdgeStabilityPlots_Network"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, paste0(estimator, "-bootstrapped-edges"))
  })

  test_that(paste0(estimator, ": Bootstrapped centrality plot matches"), {
    skip_if_adalasso(estimator)
    plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_bootstrapContainer"]][["collection"]][["mainContainer_bootstrapContainer_statisticsCentralityPlots"]][["collection"]][["mainContainer_bootstrapContainer_statisticsCentralityPlots_Network"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, paste0(estimator, "-bootstrapped-centrality"))
  })

  test_that(paste0(estimator, ": Centrality Plot matches"), {
    skip("Cannot reliably test Centrality Plots")
    skip_if_adalasso(estimator)
    plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_centralityPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, paste0(estimator, "-centrality-plot"))
  })

  test_that(paste0(estimator, ": Clustering Plot matches"), {
    skip("Cannot reliably test Clustering Plots")
    skip_if_adalasso(estimator)
    plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_clusteringPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, paste0(estimator, "-clustering-plot"))
  })

  test_that(paste0(estimator, ": Network plot matches"), {
    if (estimator == "isingSampler")
      skip("Cannot reliably test Network plot for isingSampler")
    skip_if_adalasso(estimator)
    plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_networkPlotContainer"]][["collection"]][["mainContainer_plotContainer_networkPlotContainer_Network"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, paste0(estimator, "-network-plot"))
  })
}

# test error check

set.seed(123)
n <- 50
p <- 5
dataset <- matrix(rnorm(n * p), n, p)
dataset[sample(n, 0.5 * n), ] <- NA
dataset <- as.data.frame(dataset)
options <- jaspTools::analysisOptions("NetworkAnalysis")
options$estimator <- "ebicGlasso"
options$variables <- c("V1", "V2", "V3")
results <- jaspTools::runAnalysis("NetworkAnalysis", dataset, options)

test_that("Too many missing rows returns an error", {
  r <- results[["results"]]
  expect_identical(results[["status"]], "validationError")
  expect_true(any(grepl("rows", results[["results"]][["errorMessage"]], ignore.case = TRUE)), label = "Entirely missing rows check")
})


# test incorrect layout input does not crash ----

set.seed(123)
n <- 50
p <- 3
dataset <- as.data.frame(matrix(rnorm(n * (p-1)), n, p-1))
dataset$V3 <- dataset$V1 + dataset$V2 + rnorm(n)
dataset$layoutX <- c("V1 = 1", "V2 = 0")
dataset$layoutY <- c("V1 = 1", "V2 = 0")
options <- jaspTools::analysisOptions("NetworkAnalysis")
options$estimator <- "ebicGlasso"
options$variables <- c("V1", "V2", "V3")
options$layoutX <- "layoutX"
options$layoutY <- "layoutY"
options$networkPlot <- TRUE
results <- jaspTools::runAnalysis("NetworkAnalysis", dataset, options)

test_that("Incorrect user layout shows a warning", {
  footnote <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_generalTable"]][["footnotes"]][[1]]$text
  expect_identical(footnote, "Supplied data for layout was not understood and instead a circle layout was used. X-Coordinates for variable V3 was not understood. Y-Coordinates for variable V3 was not understood.")

  layout <- results[["state"]][["figures"]][[1L]][["obj"]][["layout"]]
  expect_equal(layout, structure(c(-0.154179671844446, -1, 1, -1, 1, 0.354868971143063), .Dim = 3:2))
})

# test partial correlation networks work for all thresholds.
options <- analysisOptions("NetworkAnalysis")
options$estimator <- "pcor"
options$variables <- c(paste0("A", 1:5), paste0("O", 1:5), paste0("E", 1:5))
options$networkPlot <- TRUE
options$weightsMatrixTable <- TRUE
options$thresholdBox <- "method"

table2df <- function(data, variables) {
  # transform raw data to data.frame for comparison
  df <- data.frame(Variable = sapply(data, `[[`, "Variable"))
  for (i in seq_along(variables))
    df[[variables[i]]] <- unlist(data[[i]][seq_along(variables)], use.names = FALSE)
  df
}

parcorFile <- testthat::test_path("networkResultsParCorThresholds.rds")

# to create the results object
# tbls <- list()
# set.seed(147)
# for (thresholdMethod in c("sig", "bonferroni", "locfdr", "holm", "hochberg", "hommel", "BH", "BY", "fdr")) {
#   options$thresholdMethod <- thresholdMethod
#   rr <- runAnalysis(dataset = "BFI Network.csv", options = options, view = FALSE)
#   df <- table2df(rr[["results"]][["mainContainer"]][["collection"]][["mainContainer_weightsTable"]][["data"]], options$variables)
#   tbls[[thresholdMethod]] <- df
# }
# saveRDS(tbls, file = parcorFile)

tbls <- readRDS(file = parcorFile)

set.seed(147)
thresholdMethods <- c("sig", "bonferroni", "locfdr", "holm", "hochberg", "hommel", "BH", "BY", "fdr")
for (thresholdMethod in thresholdMethods) {

  options$thresholdMethod <- thresholdMethod
  results <- runAnalysis(dataset = "BFI Network.csv", options = options)

  test_that(paste0("parcor-threshold-", thresholdMethod, ": Weights matrix matches"), {
    df <- table2df(results[["results"]][["mainContainer"]][["collection"]][["mainContainer_weightsTable"]][["data"]], options$variables)
    expected <- tbls[[thresholdMethod]]
    testthat::expect_equal(df, expected, label = paste0("parcor-threshold-", thresholdMethod))
  })

  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_networkPlotContainer"]][["collection"]][["mainContainer_plotContainer_networkPlotContainer_Network"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  test_that(paste0("parcor-threshold-", thresholdMethod, ": Network Plot matches"), {
    expect_equal_plots(test = testPlot, paste0("parcor-threshold-", thresholdMethod))
  })
}


