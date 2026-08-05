# jaspTools installs the module into a temporary library on the first
# runAnalysis() call. Tests that reach into the namespace with ::: therefore fail
# on a clean CI checkout unless an analysis has already run. Trigger the install
# once here so those tests do not depend on the order of the test files.
#
# No variables are assigned, so this returns before any model is fitted.
local({
  options <- jaspTools::analysisOptions("BayesianNetworkAnalysis")
  options$variables <- character(0)
  invisible(jaspTools::runAnalysis("BayesianNetworkAnalysis", "test.csv", options, view = FALSE))
})
