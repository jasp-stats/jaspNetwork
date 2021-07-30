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
  
  mainContainer <- .bayesianNetworkAnalysisSetupMainContainerAndTable(jaspResults, dataset, options)
  
  
  return()
  
}

# tables ----
.bayesianNetworkAnalysisSetupMainContainerAndTable <- function(jaspResults, dataset, options) {
  
  mainContainer <- jaspResults[["mainContainer"]]
  if (is.null(mainContainer)) {
    mainContainer <- createJaspContainer(dependencies = c(
      # data
      "variables", "groupingVariable", 

      # arguments for the estimation
      "burnin", "iter", "gprior", "dfprior"
      
    ))
    jaspResults[["mainContainer"]] <- mainContainer
  }
  .bayesianNetworkAnalysisMainTableMeta(mainContainer, dataset, options)
  
  return(mainContainer)
}

.bayesianNetworkAnalysisMainTableMeta <- function(mainContainer, dataset, options) {
  
  if (is.null(mainContainer[["generalTable"]])) {
    tb <- createJaspTable(gettext("Summary of Network"), position = 1) # check dependencies 
    if (length(dataset) > 1L)
      tb$addColumnInfo(name = "info", title = gettext("Network"), type = "string")
    
    tb$addColumnInfo(name = "nodes",    title = gettext("Number of nodes"),          type = "integer")
    tb$addColumnInfo(name = "nonZero",  title = gettext("Number of non-zero edges"), type = "string")
    tb$addColumnInfo(name = "Sparsity", title = gettext("Sparsity"),                 type = "number")
    
    mainContainer[["generalTable"]] <- tb
  }
  return()
}

