Network Analysis
===

Network Analysis allows the user to analyze the network structure of variables. Load the data of interest into the box `Variables`. In order to compare networks for different groups, add a grouping variable in `Split by`. In networks, observed variables are referred to as nodes and estimated relations are called edges.


### Assumptions

Assumption vary per network analysis method. In general, relations among variables are assumed to be *linear*.

Additional assumptions are required for some networks:

- Correlations & Partial correlation networks: If you want to interpret edges with only significant (partial) correlations, your data must be normally distributed. This assumptions only matters for the significance test.
- EBICglasso: your data are normally distributed.
- Mixed Graphical Models: Your variables are either normally distributed, catgorical, or Poisson (counts).

### Input
---

#### Assignment Box
- Variables: In this box the dependent variables are selected.  
- Split: Split by a categorical variable, such as experimental condition.

#### Estimator
This analysis allows you to estimate not one type of network, but multiple. Supported networks are:
- Correlation Networks
- Partial correlation Networks
- EBICglasso Networks (Foygel & Drton, 2010; Friedman, Hastie, & Tibshirani, 2008; Friedman, Hastie, & Tibshirani, 2014)
- Huge: High-dimensional undirected graph estimation (Zhao et a., 2015).
- Ising Networks (using IsingFit, van Borkulo et al., 2014; or IsingSampler, Epskamp, 2014; Epskamp, Borsboom & Maris, in Press).
- mgm: Mixed Graphical Models (Haslbeck & Waldorp, 2015).

#### Plots
- Network plot: a plot of the estimated network.
- Centrality plot: a plot of the centrality measures of the estimated network.
- Clustering plot: a plot of the clustering measures of the estimated network. 

#### Tables
- Centrality table: a table containing the values of the centrality measures.
- Clustering table: a table ocntaining the values of the clustering measures. 
- Weights matrix: the estimates parameters, in the form of a matrix.


### Analysis options
For each network method, options can be adjusted to influence the result. Only options available to a specific estimation method will be available at a time.

#### Correlation method
- `Auto`: Automatically detect variable type and uses the most suitable correlation type. This will detect continuous, binary and ordinal variables and will use Pearson, tetrachoric or polychoric correlations.
- `Cor`: Pearson correlation.
- `Cov`: Covariances.
- `Npn`: This will first apply the nonparanormal transformation to make all data normally distributed and then use Pearson correlations.
- `Spearman`: Spearman correlation.

#### Rule
What 'rule' should be used to determine if an edge is present between two nodes?

- `AND`: Both estimates, the edge from A to B and the edge from B to A, must be nonzero.
- `OR`: Either of the estimates must be nonzero.

#### Split
When estimating an Ising model (using either IsingFit or IsingSampler) non binary variables will be binarized using either:
- `Median`: the median of the observed scores.
- `Mean`: the mean of the observed scores.

#### Variable Type
When estimating a Mixed Graphical Model, insert a variable here to specify the variable type. Allowed inputs are:
- `g`: for normally distributed / Gaussian variables.
- `c`: for categorical variables.
- `p`: for Poisson variables.

The data should of the format "`variableName` = `group`", similarly to `data` for the layout and `color nodes by`. For example, if a variable is called "Number of cars" is assumed to be Poisson distributed this becomes: "Number of cars = p".

#### Ising Estimator
Many methods exist for estimating Ising models. Supported methods are:

- `Pseudo-likelihood`: Estimate the Ising model by maximizing the pseudolikelihood (TODO: Besag, 1975).
- `Univariate regressions`: Compute univariate logistic regressions for from all nodes to all other nodes. This gives two estimates for each edge which will then be combine using the method specified under Rule.
- `Bivariate regressions`: Compute bivariate logistic regressions for from all nodes to all other nodes. This gives two estimates for each edge which will then be combine using the method specified under Rule.
- `Loglinear`: Estimate the Ising model as if it were a loglinear model, that has at most pairwise interactions.

#### Missing Values
How should missing values be handled? Some analyses allow for pairwise exclusion, but not all.

#### Tuning Parameters
This parameter is the &gamma; hyperparameter of the EBIC estimation procedure. It controls the sparsity of the estimated network. Setting it to 0 will cause the regular BIC to be used.

#### Cross-validation
How many cross-validation samples should be made? This method is only used by Adaptive Lasso and  Mixed Graphical Models (if Criterion is set to cross-validation).

#### Thresholds
Thresholds to be used in correlation or partial correlation networks. This can either be set to a number, or to a method. If set to a number, edges with an absolute strength below that value will not be shown. Alternatively it can be set to a method to control the family-wise error rate. Available methods are:
- `Significant`: Show edges significant at the 0.05 level.
- `Bonferroni`: as `Significant`, but with the Bonferroni multiplicity correction.
- `Holm`: as `Significant`, but with the Holm multiplicity correction.
- `Hochberg`: as `Significant`, but with the Hochberg multiplicity correction. Assumes that  hypothesis tests are independent or non-negatively associated.
- `Hommel`: as `Significant`, but with the Holm multiplicity correction. Assumes that  hypothesis tests are independent or non-negatively associated.
- `BH`: as `Significant`, but controls the false discovery rate.

In most scenarios, the `Bonferroni` method is rather restrictive and the `Holm` method is preferred.

#### Criterion
What criterion should be used to fit the network? Available options are:

- `EBIC`: Extended Bayesian Information Criterion.
- `RIC`:  Rotation Information Criterion.
- `STARS`: Stability Approach to Regularization Selection.
- `CV`: Cross-validation.

#### Sample Size
- `Maximum`: Sets the sample size to the total number of rows including rows with NA's.
- `Minimum`: Sets the sample size to the total number of rows excluding rows with NA's.
- `Pairwise average`: Sets the sample size to the average of sample sizes used for each individual correlation.
- `Pairwise maximum`: Sets the sample size to the largest sample size used for each individual correlation.
- `Pairwise minimum`: Sets the sample size to the smallest sample size used for each individual correlation. 

#### Network
If you untick `Weighted`, the estimated network will only consist of positive (1) negative (-1) and absent (0) edges. If you untick `Signed`, the estimated network will only consist of positive edges. Note that the absolute value is taken of negative edges, to make them positive. If you untick both `Weighted` and `Signed` the network will say if there is an edge (1) or not (0).


#### Centrality Measures
Centralilty measures of a network can be difficult to compare. To facilitate this, you can select `Normalized` to ensure each centrality measure has a mean of zero and a variance of one. Alternatively, you can select relative to divide each centrality measure by it's maximum observed value.


### Bootstrap options
To investigate the stability of estimated networks, check `Bootstrap Network` underneath `Estimator`. Doing so will automatically bootstrap the edges of estimated network and their centrality. Additional options can be specified, such as the number of bootstraps and the type of bootstrap. 


#### Statistics 
- `Edges`: a plot of the bootstrapped edges of the estimated network. 
- `Centrality`: a plot of the bootstrapped centrality measures of the estimated network. 

#### Bootstrap Type
- `Nonparameteric`: Resampling from the data with replacement. 
- `Case`: Case-dropping bootstrap (which is resampling with different subsets of the data)
- `Node`:
- `Parametric`:
- `Person`:
- `Jackknife`:

### Graphical options
All options below modify the output from Network plot. Other figures and tables are not affected. To make Networks plots aesthetically pleasing, many options exist.

#### Layout
The layout of a network determines where the nodes are placed. By default the layout is set to `spring`, which implies the layout will be generated via the force-driven Fruchterman-Reingold algorithm. This algorithm can be tweaked using the `repulsion` parameter; a larger repulsion increases the distance between adjacent nodes. Alternatively all nodes can be displayed in a circle by selecting the `circle` layout. A third option is called `data`. Here, coordinates can be supplied for the networks. The data should of the format "`variableName` = `group`", similarly to `Variable Type` and the `color nodes by`. For example, if a variable is called "A1" and should be plotted at x-coordinate 1 this becomes: "A1 = 1".

#### Edges
- `Edge size`: A multiplier on edge size (i.e. 2 is twice as big).
- `minimum`: The (absolute) minimum edge strength to be displayed.
- `maximum`: The (absolute) minimum edge strength to be displayed.
- `cut`: Scales the width and the saturation of the edges. 
- `show details`: If checked, `minimum`, `maximum`, and `cut` will be displayed on the network plot (if they were modified).
- `color scheme`: What colors should be used for positive and negative edges?

#### Legend
There are three options:

- Don't show the legend.
- Show the legend in all networks.
- Show the legend in a specified plot number.

- `Legend to plot ratio`: Specifies the width of the legend relative to the plot.

#### Labels
- `Label size`: A multiplier on label size  (i.e. 2 is twice as big).
- `Scale label size`:
- `Abbreviate labels to ... characters`: If labels are too long, they can be automatically abbreviated.

#### Nodes
- `Node size`: A multiplier on node size  (i.e. 2 is twice as big).
- `color nodes by`: A categorical variable that indicates the group to which each variable belongs. The data should of the format "`variableName` = `group`", similarly to `Variable Type` and the `data` option for the layout. For example, if a variable is called "JASP" and belongs to the group "Open Source" this becomes: "JASP = Open Source".
- `color scheme`: What colors should be used for the coloring?

#### Show variable names
An alternative to abbreviating the node labels is showing them in the legend. This can be enabled by clicking `In legend`, or disabled by clicking `In nodes`.

#### Show variable type
When estimating a Mixed Graphical Model, the assumed distribution of a variable can be displayed in multiple ways:

- `Dont' show`: Do not show the assumed distribution.
- `Using node colors`: Change the color of the nodes based on their assumed distribution. Ignored if  `color nodes by` is set.
- `Using node shape`: Change the shape of the nodes based on their assumed distribution. Gaussian nodes are circles, categorical nodes are squares, and Poisson nodes are triangles.

### Output
-------

#### Summary of the network
The summary table shows the number of nodes (equal to the number of variables selected), the number of non-zero edges, and the sparsity. Sparsity is a value between 0 and 1, where te higher the sparsity, the more weakly connected the network.

#### Network 
The network plot under the specified Graphical Options. The stronger the association between nodes, the thicker and more saturated the edge is represented in the network. The coloring of the edges refers to the direction of the association: The default colors are blue and red, where blue edges represent positive associations and red edges negative associations. 

#### Centrality plot
The centrality plot shows the indices of all nodes for the three centrality measures betweenness, closeness and Strength. The indices are shown as standardzed z-scores. 

#### Clustering plot 
The clustering plot shows the indices of all nodes for the four clustering measures Barrat, Onnela, WS and Zhang.

#### Centrality measures per variable
The values of the centrality measures betweenness, closeness and strength are shown in a table. 

#### Clustering measure per variable 
The values of the clustering measures Barrat, Onnela, WS and Zhang are shown in a table. 

#### Weights matrix
The values of all the edge weights (i.e., strength of the associations between nodes in the network) are shown in a table. 

#### Bootstrap summary of Network 
The summary table shows the bootstrap method used and the number of bootstraps. 

#### Edge stability 
When the non-parmateric bootstrap is selected: The grey area shows the bootstrapped confidence intervals of the estimated edge weights for the estimated network. The red values (connected by the red line) indicate the sample mean values for the bootstrapped edge weights. The black values indicate the estimated edge weights. 

When other bootstrap types are selected: The plot shows the correlation between the edge weights for estimated network and the bootstrapped network. The line indicates the mean correlation and the area around the line indicates the 2.5th till the 97.5th quantile. 

#### Centrality Stability 
When the non-parmateric bootstrap is selected: The plot shows per centrality measures the difference test (with an alpha of 0.05) between the estimated and bootstrapped centrality measures. Black boxes indicate that centrality measures differ significantly, while grey boxes indicate no significant difference. The numbers in the white boxes refer to the value of the node strenght. 

When other bootstrap types are selected: The plot shows the average correlation between the centrality measures for the estimated network and the bootstrapped network. The lines indicates the mean correlation between centrality measures and the area around the indicates the 2.5th till the 97.5th quantile. 

### References
-------
The reference list below contains references for all networks. A subset of references relevant to a specific estimator can be obtained by right clicking the downward arrow right of any table and selecting "Copy Citations".

- van Borkulo, C. D., Borsboom, D., Epskamp, S., Blanken, T. F., Boschloo, L., Schoevers, R. A., & Waldorp, L. J. (2014). A new method for constructing networks from binary data. *Scientific reports, 4*(5918), 1-10.
- Epskamp, S., Borsboom, D., & Fried, E. I. (2016). Estimating psychological networks and their accuracy: a tutorial paper. arXiv preprint, arXiv:1604.08462.
- Epskamp, S., Cramer, A. O., Waldorp, L. J., Schmittmann, V. D., & Borsboom, D. (2012). qgraph: Network visualizations of relationships in psychometric data. *Journal of Statistical Software, 48*(4), 1-18.
Chicago
- Epskamp, S., Maris, G., Waldorp, L., & Borsboom, D. (in press). Network psychometrics. In P. Irwing, D. Hughes, & T. Booth (Eds.), *Handbook of psychometrics.* New York, NY, USA: Wiley.
- Epskamp, S. (2014). IsingSampler: Sampling methods and distribution functions for the Ising model. Retrieved from github.com/SachaEpskamp/IsingSampler
- Foygel, R., & Drton, M. (2010). Extended Bayesian information criteria for Gaussian graphical models. *In Advances in neural information processing systems* (pp. 604-612).
- Friedman, J., Hastie, T., & Tibshirani, R. (2008). Sparse inverse covariance estimation with the graphical lasso. *Biostatistics, 9*(3), 432-441.
- Friedman, J. H., Hastie, T., & Tibshirani, R. (2014). glasso: Graphical lasso estimation of gaussian graphical models. Retrieved from https://CRAN.R-project.org/package=glasso
- Fruchterman, T. M., & Reingold, E. M. (1991). Graph drawing by force-directed placement. Software: Practice and experience, 21(11), 1129-1164.
- Haslbeck, J., & Waldorp, L. J. (2015). mgm: Structure Estimation for time-varying mixed graphical models in high-dimensional data. arXiv preprint arXiv:1510.06871.
- Kraeamer, N., Schaeafer, J., & Boulesteix, A.-L. (2009). Regularized estimation of large-scale gene association networks using graphical gaussian models. *BMC Bioinformatics, 10*(1), 1-24.
- Zhao, T., Li, X., Liu, H., Roeder, K., Lafferty, J., & Wasserman, L. (2015). huge: High-dimensional undirected graph estimation. Retrieved from https://CRAN.R-project.org/package=huge


### R Packages
---
- bootnet
- glasso
- huge
- mgm
