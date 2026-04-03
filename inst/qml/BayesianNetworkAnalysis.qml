//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import QtQuick.Layouts
import JASP.Controls

Form
{
	info: qsTr("Bayesian Network Analysis estimates the structure and strength of conditional associations among variables in a graphical model.\n") +
		qsTr("The module supports the Gaussian graphical model (for continuous variables), the ordinal Markov random field, for ordinal (and binary) variables including Blume-Capel parameterizations when the ordinal variables with more than two categories have a meaningful neutral category,
		as well as a mixed graphical model of continuous and ordinal (Blume-Capel) variables.\n") +
		qsTr("When a grouping factor variable is supplied, one network is estimated per group. A a statistical comparison of network differences is available if all selected variables are ordinal or Blume-Capel; otherwise, only group-specific networks are estimated.\n") +
		qsTr("Posterior summaries include the edge weight estimates, along with the edge inclusion probabilities and inclusion Bayes factors, that can be inspected using the different tables and plots.\n") +

VariablesForm
{
	AvailableVariablesList
	{
		name: "allVariablesList"
		info: qsTr("Variables available for inclusion in the network model.")
	}

	AssignedVariablesList
	{
		name: "variables"
		title: ""
		info: qsTr("Select variables to include as nodes in the network. Continuous (scale) and ordinal variables are accepted. When a grouping variable is present, a difference network is estimated only if all selected variables are ordinal or Blume-Capel; otherwise separate group networks are estimated.")
		allowedColumns: ["ordinal", "scale"]
		allowTypeChange: false
		id: networkVariables
		rowComponentTitle: qsTr("Blume-Capel / Baseline category")
		rowComponent: RowLayout
		{
			spacing: 6

			Item { Layout.preferredWidth: rowType == "ordinal" ? 10 : 0 }

			CheckBox
			{
				id: blumeCapelToggle
				name: "blumeCapel"
				label: qsTr("B-C")
				checked: false
				visible: rowType == "ordinal"
				enabled: rowType == "ordinal"
				info: qsTr("Treat this ordinal variable using the Blume-Capel parameterization in the OMRF model. Use this when the variable has a meaningful neutral or reference category (e.g., a symmetric Likert scale with a midpoint).")
			}

			DropDown
			{
				name: "levels"
				label: qsTr("Baseline")
				source: [{ values: [rowValue], use: "levels" }]
				visible: rowType == "ordinal" && blumeCapelToggle.checked
				enabled: blumeCapelToggle.checked
				info: qsTr("Select the neutral or reference category for the Blume-Capel parameterization. Distances between ordinal categories are measured relative to this baseline.")
			}
		}
	}

	AssignedVariablesList
	{
		name: "groupingVariable"
		id: groupingVariableSelector
		title: qsTr("Compare Networks Across Groups")
		singleVariable: true
		allowedColumns: ["nominal"]
		info: qsTr("Select a nominal variable to estimate one network per group. A difference network is estimated only when all selected variables are ordinal or Blume-Capel; if scale variables are included, only separate group networks are estimated. At least 2 groups and 3 observations per group are required.")
	}
}

	Group
	{
		title: qsTr("Plots")
		CheckBox
		{
			name: "networkPlot"
			label: qsTr("Network plot")
			info: qsTr("Displays the posterior mean partial association network. Edge width and color saturation reflect association strength; blue edges are positive and red edges are negative. Edges whose BF\u2081\u2080 falls below the inclusion threshold are hidden.")
			IntegerField
			{
				name:			"networkPlotInclusionCriteria"
				label:			qsTr("Inclusion criteria: BF\u2081\u2080 > ")
				info: qsTr("Edges with BF\u2081\u2080 below this value are set to zero in the network plot.")
				min:			1
				defaultValue:	10
				max:			2e2
			}
		}
		CheckBox
		{
			name: "evidencePlot";
			label: qsTr("Edge evidence plot")
			info: qsTr("Displays the network with edges colored by the evidence category they belong to. Blue edges have BF\u2081\u2080 at or above the threshold (evidence for inclusion); yellow edges have BF\u2081\u2080 at or below the reciprocal threshold (evidence for exclusion); gray edges fall in between (absence of evidence).")
			IntegerField
			{
				name:			"edgeInclusionCriteria";
				label:			qsTr("Inclusion criteria: BF\u2081\u2080 > ");
				info: qsTr("BF threshold for categorizing evidence. An edge is evidence for inclusion if BF\u2081\u2080 \u2265 threshold, evidence for exclusion if BF\u2081\u2080 \u2264 1/threshold, and absence of evidence otherwise.")
				min:			1;
				defaultValue:	10;
				max:			2e2
			}
			CheckBox { name: "edgeInclusion";	label: qsTr("Evidence for inclusion");	checked: true; info: qsTr("Show edges with BF\u2081\u2080 \u2265 threshold, colored blue.") }
			CheckBox { name: "edgeExclusion";	label: qsTr("Evidence for exclusion");	checked: true; info: qsTr("Show edges with BF\u2081\u2080 \u2264 1/threshold, colored yellow.") }
			CheckBox { name: "edgeAbsence";		label: qsTr("Absence of evidence"); 	checked: true; info: qsTr("Show edges where BF\u2081\u2080 falls between the two thresholds, colored gray.") }
	}
	CheckBox
	{
		name: "centralityPlot"; id: centralityPlot; label: qsTr("Centrality plot")
		info: qsTr("Displays posterior mean centrality for the selected measures (betweenness, closeness, strength, expected influence). Measures are plotted side by side per node. Only available when no grouping variable is selected.")
		CheckBox
		{
			name: "credibilityInterval";
			label: qsTr("Credibility interval 95%");
			info: qsTr("Adds 95% highest density intervals (HDI) to centrality summaries.")
			checked: false;
		}
	}
	CheckBox
	{
		name: "parameterHdiPlot"
		label: qsTr("Parameter HDI plot")
		info: qsTr("Displays the posterior mean and highest density interval (HDI) for every pairwise partial association, ordered from smallest to largest posterior mean. One panel per network is shown when multiple networks are estimated.")
		DoubleField
		{
			name:         "parameterHdiPlotCoverage"
			label:        qsTr("HDI coverage:")
			value:        0.95
			min:          0.01
			max:          0.99
			decimals:     2
			info:         qsTr("Coverage of the highest density interval (e.g., 0.95 for a 95% HDI).")
		}
	}
	CheckBox
	{
		name: "coclusteringPlot"
		label: qsTr("Co-clustering matrix plot")
		info: qsTr("Displays a heatmap where each cell shows the posterior proportion of the corresponding pairs of nodes belonging to the same cluster. Only available with the Stochastic block model edge prior (see Prior Specification) and no grouping variable.")
		visible: edgePrior.currentValue === "Stochastic-Block" && groupingVariableSelector.count === 0
	}
	Column
	{
		spacing: 10
		Text
		{
			text: qsTr("Network structure selection")
			font.bold: false
		}
		CheckBox
		{
			name: "posteriorStructurePlot"
			label: qsTr("Posterior structure probability plot")
			info: qsTr("Plots the posterior probability of each sampled graph structure (y-axis) against a structure index (x-axis), sorted in decreasing order of probability. Useful for assessing whether a single graph structure dominates the posterior.")
		}
		CheckBox
		{
			name: "complexityPlot"
			label: qsTr("Posterior complexity probability plot")
			info: qsTr("Plots the summed posterior probability (y-axis) against the number of edges (x-axis). Useful for assessing which network complexities are most supported by the data.")
		}
	}
}

	Group
	{
		title: qsTr("Tables")
		CheckBox
		{
			name: "edgeSpecificOverviewTable"
			label: qsTr("Edge specific overview")
			info: qsTr("Shows one row per edge. Columns include posterior estimates, inclusion probability, inclusion Bayes factor, and convergence statistics (R-hat). Edges are listed in order of decreasing inclusion probability.")
			IntegerField
			{
				name:			"edgeSpecificOverviewInclusionCriteria"
				label:			qsTr("Inclusion criteria: BF\u2081\u2080 > ")
				info: qsTr("Only edges with BF\u2081\u2080 above this value are listed in the edge-specific overview table.")
				min:			1
				defaultValue:	10
				max:			2e2
			}
				CheckBox
			{
				name: "showInterpretativeScaleEstimates"
				label: qsTr("Show estimates on interpretative scale")
				checked: false
				info: qsTr("Shows log-odds, precisios, and partial correlations when available.")
			}
		}
		CheckBox { name: "weightsMatrixTable";	label: qsTr("Partial associations matrix"); info: qsTr("Displays the posterior mean interaction parameters (edge weights) as a symmetric matrix. Rows and columns correspond to variables; off-diagonal entries are the estimated pairwise partial associations.") }
		CheckBox
		{
			name: "edgeEvidenceTable";		label: qsTr("Edge evidence probability table")
			info: qsTr("Displays a symmetric matrix of edge-wise evidence values. All edges are listed regardless of evidence strength.")
			RadioButtonGroup
			{
				name: "evidenceType";
				info: qsTr("Choose which evidence quantity to display for each edge.")
				RadioButton
				{
									value: "inclusionProbability";	label: qsTr("Edge inclusion probability"); checked: true; info: qsTr("Posterior probability that an edge is present, ranging from 0 (no evidence for the edge) to 1 (certain inclusion).") }
					RadioButton { 	value: "BF10"; 					label: qsTr("BF\u2081\u2080"); info: qsTr("Bayes factor quantifying evidence for edge inclusion relative to exclusion. BF\u2081\u2080 > 1 favors inclusion; BF\u2081\u2080 < 1 favors exclusion.")									}
					RadioButton { 	value: "BF01"; 					label: qsTr("BF\u2080\u2081"); info: qsTr("Reciprocal Bayes factor quantifying evidence for edge exclusion relative to inclusion. BF\u2080\u2081 = 1 / BF\u2081\u2080.")									}
					RadioButton { 	value: "log(BF)"; 				label: qsTr("Log(BF\u2081\u2080)"); info: qsTr("Natural logarithm of BF\u2081\u2080.")						}
				}
		}
		CheckBox { name: "centralityTable"; label: qsTr("Centrality table"); info: qsTr("Shows the posterior mean betweenness, closeness, strength, and expected influence for each node. Centrality is computed on the posterior mean network. Only available when no grouping variable is selected.") }

		Group
		{
			title: qsTr("Clustering Overview")
			visible: edgePrior.currentValue === "Stochastic-Block" && groupingVariableSelector.count === 0

			CheckBox
			{
				name: "clusterAllocationsTable"
				label: qsTr("Cluster allocations")
				info: qsTr("Shows the estimated cluster membership for each node. The allocation is summarized as either the posterior mean or posterior mode of the cluster indicator across MCMC samples.")
				DropDown
				{
					name: "clusterAllocationsType"
					label: qsTr("Summary statistic")
					info: qsTr("Posterior mean uses the average cluster index across samples (may be non-integer). Posterior mode uses the most frequently sampled cluster assignment.")
					values: [
						{ value: "mean", label: qsTr("Posterior mean") },
						{ value: "mode", label: qsTr("Posterior mode") }
					]
				}
			}
			CheckBox { name: "posteriorNumBlocksTable";			label: qsTr("Posterior probabilities for the number of clusters"); info: qsTr("Shows the posterior probability for every possible number of clusters.") }
			CheckBox { name: "posteriorCoclusteringMatrixTable";	label: qsTr("Posterior co-clustering matrix"); info: qsTr("Shows the posterior probability that each pair of nodes belongs to the same cluster.") }
			CheckBox
			{
				name: "clusterBayesFactor"
				label: qsTr("Cluster Bayes factor")
				info: qsTr("Computes a Bayes factor comparing hypotheses about the clustering structure.")
				RadioButtonGroup
				{
					name: "clusterBayesFactorType"
					info: qsTr("Clustering vs. no clustering compares the hypothesis of multi-cluster solution against a the hypothesis of a single-cluster (no clustering) model. Point hypotheses compares two specific numbers of clusters.")
					RadioButton { value: "complement";	label: qsTr("Clustering vs. no clustering"); checked: true; info: qsTr("The BF tests whether any clustered structure is more probable than a single-cluster network.") }
					RadioButton
					{
						value: "point"
						label: qsTr("Point hypotheses")
						info: qsTr("The BF compares the posterior probability of exactly H\u2081 clusters versus exactly H\u2082 clusters.")
						IntegerField
						{
							name:			"clusterBayesFactorB1"
							label:			qsTr("H\u2081")
							info: qsTr("Number of clusters under hypothesis H\u2081.")
							defaultValue:	1
							min:			1
							max:			Math.max(1, networkVariables.count)
						}
						IntegerField
						{
							name:			"clusterBayesFactorB2"
							label:			qsTr("H\u2082")
							info: qsTr("Number of clusters under hypothesis H\u2082.")
							defaultValue:	1
							min:			1
							max:			Math.max(1, networkVariables.count)
						}
					}
				}
			}
		}
	}

  Section {
	title: qsTr("Prior Specification")
	Column {
		spacing: 15
		anchors.fill: parent

		Group {
			title: qsTr("Network Structure (Edge) Priors")
			Column {
				spacing: 10
				DropDown
				{
					id: edgePrior
					name: "edgePrior"
					label: qsTr("Edge prior:")
							info: qsTr("Bernoulli: each edge is independently included with a fixed prior probability. Beta-binomial: edges share a random inclusion probability drawn from a Beta distribution, allowing borrowing of information. Stochastic block: inclusion probabilities depend on latent cluster membership, with separate within- and between-cluster parameters. The Stochastic block option is only available without a grouping variable.")
					preferredWidth: 300
					values: groupingVariableSelector.count > 0 ?
						[
							{ value: "Bernoulli",      label: qsTr("Bernoulli")     },
							{ value: "Beta-Bernoulli", label: qsTr("Beta-binomial") }
						] :
						[
							{ value: "Bernoulli",        label: qsTr("Bernoulli")              },
							{ value: "Beta-Bernoulli",   label: qsTr("Beta-binomial")          },
							{ value: "Stochastic-Block", label: qsTr("Stochastic block model") }
						]
				}

				DoubleField
				{
					name: "gPrior"
					label: qsTr("Prior edge inclusion probability:")
					info: qsTr("Prior probability that any given edge is present under the Bernoulli prior. The default of 0.5 assigns equal prior probability to inclusion and exclusion. Values closer to 0 impose a sparser prior.")
					value: 0.5
					min: 0
					max: 1
					inclusive: JASP.MaxOnly
					preferredWidth: 300
					visible: edgePrior.currentValue === "Bernoulli"
			   }

				DoubleField
				{
					name: "betaAlpha"
					label: qsTr("Within cluster shape parameter 1:")
					info: qsTr("First shape parameter (\u03b11) of the Beta prior on the within-cluster edge inclusion probability. Together with shape parameter 2, controls the prior mean (\u03b11 / (\u03b11 + \u03b12)) and concentration. Equal values of 1 correspond to a uniform prior within each cluster.")
					value: 1
					min: 0
					inclusive: JASP.None
					preferredWidth: 300
					visible: edgePrior.currentValue === "Beta-Bernoulli" || edgePrior.currentValue === "Stochastic-Block"
				}

				DoubleField
				{
					name: "betaBeta"
					label: qsTr("Within cluster shape parameter 2:")
					info: qsTr("Second shape parameter (\u03b12) of the Beta prior on the within-cluster edge inclusion probability. Equal values of 1 correspond to a uniform prior.")
					value: 1
					min: 0
					inclusive: JASP.None
					preferredWidth: 300
					visible: edgePrior.currentValue === "Beta-Bernoulli" || edgePrior.currentValue === "Stochastic-Block"
				}
        DoubleField
				{
					name: "betaAlpha_between"
					label: qsTr("Between cluster shape parameter 1:")
					info: qsTr("First shape parameter (\u03b11) of the Beta prior on the between-cluster edge inclusion probability. To encourage sparsity between clusters (block structure), set both between-cluster parameters to values less than 1.")
					value: 1
					min: 0
					inclusive: JASP.None
					preferredWidth: 300
					visible: edgePrior.currentValue === "Stochastic-Block"
				}

				DoubleField
				{
					name: "betaBeta_between"
					label: qsTr("Between cluster shape parameter 2:")
					info: qsTr("Second shape parameter (\u03b12) of the Beta prior on the between-cluster edge inclusion probability.")
					value: 1
					min: 0
					inclusive: JASP.None
					preferredWidth: 300
					visible: edgePrior.currentValue === "Stochastic-Block"
				}

				DoubleField
				{
					name: "lambda"
					label: qsTr("Parameter for the number of clusters:")
					info: qsTr("Rate parameter of the truncated Poisson prior on the number of clusters. Smaller values favor fewer clusters.")
					value: 1
					min: 0
					inclusive: JASP.None
					preferredWidth: 300
					visible: edgePrior.currentValue === "Stochastic-Block"
				}

				DoubleField
				{
					name: "dirichletAlpha"
					label: qsTr("Concentration parameter:")
					info: qsTr("Symmetric Dirichlet concentration parameter for cluster membership probabilities. The default of 1 is roughly uninformative about cluster sizes. Values less than 1 favor unequal cluster sizes.")
					value: 1
					min: 0
					inclusive: JASP.None
					preferredWidth: 300
					visible: edgePrior.currentValue === "Stochastic-Block"
				}

			}
		}

		Group
		{
			title: qsTr("Parameter Priors")


			Column
			{
				spacing: 10
				DoubleField
				{
					name: "interactionScale"
					label: qsTr("Scale of the Cauchy distribution for the partial association parameters:")
					info: qsTr("Scale parameter of the Cauchy prior on the partial association parameters.")
					value: 2.5
					min: 0
					inclusive: JASP.None
					preferredWidth: 300
				}

				DoubleField
				{
					name: "thresholdAlpha"
					label: qsTr("Threshold shape parameter 1:")
					info: qsTr("First shape parameter (\u03b11) of the Beta prior on ordinal category threshold probabilities. The default of 0.5 together with shape parameter 2 = 0.5 corresponds to a Jeffreys prior.")
					value: 0.5
					min: 0
					inclusive: JASP.None
					preferredWidth: 300
				}

				DoubleField
				{
					name: "thresholdBeta"
					label: qsTr("Threshold shape parameter 2:")
					info: qsTr("Second shape parameter (\u03b12) of the Beta prior on ordinal category threshold probabilities.")
					value: 0.5
					min: 0
					inclusive: JASP.None
					preferredWidth: 300
				}
			}
		}
	}
}

	Section
	{
		title: qsTr("Sampling Options")
		Layout.columnSpan: 2
		IntegerField { name: "burnin";	label: qsTr("Burn in: ");		value: 1000;	min: 0; 				max: iter.value / 2;	fieldWidth: 100; id: burnin; info: qsTr("Number of warmup iterations discarded from the start of each chain to allow the Markov chain to converge before collecting posterior samples. Must be less than half the total iterations.")	}
		IntegerField { name: "iter";		label: qsTr("Iterations: ");	value: 2000;	min: burnin.value * 2; 								fieldWidth: 100; id: iter; info: qsTr("Total number of MCMC iterations per chain, including the burn-in. Posterior inference uses the iterations after burn-in. Increase for more stable estimates, especially for complex models.")	}

		Group
		{
			title: qsTr("Advanced Options")

			DropDown
			{
				name: "chains"
				label: qsTr("Number of chains")
				info: qsTr("Number of independent MCMC chains. With 2 chains, the Gelman-Rubin R-hat convergence statistic is computed from the two chains. With 1 chain, a split-chain version is used instead. R-hat values above approximately 1.05 suggest the chains have not converged.")
				indexDefaultValue: 1
				values: [
					{ value: "1",	label: "1" },
					{ value: "2",	label: "2" }
				]
			}

			DropDown
			{
				name: "omrfUpdateMethod"
				label: qsTr("Update method")
				info: qsTr("MCMC algorithm used to sample from the posterior.")
				values: [
					{ value: "adaptive-metropolis",	label: qsTr("Adaptive Metropolis")	},
					{ value: "hamiltonian-mc",		label: qsTr("Hamiltonian MC")		},
					{ value: "nuts",				label: qsTr("NUTS")				}
				]
			}
		}

		SetSeed{}
	}


  Section
	{
		title: qsTr("Graphical Options")

		InputListView
		{
			id					: networkFactors
			name				: "manualColorGroups"
			title				: qsTr("Group name")
			info				: qsTr("Define named groups for manual node coloring. Each group can be assigned a color that will appear in the network plot when Manual colors is enabled.")
			optionKey			: "name"
			defaultValues		: [qsTr("Group 1"), qsTr("Group 2")]
			placeHolder			: qsTr("New Group")
			minRows				: 2
			preferredWidth		: (2 * form.width) / 5
			rowComponentTitle				: manualColor.checked ? qsTr("Group color") : ""
			rowComponent: DropDown
			{
				name: "color"
				visible: manualColor.checked
				values: [
					{ label: qsTr("red")	, value: "red"		},
					{ label: qsTr("blue")	, value: "blue"		},
					{ label: qsTr("yellow")	, value: "yellow"	},
					{ label: qsTr("green")	, value: "green"	},
					{ label: qsTr("purple")	, value: "purple"	},
					{ label: qsTr("orange") , value: "orange"	}
				]
			}
		}

		AssignedVariablesList
		{
			Layout.fillWidth				: true
			Layout.leftMargin				: 40
			preferredWidth					: (2 * form.width) / 5
			title							: qsTr("Variables in network")
			name							: "colorGroupVariables"
								info								: qsTr("Assign each network variable to one of the color groups defined on the left. All variables start in Group 1 by default.")
			source							: ["variables"]
			addAvailableVariablesToAssigned	: true
			draggable						: false
			rowComponentTitle				: qsTr("Group")
			rowComponent: DropDown
			{
				name: "group"
				source: ["manualColorGroups"]
			}
		}

		Group
		{
			Layout.columnSpan: 2
			CheckBox	{ name: "manualColor";	label: qsTr("Manual colors");	id: manualColor; info: qsTr("When enabled, node colors are taken from the group color assignments above. When disabled, a predefined palette is applied uniformly.")	}
			DropDown
			{
				enabled: !manualColor.checked
				id: paletteSelector
				name: "nodePalette"
				label: qsTr("Node palette")
				info: qsTr("Color palette applied to node groups when Manual colors is disabled. Colorblind-safe options are recommended for accessible figures.")
				indexDefaultValue: 1
				values: [
					{ label: qsTr("Rainbow"),		value: "rainbow"	},
					{ label: qsTr("Colorblind"),	value: "colorblind"	},
					{ label: qsTr("Pastel"),		value: "pastel"		},
					{ label: qsTr("Gray"),			value: "gray"		},
					{ label: qsTr("R"),				value: "R"			},
					{ label: qsTr("ggplot2"),		value: "ggplot2"	}
				]
			}
			DoubleField	{ name: "nodeSize";		label: qsTr("Node size");		defaultValue: 1; max: 10; info: qsTr("Multiplier applied to all node sizes. A value of 1 uses the default size; values above 1 create larger nodes.")	}
		}

		Group
		{
			title: qsTr("Edges")
			DoubleField { name: "edgeSize";			label: qsTr("Edge size");			defaultValue: 1; info: qsTr("Multiplier applied to all edge widths. A value of 2 doubles the edge width relative to the default.") }
			DoubleField { name: "maxEdgeStrength";	label: qsTr("Max edge strength");	defaultValue: 0; max: 10; info: qsTr("Sets the edge weight that corresponds to the maximum line width. Edges stronger than this value are clipped to the maximum width. Set to 0 to scale all edges relative to the strongest edge.") }
			DoubleField { name: "minEdgeStrength";	label: qsTr("Min edge strength");	defaultValue: 0; max: 10; info: qsTr("Edges with absolute weight below this value are hidden in the network plot. If the threshold exceeds the strongest edge, it is ignored and a warning is shown in the summary table.") }
			DoubleField { name: "cut";				label: qsTr("Cut");					defaultValue: 0; max: 10; info: qsTr("Edges with absolute weight below this value are drawn thin and desaturated; above it they scale to full width and saturation. Set to 0 to scale all edges continuously without a cut threshold.") }
			CheckBox	{ name: "details";			label: qsTr("Show details"); info: qsTr("Overlays the minimum edge strength, maximum edge strength, and cut values as text on the network plot.") }
			CheckBox
			{
								name: "edgeLabels";			label: qsTr("Edge labels");				checked: false; info: qsTr("Overlay the numerical edge weight on each edge in the network plot.")
				DoubleField {	name: "edgeLabelSize";		label: qsTr("Edge label size");			min: 0;		max: 10;	defaultValue: 1; info: qsTr("Multiplier for edge label size.")		}
				DoubleField {	name: "edgeLabelPosition";	label: qsTr("Edge label position");		min: 0;		max: 1;		defaultValue: 0.5; info: qsTr("Position of edge labels along each edge (0 to 1).")	}
			}

			DropDown
			{
				name: "edgePalette"
				label: qsTr("Edge palette")
				info: qsTr("Color scheme applied to positive and negative edges. Classic uses blue for positive and red for negative associations. Colorblind-safe and grayscale options are also available.")
				indexDefaultValue: 1
				values:
				[
					{ label: qsTr("Classic"),		value: "classic"		},
					{ label: qsTr("Colorblind"),	value: "colorblind"		},
					{ label: qsTr("Gray"),			value: "gray"			},
					{ label: qsTr("Hollywood"),		value: "Hollywood"		},
					{ label: qsTr("Borkulo"),		value: "Borkulo"		},
					{ label: qsTr("TeamFortress"),	value: "TeamFortress"	},
					{ label: qsTr("Reddit"),		value: "Reddit"			},
					{ label: qsTr("Fried"),			value: "Fried"			}
				]
			}
		}

		Group
		{
			title: qsTr("Labels")
			DoubleField { name: "labelSize";	label: qsTr("Label size");		defaultValue: 1; max: 10; info: qsTr("Multiplier for node label font size. A value of 2 doubles the label size relative to the default.") }
			CheckBox	{ name: "labelScale";	label: qsTr("Scale label size");	checked: true; info: qsTr("When enabled, label font size is automatically scaled relative to node size so that labels fit inside nodes.") }
			CheckBox
			{
				name: "labelAbbreviation"; label: qsTr("Abbreviate labels to ")
				info: qsTr("Abbreviate variable names in plots to reduce label clutter.")
				childrenOnSameRow: true
				IntegerField { name: "labelAbbreviationLength"; defaultValue: 4; min: 1; max: 100000; info: qsTr("Target character length for abbreviated labels.") }
			}
		}

		RadioButtonGroup
		{
			name: "variableNamesShown";
			title: qsTr("Show Variable Names")
			info: qsTr("Choose where variable names are displayed in network plots.")
			RadioButton { value: "inNodes";			label: qsTr("In plot");	 checked: true; info: qsTr("Variable names are shown as labels directly on the nodes.")	}
			RadioButton { value: "inLegend";		label: qsTr("In legend"); info: qsTr("Nodes are labeled with numbers; a legend maps each number to the variable name. Useful when variable names are long.")				}
		}

		RadioButtonGroup
		{
			name: "legend"
			title: qsTr("Legend")
			info: qsTr("Controls legend placement across network plots. When multiple networks are shown (e.g., with grouping), this setting applies globally.")
			RadioButton { value: "hide";		label: qsTr("No legend"); info: qsTr("No legend is shown in any plot.")				}
			RadioButton { value: "allPlots";	label: qsTr("All plots"); checked: true; info: qsTr("A legend is added to every network plot.")	}
			RadioButton
			{
				value: "specificPlot"; label: qsTr("In plot number: ")
				info: qsTr("A legend is added to only the plot with the specified number, and other plots are shown without a legend.")
				childrenOnSameRow: true
				IntegerField { name: "legendSpecificPlotNumber"; defaultValue: 1; info: qsTr("1-based index of the plot in which the legend should appear.") }
			}
			DoubleField
			{
				name: "legendToPlotRatio"
				label: qsTr("Legend to plot ratio")
				info: qsTr("Width of the legend panel relative to the network plot. A value of 0.4 means the legend is 40% as wide as the plot.")
				defaultValue: 0.4
				min: 0.001
				max: 4 // not strictly necessary but png crashes if it gets too big
			}
		}

		RadioButtonGroup
		{
			name: "layout"
			title: qsTr("Layout")
			info: qsTr("Determines how nodes are positioned in network plots. The same layout is applied to all networks in a multi-network analysis, computed from the average of estimated edge weights.")
			RadioButton
			{
				value: "spring"; label: qsTr("Spring"); checked: true
				info: qsTr("Nodes are positioned using the Fruchterman-Reingold force-directed algorithm. Strongly connected nodes are placed closer together.")
				childrenOnSameRow: true
				DoubleField { name: "layoutSpringRepulsion"; label: qsTr("Repulsion"); defaultValue: 1; max: 10; info: qsTr("Controls how strongly nodes repel each other. Larger values spread nodes further apart.") }
			}
			RadioButton { value: "circle";	label: qsTr("Circle"); info: qsTr("Nodes are evenly spaced on a circle. Useful for comparing relative edge patterns without the layout reflecting association strength.")							}
		}

		Group
		{
			title: qsTr("Measures shown in centrality plot")
			enabled: centralityPlot.checked
			CheckBox	{	name: "betweenness";		label: qsTr("Betweenness");			checked: true; info: qsTr("Number of shortest paths between other node pairs that pass through this node. Nodes with high betweenness act as bridges in the network.")	}
			CheckBox	{	name: "closeness";			label: qsTr("Closeness");			checked: true; info: qsTr("Inverse of the average shortest path length from this node to all other nodes. Nodes with high closeness can efficiently reach all other nodes.")	}
			CheckBox	{	name: "strength";			label: qsTr("Strength");			checked: true; info: qsTr("Sum of absolute edge weights connected to this node. Reflects how strongly a node is associated with its neighbors.")	}
			CheckBox	{	name: "expectedInfluence";	label: qsTr("Expected influence");	checked: true; info: qsTr("Sum of signed edge weights connected to this node. Unlike strength, negative edges reduce the value, so nodes with mixed positive and negative connections may have low expected influence.")	}
		}
	}
}
