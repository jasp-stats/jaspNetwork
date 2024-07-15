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

import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0


Form
{

	VariablesForm
	{
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "variables";			title: qsTr("Dependent Variables"); allowedColumns: ["ordinal", "scale"]; allowTypeChange: true; id: networkVariables}
		AssignedVariablesList { name: "groupingVariable";	title: qsTr("Split"); singleVariable: true; allowedColumns: [ "nominal"] }
	}

	DropDown
	{
		id: estimator
		name: "estimator"
		label: qsTr("Estimator")
		Layout.columnSpan: 2
		values: [
			{ value: "ebicGlasso",		label: "EBICglasso"			},
			{ value: "cor",				label: qsTr("Correlation")		},
			{ value: "pcor",			label: qsTr("Partial Correlation")},
			{ value: "isingFit",		label: "IsingFit"			},
			{ value: "isingSampler",	label: "IsingSampler"		},
			{ value: "huge",			label: qsTr("huge")			},
//			{ value: "adalasso",		label: "adalasso"			},	// no longer available due to removal of parcor from CRAN
			{ value: "mgm",				label: "mgm"				}
		]
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox { name: "networkPlot";		label: qsTr("Network plot")								}
		CheckBox { name: "centralityPlot";	label: qsTr("Centrality plot");		id: centralityPlot	}
		CheckBox { name: "clusteringPlot";	label: qsTr("Clustering plot")							}
	}

	Group
	{
		title: qsTr("Tables")
		CheckBox { name: "centralityTable";		label: qsTr("Centrality table")	}
		CheckBox { name: "clusteringTable";		label: qsTr("Clustering table")	}
		CheckBox { name: "weightsMatrixTable";	label: qsTr("Weights matrix")	}
	}

	Section
	{
		title: qsTr("Analysis Options - ") + estimator.currentText

		RadioButtonGroup
		{
			name: "correlationMethod"
			title: qsTr("Correlation Method")
			visible: [0, 1, 2].includes(estimator.currentIndex)
			RadioButton { value: "auto";	label: qsTr("Auto"); checked: true	}
			RadioButton { value: "cor";		label: qsTr("Cor")					}
			RadioButton { value: "cov";		label: qsTr("Cov")					}
			RadioButton { value: "npn";		label: qsTr("Npn")					}
		}

		RadioButtonGroup
		{
			name: "centralityNormalization"
			title: qsTr("Centrality Measures")
			visible: estimator.currentIndex === 0
			RadioButton { value: "normalized";	label: qsTr("Normalized"); checked: true }
			RadioButton { value: "relative" ;	label: qsTr("Relative")					}
			RadioButton { value: "raw";			label: qsTr("Raw")						}
		}

		Group
		{
			title: qsTr("Network")
			visible: estimator.currentIndex === 0
			CheckBox { name: "weightedNetwork"; label: qsTr("Weighted"); checked: true	}
			CheckBox { name: "signedNetwork";	label: qsTr("Signed");	checked: true	}
		}

		RadioButtonGroup
		{
			name: "missingValues"
			title: qsTr("Missing Values")
			visible: [0, 1, 2].includes(estimator.currentIndex)
			RadioButton { value: "pairwise";	label: qsTr("Exclude pairwise"); checked: true	}
			RadioButton { value: "listwise";	label: qsTr("Exclude listwise")					}
		}

		RadioButtonGroup
		{
			name: "sampleSize"
			title: qsTr("Sample Size")
			visible: estimator.currentIndex === 0
			RadioButton { value: "maximum";	label: qsTr("Maximum"); checked: true	}
			RadioButton { value: "minimim";	label: qsTr("Minimum")					}
		}

		RadioButtonGroup
		{
			name: "isingEstimator"
			title: qsTr("Ising Estimator")
			visible: estimator.currentIndex === 4
			RadioButton { value: "pseudoLikelihood";		label: qsTr("Pseudo-likelihood"); checked: true	}
			RadioButton { value: "univariateRegressions";	label: qsTr("Univariate regressions")			}
			RadioButton { value: "bivariateRegressions";	label: qsTr("Bivariate regressions")			}
			RadioButton { value: "logLinear";				label: qsTr("Loglinear")						}
		}

		RadioButtonGroup
		{
			name: "criterion"
			title: qsTr("Criterion")
			visible: [5, 6].includes(estimator.currentIndex)
			RadioButton { value: "ebic";	label: qsTr("EBIC"); checked: true	}
			RadioButton { value: "ric";		label: qsTr("RIC")					}
			RadioButton { value: "stars";	label: qsTr("STARS")				}
			RadioButton { value: "cv";		label: qsTr("CV")					}
		}

		RadioButtonGroup
		{
			name: "rule"
			title: qsTr("Rule")
			visible: [3, 6].includes(estimator.currentIndex)
			RadioButton { value: "and";	label: qsTr("AND"); checked: true	}
			RadioButton { value: "or";	label: qsTr("OR")					}
		}

		RadioButtonGroup
		{
			name: "split"
			title: qsTr("Split")
			visible: [3, 4].includes(estimator.currentIndex)
			RadioButton { value: "median";	label: qsTr("Median"); checked: true	}
			RadioButton { value: "mean";	label: qsTr("Mean")						}
		}

		Group
		{
			title: qsTr("Tuning Parameter")
			visible: [0, 3, 5, 7].includes(estimator.currentIndex)
			DoubleField { name: "tuningParameter"; label: qsTr("Value"); defaultValue: 0.5; max: 1 }
		}

		RadioButtonGroup
		{
			name: "thresholdBox"
			title: qsTr("Threshold")
			visible: [1, 2].includes(estimator.currentIndex)
			RadioButton
			{
				value: "value";	label: qsTr("Value"); checked: true
				childrenOnSameRow: true
				DoubleField { name: "thresholdValue"; defaultValue: 0; max: 1000000000 }
			}
			RadioButton
			{
				value: "method"; label: qsTr("Method")
				childrenOnSameRow: true
				DropDown
				{
					name: "thresholdMethod"
					values: [
						{ label: qsTr("Significant"),	value: "sig"		},
						{ label: "Bonferroni",	value: "bonferroni"	},
						{ label: "Locfdr",		value: "locfdr"		},
						{ label: "Holm",		value: "holm"		},
						{ label: "Hochberg",	value: "hochberg"	},
						{ label: "Hommel",		value: "hommel"		},
						{ label: "BH",			value: "BH"			},
						{ label: "BY",			value: "BY"			},
						{ label: "fdr",			value: "fdr"		}
					]
				}
			}
		}

		Group
		{
			title: qsTr("Cross-validation")
			visible: [6].includes(estimator.currentIndex)
			IntegerField { name: "nFolds"; label: qsTr("nFolds"); min: 3; max: 100000; fieldWidth: 60; defaultValue: 3 }
		}

		VariablesForm
		{
			visible: [6].includes(estimator.currentIndex)
			AvailableVariablesList
			{
				title: qsTr("Variables in network")
				name: "variablesTypeAvailable"
				source: ["variables"]
			}

			AssignedVariablesList { name: "mgmContinuousVariables";		title: qsTr("Continuous Variables");	allowedColumns: ["scale"]}
			AssignedVariablesList { name: "mgmCategoricalVariables";	title: qsTr("Categorical Variables");	allowedColumns: ["ordinal"]}
			AssignedVariablesList { name: "mgmCountVariables";			title: qsTr("Count Variables");			allowedColumns: ["nominal"]}
		}
	}

	Section
	{
		title: qsTr("Bootstrap Options")

		Group
		{
			title: qsTr("Settings")
			CheckBox	 { name: "bootstrap";		label: qsTr("Bootstrap network")	}
			IntegerField { name: "bootstrapSamples";	label: qsTr("Number of bootstraps"); defaultValue: 0; max: 100000 }

			CheckBox	 { name: "bootstrapParallel";	label: qsTr("Parallel Bootstrap");	checked: false;	visible: false }
		}

		RadioButtonGroup
		{
			name: "bootstrapType"
			title: qsTr("Bootstrap Type")
			Layout.rowSpan: 2
			RadioButton { value: "nonparametric";	label: qsTr("Nonparametric"); checked: true	}
			RadioButton { value: "case";			label: qsTr("Case")							}
			RadioButton { value: "node";			label: qsTr("Node")							}
			RadioButton { value: "parametric";		label: qsTr("Parametric")					}
			RadioButton { value: "person";			label: qsTr("Person")						}
			RadioButton { value: "jackknife";		label: qsTr("Jackknife")					}
		}

		Group
		{
			title: qsTr("Statistics")
			CheckBox { name: "statisticsEdges";			label: qsTr("Edges");		checked: true }
			CheckBox { name: "statisticsCentrality";	label: qsTr("Centrality");	checked: true }
		}
	}

	Section
	{
		title: qsTr("Graphical Options")

		InputListView
		{
			id					: networkFactors
			name				: "manualColorGroups"
			title				: qsTr("Group name")
			optionKey			: "name"
			defaultValues		: [qsTr("Group 1"), qsTr("Group 2")]
			placeHolder			: qsTr("New Group")
			minRows				: 2
			preferredWidth		: (2 * form.width) / 5
			rowComponentTitle	: manualColor.checked ? qsTr("Group color") : ""
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
			preferredWidth					: (2 * form.width) / 5
			Layout.fillWidth				: true
			Layout.leftMargin				: 40
			title							: qsTr("Variables in network")
			name							: "colorGroupVariables"
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
			CheckBox	{ name: "manualColor";	label: qsTr("Manual colors");	id: manualColor	}
			DropDown
			{
				enabled: !manualColor.checked
				id: paletteSelector
				name: "nodePalette"
				label: qsTr("Node palette")
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
			DoubleField	{ name: "nodeSize";		label: qsTr("Node size");		defaultValue: 1; max: 10	}
		}

		Group
		{
			title: qsTr("Edges")
			DoubleField { name: "edgeSize";			label: qsTr("Edge size");			defaultValue: 1 }
			DoubleField { name: "maxEdgeStrength";	label: qsTr("Max edge strength");	defaultValue: 0; id: maxEdgeStrength; min: minEdgeStrength.value;	max: 100					}
			DoubleField { name: "minEdgeStrength";	label: qsTr("Min edge strength");	defaultValue: 0; id: minEdgeStrength; min: -100;					max: maxEdgeStrength.value	}
			DoubleField { name: "cut";				label: qsTr("Cut");					defaultValue: 0; max: 10 }
			CheckBox	{ name: "details";			label: qsTr("Show details") }
			CheckBox
			{
								name: "edgeLabels";			label: qsTr("Edge labels");				checked: false
				DoubleField {	name: "edgeLabelSize";		label: qsTr("Edge label size");			min: 0;			max: 10;	defaultValue: 1		}
				DoubleField {	name: "edgeLabelPosition";	label: qsTr("Edge label position");		min: 0;			max: 1;		defaultValue: 0.5	}
			}

			DropDown
			{
				name: "edgePalette"
				label: qsTr("Edge palette")
				indexDefaultValue: 1
				values:
				[
					{ label: qsTr("Classic"),		value: "classic"		},
					{ label: qsTr("Colorblind"),	value: "colorblind"		},
					{ label: qsTr("Gray"),			value: "gray"			},
					{ label: qsTr("Hollywood"),		value: "hollywood"		},
					{ label: qsTr("Borkulo"),		value: "borkulo"		},
					{ label: qsTr("TeamFortress"),	value: "teamFortress"	},
					{ label: qsTr("Reddit"),		value: "reddit"			},
					{ label: qsTr("Fried"),			value: "fried"			}
				]
			}
		}

		Group
		{
			title: qsTr("Labels")
			DoubleField { name: "labelSize";	label: qsTr("Label size");		defaultValue: 1; max: 10 }
			CheckBox	{ name: "labelScale";	label: qsTr("Scale label size");	checked: true }
			CheckBox
			{
				name: "labelAbbreviation"; label: qsTr("Abbreviate labels to ")
				childrenOnSameRow: true
				IntegerField { name: "labelAbbreviationLength"; defaultValue: 4; min: 1; max: 100000 }
			}
		}

		RadioButtonGroup
		{
			name: "variableNamesShown";
			title: qsTr("Show Variable Names")
			RadioButton { value: "inNodes";			label: qsTr("In plot");	 checked: true	}
			RadioButton { value: "inLegend";		label: qsTr("In Legend")				}
		}

		RadioButtonGroup
		{
			name: "mgmVariableTypeShown";
			title: qsTr("Show Variable Type")
			visible: [7].includes(estimator.currentIndex)
			RadioButton { value: "hide";		label: qsTr("Don't show")						}
			RadioButton { value: "nodeColor";	label: qsTr("Using node color")					}
			RadioButton { value: "nodeShape";	label: qsTr("Using node shape"); checked: true	}
		}

		RadioButtonGroup
		{
			name: "legend"
			title: qsTr("Legend")
			RadioButton { value: "hide";			label: qsTr("No legend")					}
			RadioButton { value: "allPlots";		label: qsTr("All plots"); checked: true	}
			RadioButton
			{
				value: "specificPlot"; label: qsTr("In plot number: ")
				childrenOnSameRow: true
				IntegerField { name: "legendSpecificPlotNumber"; defaultValue: 1 }
			}
			DoubleField
			{
				name: "legendToPlotRatio"
				label: qsTr("Legend to plot ratio")
				defaultValue: 0.4
				min: 0.001
				max: 4 // not strictly necessary but png crashes if it gets too big
			}
		}

		RadioButtonGroup
		{
			name: "layout"
			title: qsTr("Layout")
			CheckBox { name: "layoutNotUpdated"; label: qsTr("Do not update layout") }
			RadioButton
			{
				value: "spring"; label: qsTr("Spring"); checked: true
				childrenOnSameRow: true
				DoubleField { name: "layoutSpringRepulsion"; label: qsTr("Repulsion"); defaultValue: 1; max: 10 }
			}
			RadioButton { value: "circle";	label: qsTr("Circle")							}
			RadioButton { value: "data";	label: qsTr("Data");	id: dataRatioButton		}
		}

		Group
		{
			title: qsTr("Measures shown in centrality plot")
			enabled: centralityPlot.checked
			CheckBox	{	name: "betweenness";		label: qsTr("Betweenness");			checked: true	}
			CheckBox	{	name: "closeness";			label: qsTr("Closeness");			checked: true	}
			CheckBox	{	name: "strength";			label: qsTr("Strength");			checked: true	}
			CheckBox	{	name: "expectedInfluence";	label: qsTr("Expected Influence");	checked: true	}
		}

		VariablesForm
		{
			visible: dataRatioButton.checked
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList	{ name: "allXYVariables" }
			AssignedVariablesList	{ name: "layoutX"; title: qsTr("x"); singleVariable: true; allowedColumns: "nominal"}
			AssignedVariablesList	{ name: "layoutY"; title: qsTr("y"); singleVariable: true; allowedColumns: "nominal"}
		}

		CheckBox
		{
			text: qsTr("Save the layout in the data set")
			name: "layoutSavedToData"
			Layout.columnSpan: 2
			ComputedColumnField { name: "computedLayoutX"; text: qsTr("name for x-coordinates"); id: layoutX }
			ComputedColumnField { name: "computedLayoutY"; text: qsTr("name for y-coordinates"); id: layoutY }
			enabled: !dataRatioButton.checked
			visible: !dataRatioButton.checked
			id: layoutCheckbox
			onCheckedChanged:
			{
				if (!layoutCheckbox.checked)
				{
//					The user no longer wants to add the layout to the dataset so we remove it from the dataset if it was there.
					layoutX.value = ""
					layoutY.value = ""
					layoutX.doEditingFinished()
					layoutY.doEditingFinished()
				}
			}
		}
	}
}
