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
import JASP.Widgets 1.0

Form
{

	VariablesForm
	{
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "variables";			title: qsTr("Dependent Variables"); suggestedColumns: ["scale"]; id: networkVariables}
		AssignedVariablesList { name: "groupingVariable";	title: qsTr("Split"); singleVariable: true; suggestedColumns: ["ordinal", "nominal"] }
	}
	
	DropDown
	{
		id: estimator
		name: "estimator"
		label: qsTr("Estimator")
		Layout.columnSpan: 2
		values: [
			{ value: "ggm",		        label: "ggm"			},
			{ value: "gcgm",				  label: "mgm"				}
		]
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox { name: "plotNetwork";		label: qsTr("Network plot")								}
		CheckBox { 
		  name: "plotEvidence";		label: qsTr("Edge Evidence plot")
				  CheckBox { 
				        name:    "edgeInclusion";
				        label:   qsTr("Evidence for inclusion");
				        checked: true 
				        IntegerField {
				          name:         "edgeInclusionCriteria";
				          label:        qsTr("Inclusion criteria: BF\u2081\u2080 > ");
				          min:          1;
				          defaultValue: 10;
				          max:          2e2
				        }
				  }
				  CheckBox { name: "edgeExclusion";  label: qsTr("Evidence for exclusion"); checked: true }
				  CheckBox { name: "edgeAbsence"; label: qsTr("Absence of evidence");   checked: true }
		}
		CheckBox { name: "plotCentrality";  label: qsTr("Centrality plot")          }
	}

	Group
	{
		title: qsTr("Tables")
		CheckBox { name: "tableWeightsMatrix";	label: qsTr("Weights matrix")	}
		CheckBox { 
		    name: "tableEdgeEvidence";		label: qsTr("Edge Evidence Probability table")
		    RadioButtonGroup {
				  name: "evidenceType";
		      RadioButton { value: "inclusionProbability"; label: qsTr("Edge Inclusion probability") ; checked: true }
				  RadioButton { value: "BF10"; label: qsTr("BF\u2081\u2080") }
				  RadioButton { value: "BF01"; label: qsTr("BF\u2080\u2081") }
				  RadioButton { value: "log(BF)"; label: qsTr("Log(BF\u2081\u2080)") }
				}
		}
//		CheckBox { name: "tableCentrality"; label: qsTr("Centrality table") }
	}
	
	Section 
	{
	  title: qsTr("Options")
	  Layout.columnSpan: 2
	  IntegerField { name: "burnin"; label: qsTr("Burn in: "); value: "5000" ; min: 0; max: iter.value / 2; fieldWidth: 100; id: burnin }
	  IntegerField { name: "iter"; label: qsTr("Iterations: "); value: "10000" ; min: burnin.value * 2; fieldWidth: 100; id: iter }
	  
	  RadioButtonGroup
		{
			name: "missingValues"
			title: qsTr("Missing Values")
			visible: [0, 1, 2].includes(estimator.currentIndex)
			RadioButton { value: "pairwise";	label: qsTr("Exclude pairwise"); checked: true	}
			RadioButton { value: "listwise";	label: qsTr("Exclude listwise")					}
		}
		
		SetSeed{}
	}

	Section
	{
		title: qsTr("Prior")

		FormulaField { name: "gprior"; label: qsTr("Prior edge inclusion (g prior): "); value: "0.5" ; min: 0.001; max: 1; Layout.columnSpan: 2 }
		
		DropDown
	  {
		  id: initialConfiguration
		  name: "initialConfiguration"
		  label: qsTr("Initial configuration prior edge inclusion (g start):")
		  Layout.columnSpan: 2
		  values: [
			  { value: "empty",		      label: "empty"			  },
			  { value: "full",				  label: "full"				  }
		  ]
	  }

		IntegerField { name: "dfprior"; label: qsTr("Degrees of freedom of G-Wishert prior (df prior): "); value: "3" ; min: 0; Layout.columnSpan: 2 }

	}


Section
	{
		title: qsTr("Graphical Options")

		InputListView
		{
			id					: networkFactors
			name				: "groupNames"
			title				: qsTr("Group name")
			optionKey			: "group"
			defaultValues		: ["Group 1", "Group 2"]
			placeHolder			: qsTr("New Group")
			minRows				: 2
			preferredWidth		: (2 * form.width) / 5
			enabled				: manualColors.checked
			rowComponentTitle	: qsTr("Group color")
			rowComponent: DropDown
			{
				name: "groupColors"
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
			title							: qsTr("Variables in network")
			name							: "variablesForColor"
			source							: ["variables"]
			addAvailableVariablesToAssigned	: true
			draggable						: false
			rowComponentTitle				: qsTr("Group")
			rowComponent: DropDown
			{
				name: "groupAssigned"
				source: ["groupNames"]
			}
		}

		Group
		{
			Layout.columnSpan: 2
			CheckBox	{ name: "manualColors";	label: qsTr("Manual colors");	id: manualColors	}
			DropDown
			{
				enabled: !manualColors.checked
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
			DoubleField { name: "maxEdgeStrength";	label: qsTr("Max edge strength");	defaultValue: 0; max: 10 }
			DoubleField { name: "minEdgeStrength";	label: qsTr("Min edge strength");	defaultValue: 0; max: 10 }
			DoubleField { name: "cut";				label: qsTr("Cut");					defaultValue: 0; max: 10 }
			CheckBox	{ name: "showDetails";		label: qsTr("Show details") }
			CheckBox
			{
								name: "edgeLabels";			label: qsTr("Edge labels");				checked: false
				DoubleField {	name: "edgeLabelCex";		label: qsTr("Edge label size");			min: 0;			max: 10;	defaultValue: 1		}
				DoubleField {	name: "edgeLabelPosition";	label: qsTr("Edge label position");		min: 0;			max: 1;		defaultValue: 0.5	}
			}

			DropDown
			{
				name: "edgeColors"
				label: qsTr("Edge palette")
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
			DoubleField { name: "labelSize";	label: qsTr("Label size");		defaultValue: 1; max: 10 }
			CheckBox	{ name: "scaleLabels";	label: qsTr("Scale label size");	checked: true }
			CheckBox
			{
				name: "abbreviateLabels"; label: qsTr("Abbreviate labels to ")
				childrenOnSameRow: true
				IntegerField { name: "abbreviateNoChars"; defaultValue: 4; min: 1; max: 100000 }
			}
		}

		RadioButtonGroup
		{
			name: "showVariableNames";
			title: qsTr("Show Variable Names")
			RadioButton { value: "In nodes";			label: qsTr("In plot");	 checked: true	}
			RadioButton { value: "In legend";		label: qsTr("In legend")					}
		}
		
		RadioButtonGroup
		{
			name: "showLegend"
			title: qsTr("Legend")
			RadioButton { value: "No legend";		label: qsTr("No legend")					}
			RadioButton { value: "All plots";		label: qsTr("All plots"); checked: true	}
			RadioButton
			{
				value: "In plot number: "; label: qsTr("In plot number: ")
				childrenOnSameRow: true
				IntegerField { name: "legendNumber"; defaultValue: 1 }
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
//			CheckBox { name: "keepLayoutTheSame"; label: qsTr("Do not update layout") }
			RadioButton
			{
				value: "spring"; label: qsTr("Spring"); checked: true
				childrenOnSameRow: true
				DoubleField { name: "repulsion"; label: qsTr("Repulsion"); defaultValue: 1; max: 10 }
			}
			RadioButton { value: "circle";	label: qsTr("Circle")							}
//			RadioButton { value: "data";	label: qsTr("Data");	id: dataRatioButton		}
		}

//		Group
//		{
//			title: qsTr("Measures shown in centrality plot")
//			enabled: plotCentrality.checked
//			CheckBox	{	name: "Betweenness";		label: qsTr("Betweenness");			checked: true	}
//			CheckBox	{	name: "Closeness";			label: qsTr("Closeness");			checked: true	}
//			CheckBox	{	name: "Degree";				label: qsTr("Betweenness");			checked: true	}
//			CheckBox	{	name: "ExpectedInfluence";	label: qsTr("Expected Influence");	checked: true	}
//		}

//		VariablesForm
//		{
//			visible: dataRatioButton.checked
//			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
//			AvailableVariablesList	{ name: "allXYVariables" }
//			AssignedVariablesList	{ name: "layoutX"; title: qsTr("x"); singleVariable: true; suggestedColumns: "nominalText"}
//			AssignedVariablesList	{ name: "layoutY"; title: qsTr("y"); singleVariable: true; suggestedColumns: "nominalText"}
//		}

//		CheckBox
//		{
//			text: qsTr("Save the layout in the data set")
//			name: "addLayoutToData"
//			Layout.columnSpan: 2
//			ComputedColumnField { name: "computedLayoutX"; text: qsTr("name for x-coordinates"); id: layoutX }
//			ComputedColumnField { name: "computedLayoutY"; text: qsTr("name for y-coordinates"); id: layoutY }
//			enabled: !dataRatioButton.checked
//			visible: !dataRatioButton.checked
//			id: layoutCheckbox
//			onCheckedChanged:
//			{
//				if (!layoutCheckbox.checked)
//				{
//					The user no longer wants to add the layout to the dataset so we remove it from the dataset if it was there.
//					layoutX.value = ""
//					layoutY.value = ""
//					layoutX.doEditingFinished()
//					layoutY.doEditingFinished()
//				}
//			}
//		}
	}
	
	
	Section
	{
		title:		qsTr("Network structure selection")
		expanded:	false
		
//		CheckBox { name: "plotStructure";		          label: qsTr("Structure plot")				    }
		CheckBox { name: "plotPosteriorStructure";		label: qsTr("Posterior Structure Probability plot")	}
		CheckBox { name: "plotComplexity";		        label: qsTr("Posterior Complexity Probability plot")	}
	}
}
