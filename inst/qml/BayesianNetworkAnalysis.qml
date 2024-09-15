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
		AssignedVariablesList { name: "variables";			title: qsTr("Dependent Variables"); allowedColumns: ["scale"]; id: networkVariables}
		AssignedVariablesList { name: "groupingVariable";	title: qsTr("Split"); singleVariable: true; allowedColumns: ["nominal"] }
	}
	
	DropDown
	{
		id: estimator
		name: "estimator"
		label: qsTr("Estimator")
		Layout.columnSpan: 2
		values: [
			{ value: "ggm",		        label: "ggm"			},
			{ value: "gcgm",				  label: "gcgm"			}
		]
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox { name: "networkPlot";		label: qsTr("Network plot")								}
		CheckBox { 
		  name: "evidencePlot";		
		  label: qsTr("Edge evidence plot")
		  IntegerField {
				  name:         "edgeInclusionCriteria";
				  label:        qsTr("Inclusion criteria: BF\u2081\u2080 > ");
				  min:          1;
				  defaultValue: 10;
				  max:          2e2
			}
				  CheckBox { name:    "edgeInclusion"; label:   qsTr("Evidence for inclusion"); checked: true }
				  CheckBox { name: "edgeExclusion";  label: qsTr("Evidence for exclusion"); checked: true }
				  CheckBox { name: "edgeAbsence"; label: qsTr("Absence of evidence");   checked: true }
		}
		CheckBox { 
		  name: "centralityPlot"; id: centralityPlot;  label: qsTr("Centrality plot") 
		  CheckBox { 
				    name:    "credibilityInterval";
				    label:   qsTr("Credibility interval 95%");
				    checked: false 
		  }
	  }
	}

	Group
	{
		title: qsTr("Tables")
		CheckBox { name: "weightsMatrixTable";	label: qsTr("Weights matrix")	}
		CheckBox { 
		    name: "edgeEvidenceTable";		label: qsTr("Edge evidence probability table")
		    RadioButtonGroup {
				  name: "evidenceType";
		      RadioButton { value: "inclusionProbability"; label: qsTr("Edge inclusion probability") ; checked: true }
				  RadioButton { value: "BF10"; label: qsTr("BF\u2081\u2080") }
				  RadioButton { value: "BF01"; label: qsTr("BF\u2080\u2081") }
				  RadioButton { value: "log(BF)"; label: qsTr("Log(BF\u2081\u2080)") }
				}
		}
		CheckBox { name: "centralityTable"; label: qsTr("Centrality table") }
	}
	
	Section 
	{
	  title: qsTr("Sampling Options")
	  Layout.columnSpan: 2
	  IntegerField { name: "burnin"; label: qsTr("Burn in: "); value: "5000" ; min: 0; max: iter.value / 2; fieldWidth: 100; id: burnin }
	  IntegerField { name: "iter"; label: qsTr("Iterations: "); value: "10000" ; min: burnin.value * 2; fieldWidth: 100; id: iter }
		
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

		IntegerField { name: "dfprior"; label: qsTr("Degrees of freedom of G-Wishart prior (df prior): "); value: "3" ; min: 3; Layout.columnSpan: 2 }

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
					{ label: qsTr("Rainbow"),		   value: "rainbow"	    },
					{ label: qsTr("Colorblind"),	 value: "colorblind"	},
					{ label: qsTr("Pastel"),		   value: "pastel"		  },
					{ label: qsTr("Gray"),			   value: "gray"		    },
					{ label: qsTr("R"),				     value: "R"			      },
					{ label: qsTr("ggplot2"),		   value: "ggplot2"	    }
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
			CheckBox	{ name: "details";		label: qsTr("Show details") }
			CheckBox
			{
				name: "edgeLabels";			              label: qsTr("Edge labels");				checked: false
				DoubleField {	name: "edgeLabelSize";		label: qsTr("Edge label size");			      min: 0;			max: 10;	defaultValue: 1		}
				DoubleField {	name: "edgeLabelPosition";	label: qsTr("Edge label position");		min: 0;			max: 1;		defaultValue: 0.5	}
			}

			DropDown
			{
				name: "edgePalette"
				label: qsTr("Edge palette")
				indexDefaultValue: 1
				values:
				[
					{ label: qsTr("Classic"),		    value: "classic"		},
					{ label: qsTr("Colorblind"),	  value: "colorblind"		},
					{ label: qsTr("Gray"),			    value: "gray"			},
					{ label: qsTr("Hollywood"),		  value: "Hollywood"		},
					{ label: qsTr("Borkulo"),		    value: "Borkulo"		},
					{ label: qsTr("TeamFortress"),	value: "TeamFortress"	},
					{ label: qsTr("Reddit"),		    value: "Reddit"			},
					{ label: qsTr("Fried"),			    value: "Fried"			}
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
			RadioButton { value: "inLegend";		label: qsTr("In legend")					}
		}
		
		RadioButtonGroup
		{
			name: "legend"
			title: qsTr("Legend")
			RadioButton { value: "hide";		label: qsTr("No legend")					}
			RadioButton { value: "allPlots";		label: qsTr("All plots"); checked: true	}
			RadioButton
			{
				value: "specificPlot: "; label: qsTr("In plot number: ")
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
			RadioButton
			{
				value: "spring"; label: qsTr("Spring"); checked: true
				childrenOnSameRow: true
				DoubleField { name: "layoutSpringRepulsion"; label: qsTr("Repulsion"); defaultValue: 1; max: 10 }
			}
			RadioButton { value: "circle";	label: qsTr("Circle")							}
		}

		Group
		{
			title: qsTr("Measures shown in centrality plot")
			enabled: centralityPlot.checked
			CheckBox	{	name: "betweenness";		    label: qsTr("Betweenness");			    checked: true	}
			CheckBox	{	name: "closeness";			    label: qsTr("Closeness");			      checked: true	}
			CheckBox	{	name: "strength";				    label: qsTr("Strength");			      checked: true	}
			CheckBox	{	name: "expectedInfluence";	label: qsTr("Expected influence");	checked: true	}
		}
	}
	
	Section
	{
		title:		qsTr("Network structure selection")
		expanded:	false
		CheckBox { name: "posteriorStructurePlot";		label: qsTr("Posterior structure probability plot")	}
		CheckBox { name: "complexityPlot";		        label: qsTr("Posterior complexity probability plot")	}
	}
}
