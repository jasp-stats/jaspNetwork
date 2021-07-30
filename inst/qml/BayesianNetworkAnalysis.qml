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
		AssignedVariablesList { name: "variables";			title: qsTr("Dependent Variables"); suggestedColumns: ["scale"]; id: networkVariables}
		AssignedVariablesList { name: "groupingVariable";	title: qsTr("Split"); singleVariable: true; suggestedColumns: ["ordinal", "nominal"] }
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox { name: "plotNetwork";		label: qsTr("Network plot")								}
		CheckBox { name: "plotInclusion";		label: qsTr("Inclusion plot")								}
		CheckBox { name: "plotDensity";		label: qsTr("Posterior distribution of network density")	}
	}

	Group
	{
		title: qsTr("Tables")
		CheckBox { name: "tableWeightsMatrix";	label: qsTr("Weights matrix")	}
		CheckBox { name: "tableEdgeInclusion";		label: qsTr("Edge inclusion probability table")	}
	}
	
	Section 
	{
	  title: qsTr("Options")
	  FormulaField { name: "burnin"; label: qsTr("Burn in: "); value: "50000" ; min: 0; Layout.columnSpan: 2 }
	  FormulaField { name: "iter"; label: qsTr("Iterations: "); value: "100000" ; min: 0; Layout.columnSpan: 2 }
	}

	Section
	{
		title: qsTr("Prior")

		FormulaField { name: "gprior"; label: qsTr("g prior: "); value: "0.5" ; min: 0; max: 1; Layout.columnSpan: 2 }
		FormulaField { name: "dfprior"; label: qsTr("df prior: "); value: "3" ; min: 0; Layout.columnSpan: 2 }

	}

//	Section
//	{
//		title: qsTr("Partial Correlation Distribution")

//		FormulaField { name: "gprior"; label: qsTr("g prior: "); value: "0.5" ; min: 0; Layout.columnSpan: 2 }
//		FormulaField { name: "dfprior"; label: qsTr("df prior: "); value: "3" ; min: 0; Layout.columnSpan: 2 }


//	}

	Section
	{
		title: qsTr("Graphical Options")

		Group
		{
			Layout.columnSpan: 2
			DropDown
			{
				id: paletteSelector
				name: "nodePalette"
				label: qsTr("Node palette")
				indexDefaultValue: 1
				values: [
					{ label: qsTr("Rainbow"),		  value: "rainbow"	},
					{ label: qsTr("Colorblind"),	value: "colorblind"	},
					{ label: qsTr("Pastel"),		  value: "pastel"		},
					{ label: qsTr("Gray"),			  value: "gray"		},
					{ label: qsTr("R"),				    value: "R"			},
					{ label: qsTr("ggplot2"),		  value: "ggplot2"	}
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
			CheckBox { name: "keepLayoutTheSame"; label: qsTr("Do not update layout") }
			RadioButton
			{
				value: "spring"; label: qsTr("Spring"); checked: true
				childrenOnSameRow: true
				DoubleField { name: "repulsion"; label: qsTr("Repulsion"); defaultValue: 1; max: 10 }
			}
			RadioButton { value: "circle";	label: qsTr("Circle")							}
			RadioButton { value: "data";	label: qsTr("Data");	id: dataRatioButton		}
		}

	}
}
