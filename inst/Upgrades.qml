import QtQuick		2.12
import JASP.Module	1.0

Upgrades
{
	Upgrade
	{
		functionName:		"NetworkAnalysis"
		fromVersion:		"0.15"
		toVersion:			"0.16.4"

		ChangeJS
		{
			name: "estimator"
			jsFunction: function(options)
			{
				switch (options["estimator"])
				{
					case "EBICglasso":			return "ebicGlasso";
					case "IsingFit":			return "isingFit";
					case "IsingSampler":		return "isingSampler";
					// the other values did not change
					default:					return options["estimator"];

				}
			}
		}

		ChangeRename	{	from:	"plotNetwork";						to:		"networkPlot"									}
		ChangeRename	{	from:	"plotCentrality";					to:		"centralityPlot"								}
		ChangeRename	{	from:	"plotClustering";					to:		"networkclusteringPlotPlot"						}

		ChangeRename	{	from: 	"tableCentrality";					to:		"centralityTable"								}
		ChangeRename	{	from: 	"tableClustering";					to:		"clusteringTable"								}
		ChangeRename	{	from: 	"tableWeightsMatrix";				to:		"weightsMatrixTable"							}

		ChangeRename	{	from:	"normalizeCentrality";				to:		"centralityNormalization"						}

		ChangeRename	{	from: 	"mgmVariableTypeContinuous";		to:		"mgmContinuousVariables"						}
		ChangeRename	{	from: 	"mgmVariableTypeCategorical";		to:		"mgmCategoricalVariables"						}
		ChangeRename	{	from: 	"mgmVariableTypeCount";				to:		"mgmCountVariables"								}

		ChangeRename	{	from:	"BootstrapType";					to:		"bootstrapType"									}

		ChangeRename	{	from: 	"StatisticsEdges";					to:		"statisticsEdges"								}
		ChangeRename	{	from: 	"StatisticsCentrality";				to:		"statisticsCentrality"							}

		ChangeRename	{	from:	"showDetails";						to:		"details"										}

		ChangeJS
		{
			name: "edgeColors"
			jsFunction: function(options)
			{
				console.log("Inside jsFunction for 'edgeColors'");
				switch (options["edgeColors"])
				{
					case "Hollywood":			return "hollywood";
					case "Borkulo":				return "borkulo";
					case "TeamFortress":		return "teamFortress";
					case "Reddit":				return "reddit";
					case "Fried":				return "fried";
					// the other values did not change
					default:					return options["edgeColors"];

				}
			}
		}

		ChangeRename	{	from:	"scaleLabels";						to:		"labelScale"									}

		ChangeRename	{	from:	"showVariableNames";				to:		"variableNamesShown"							}
		ChangeJS
		{
			name: "variableNamesShown"
			jsFunction: function(options)
			{
				switch (options["variableNamesShown"])
				{
					case "In nodes":			return "inNodes";
					case "In legend":			return "inLegend";
				}
			}
		}

		ChangeRename	{	from:	"showMgmVariableType";				to:		"mgmVariableTypeShown"							}
		ChangeJS
		{
			name: "mgmVariableTypeShown"
			jsFunction: function(options)
			{
				switch (options["mgmVariableTypeShown"])
				{
					case "mgmNoShow":			return "hide";
					case "mgmNodeColor":		return "nodeColor";
					case "mgmNodeShape":		return "nodeShape";
				}
			}
		}

		ChangeRename	{	from:	"showLegend";						to:		"legend"										}
		ChangeJS
		{
			name: "legend"
			jsFunction: function(options)
			{
				switch (options["legend"])
				{
					case "No legend":			return "hide";
					case "All plots":			return "allPlots";
					case "In plot number: ":	return "specificPlot";
				}
			}
		}

		ChangeRename	{	from: 	"Betweenness";						to:		"betweenness"									}
		ChangeRename	{	from: 	"Closeness";						to:		"closeness"										}
		ChangeRename	{	from: 	"Degree";							to:		"degree"										}
		ChangeRename	{	from: 	"ExpectedInfluence";				to:		"expectedInfluence"								}

		ChangeRename	{	from: 	"addLayoutToData";					to:		"layoutSavedToData"								}

	}
}
