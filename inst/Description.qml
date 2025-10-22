import QtQuick
import JASP.Module

Description
{
	title:			qsTr("Network")
	description:	qsTr("Explore the connections between variables organized as a network")
	icon:			"analysis-network.svg"
	hasWrappers: 	false
	
	GroupTitle
	{
		title:	"Classical"
		icon:	"analysis-network.svg"
	}

	Analysis
	{
		title:	qsTr("Network Analysis")
		qml:	"NetworkAnalysis.qml"
		func:	"NetworkAnalysis"
	}

	Separator {}

	GroupTitle
	{
		title:	"Bayesian"
		icon:		"bayesian-analysis-network.svg"
	}

	Analysis
	{
		title:	qsTr("Bayesian Network Analysis")
		qml:	"BayesianNetworkAnalysis.qml"
		func:	"BayesianNetworkAnalysis"
	}
}
