import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name:			"jaspNetwork"
	title:			qsTr("Network")
	description:	qsTr("Explore the connections between variables organized as a network")
	icon:			"analysis-network.svg"
	version:		"0.17.0"
	author:			"JASP Team"
	maintainer:		"JASP Team <info@jasp-stats.org>"
	website:		"jasp-stats.org"
	license:		"GPL (>= 2)"


	Analysis
	{
		title:	qsTr("Network")
		func:	"NetworkAnalysis"
	}
}
