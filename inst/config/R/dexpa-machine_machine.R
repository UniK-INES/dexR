###############################################################################
# Machine=specific dexpa definition
#
# NOTE: Changes in super-level parameters that are used to derive further
# parameters need to trigger a re-evaluation of the derived parameters!
#
# Project:		dexR
# Last update: 		08/03/2019
# Author: 		Sascha Holzhauer
################################################################################

### Clean/Remove existing definitions ##########################################
rm(list=ls(name=globalenv(), pattern="[^{preserve}]"), envir=globalenv())

### Set project Root ###########################################################
projectRoot			<- "/home/USERNAME/dexr"

if (!exists("preserve")) {
	preserve <- list()
	preserve$run = 0
}

#### Load project-specfic dexpa ################################################
source(paste(projectRoot, "/config/dexpa-project.R", sep=""))

### Directories ################################################################
if(!is.list(dexpa$dirs)) dexpa$dirs <- list()
# project folder is root of config and data files of the project
dexpa$dirs$project 		<- projectRoot
# script folders is root of R-scripts of the project
dexpa$dirs$scripts 		<- projectRoot

# folder with simulation-specifc subfolders for CSV files to edit
dexpa$dirs$config 		<- paste(dexpa$dirs$project, "%PVERSION%/config", sep="/")

# change these if you want to apply changes to the templates:
dexpa$dirs$freemarkertemplate	<- system.file("config/freemarker", package="dexR")
dexpa$dirs$csvtemplates		<- system.file("config/csv", package="dexR")
dexpa$dirs$xmltemplatesstatic	<- system.file("config/xml_static", package="dexR")

# folders with simulation-specifc subfolders for output files (e.g. log files, figures)
dexpa$dirs$outputdir		<- paste(dexpa$dirs$project, "%PVERSION%/output", sep"/")

if(!is.list(dexpa$dirs$output)) dexpa$dirs$output <- list()
dexpa$dirs$output$figures	<- paste(dexpa$dirs$outputdir, "figures/", sep="")
dexpa$dirs$output$reports	<- paste(dexpa$dirs$outputdir, "reports/", sep="")
dexpa$dirs$output$dbdumps	<- paste(dexpa$dirs$outputdir, "dbdumps/", sep="")
dexpa$dirs$output$logs		<- paste(dexpa$dirs$outputdir, "logs/", sep="")

dexpa$dirs$server		<- paste(dexpa$dirs$project, "%PVERSION%/release/market-backend", sep"/")
dexpa$dirs$emgconfigtool	<- paste(dexpa$dirs$project, "%PVERSION%/release/emg-config-tool", sep"/")
dexpa$dirs$emgrundir		<- paste(dexpa$dirs$project, "%PVERSION%/release/rundir-enavi", sep"/")

### Files ###############################################################
dexpa$files$paramconfigs	<- paste(dexpa$dirs$config, "DEX_Param_Configs.ods", sep="/")
dexpa$files$runinfos		<- paste(dexpa$dirs$outputdir, "DEX_Runs.csv", sep="/")

dexpa$files$emgconfigtool	<-  paste(dexpa$dirs$emgconfigtool, "emg-config-tool.jar", sep="/")
dexpa$files$serverjar		<-  paste(dexpa$dirs$server, "enavi-market-backend-0.0.1-SNAPSHOT.jar", sep="/")
dexpa$files$backendPOM		<- "/home/USER/git/enavi/market/market-backend/pom.xml"

#### Set path to itself ########################################################
dexpa$dexpaDefinition <- paste(dexpa$dirs$project, "config/dexpa-machine_maschine.R", sep="")


futile.logger::flog.info("Current working directory: %s",
		getwd(),
		name = "enavi.dexa")

