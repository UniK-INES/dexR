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
projectRoot			<- paste(dirname(tempfile()), "Testproject", sep="/")

if (!exists("preserve")) {
	preserve <- list()
	preserve$run = 0
}

#### Load project-specfic dexpa ################################################
source(system.file("demo/dexpa-project.R", package="dexR"))

### Directories ################################################################
if(!is.list(dexpa$dirs)) dexpa$dirs <- list()
# project folder is root of config and data files of the project
dexpa$dirs$project 		<- projectRoot
# script folders is root of R-scripts of the project
dexpa$dirs$scripts 		<- paste(projectRoot, "scripts", sep="/")

# folder with simulation-specifc subfolders for CSV files to edit
dexpa$dirs$config 		<- paste(dexpa$dirs$project, "%PVERSION%/config", sep="/")

# change these if you want to apply changes to the templates:
dexpa$dirs$freemarkertemplate	<- system.file("config/freemarker", package="dexR")
dexpa$dirs$csvtemplates		<- system.file("config/csv", package="dexR")
dexpa$dirs$xmltemplatesstatic	<- system.file("config/xml_static", package="dexR")

# folders with simulation-specifc subfolders for output files (e.g. log files, figures)
dexpa$dirs$outputdir		<- paste(dexpa$dirs$project, "%PVERSION%/output", sep="/")

if(!is.list(dexpa$dirs$output)) dexpa$dirs$output <- list()
dexpa$dirs$output$figures	<- paste(dexpa$dirs$outputdir, "figures/", sep="")
dexpa$dirs$output$reports	<- paste(dexpa$dirs$outputdir, "reports/", sep="")
dexpa$dirs$output$dbdumps	<- paste(dexpa$dirs$outputdir, "dbdumps/", sep="")
dexpa$dirs$output$logs		<- paste(dexpa$dirs$outputdir, "logs/", sep="")

dexpa$dirs$server		<- paste(dexpa$dirs$project, "%PVERSION%/release/market-backend", sep="/")
#dexpa$dirs$emgconfigtool	<- paste(dexpa$dirs$project, "%PVERSION%/release/emg-config-tool", sep="/")
dexpa$dirs$emgconfigtool	<- paste(Sys.getenv("GIT_DIR"), "enavi/emg/src/emg-config-tool/emg-config-tool/target/", sep="/")
#dexpa$dirs$emgrundir		<- paste(dexpa$dirs$project, "%PVERSION%/release/rundir-enavi", sep="/")
dexpa$dirs$emgrundir		<- paste(Sys.getenv("GIT_DIR"), "enavi/emg/rundirs/rundir-enavi", sep="/")

### Files ###############################################################
if(!is.list(dexpa$files)) dexpa$files <- list()
dexpa$files$paramconfigs	<- paste(dexpa$dirs$config, "DEX_Param_Configs.ods", sep="/")
dexpa$files$runinfos		<- paste(dexpa$dirs$outputdir, "DEX_Runs.csv", sep="/")

dexpa$files$emgconfigtool	<-  paste(dexpa$dirs$emgconfigtool, "emg-config-tool.jar", sep="/")
dexpa$files$serverjar		<-  paste(dexpa$dirs$server, "enavi-market-backend-0.0.1-SNAPSHOT.jar", sep="/")
dexpa$files$backendPOM		<-  paste(Sys.getenv("GIT_DIR"), "enavi/market/market-backend/pom.xml", sep="/")

dexpa$classpath$emg		<- "/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/resources.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/rt.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/jsse.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/jce.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/charsets.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/sunjce_provider.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/nashorn.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/sunec.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/sunpkcs11.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/zipfs.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/localedata.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/cldrdata.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/java-atk-wrapper.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/jaccess.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/icedtea-sound.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/dnsns.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/jfxrt.jar:/home/sascha/git/enavi/emg/src/emg-config-tool/emg-config-tool/target/classes:/home/sascha/.m2/repository/javax/activation/activation/1.1/activation-1.1.jar:/home/sascha/.m2/repository/org/freemarker/freemarker/2.3.27-incubating/freemarker-2.3.27-incubating.jar:/home/sascha/.m2/repository/org/apache/commons/commons-csv/1.5/commons-csv-1.5.jar:/home/sascha/.m2/repository/commons-cli/commons-cli/1.4/commons-cli-1.4.jar:/home/sascha/.m2/repository/ch/qos/logback/logback-classic/1.3.0-alpha4/logback-classic-1.3.0-alpha4.jar:/home/sascha/.m2/repository/ch/qos/logback/logback-core/1.3.0-alpha4/logback-core-1.3.0-alpha4.jar:/home/sascha/.m2/repository/org/slf4j/slf4j-api/1.8.0-beta1/slf4j-api-1.8.0-beta1.jar de.unik.ines.enavi.ctool.EmgConfigManager"


#### Set path to itself ########################################################
dexpa$dexpaDefinition <- paste(dexpa$dirs$project, "config/dexpa-machine_maschine.R", sep="")


futile.logger::flog.info("Current working directory: %s",
		getwd(),
		name = "enavi.dexpa")

