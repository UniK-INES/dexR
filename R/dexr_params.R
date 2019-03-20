#' Merge the given list into the default dexpa parameter list.
#' Applies \code{\link[utils]{mergeList}}.
#' @param dexpa 
#' @return given list modified by adding undefined parameter of dexpa default parameter list.
#' 
#' @author Sascha Holzhauer
#' @export
param_mergeDefaultDexpa <- function(dexpa = list()) {
	################################################################
	# General SIMulation Properties:
	################################################################
	
	if (!exists("defdexpa")) defdexpa <- list()
	defdexpa$sim <- list()

	defdexpa$sim$version			<- "version"
	defdexpa$sim$id				<- c("NN")
	defdexpa$sim$duration			<- 2*60*60             # in sec
	defdexpa$sim$timefactor			<- 60.0

	defdexpa$sim$firstdeliverystart		<- list()
	defdexpa$sim$firstdeliverystart$delay	<- 0

	defdexpa$sim$starttime_min		<- as.numeric(strptime("01/01/1970 00:00", "%d/%m/%Y %H:%M"))*1000
	defdexpa$sim$starttime_max		<- as.numeric(strptime("31/12/2099 24:00", "%d/%m/%Y %H:%M"))*1000

	defdexpa$sim$filter$requests		<- function(dexpa, data) {
							dplyr::filter(data, TRUE)
						   }

	defdexpa$sim$filter$clearings		<- function(dexpa, data) {
							dplyr::filter(data, TRUE)
						   }
	### Basic ################################################################
	defdexpa$remoteserver			<- FALSE
	
	### Directories ################################################################
	
	defdexpa$dirs <- list()
	defdexpa$dirs$project			<- "./"
	defdexpa$dirs$config			<- "./config"
	
	defdexpa$dirs$freemarkertemplate	<- system.file("config/freemarker", package="dexR")
	defdexpa$dirs$csvtemplates		<- system.file("config/csv", package="dexR")
	defdexpa$dirs$xmltemplatesstatic	<- system.file("config/xml_static", package="dexR")
	
	# If empty, the a path relativ to dexpa$dirs$emgconfigtool is used!
	defdexpa$dirs$emgrundir			<- NULL
	defdexpa$dirs$emgnoderundir		<- "/local/ogema-run-dir"
	defdexpa$dirs$emglogdir			<- "data/logs"

	defdexpa$dirs$output <- list()
	defdexpa$dirs$outputdir			<- paste(defdexpa$dirs$project, "output/version/", sep="")	
	defdexpa$dirs$output$rdata		<- paste(defdexpa$dirs$outputdir, "rData/", sep="")
	defdexpa$dirs$output$dbdumps		<- paste(defdexpa$dirs$outputdir, "dbdumps/", sep="")
	defdexpa$dirs$output$dbdumpsremote	<- "/var/dex/dbdumps/"
	defdexpa$dirs$output$logs		<- paste(defdexpa$dirs$outputdir, "logs/", sep="")
	defdexpa$dirs$output$figures		<- paste(defdexpa$dirs$outputdir, "figures/", sep="")
	defdexpa$dirs$output$reports		<- paste(defdexpa$dirs$outputdir, "reports/", sep="")
	defdexpa$dirs$tmp				<- "/tmp"
	defdexpa$dirs$server			<- "set dexpa$dirs$server"
	defdexpa$dirs$emgconfigtool		<- "set dexpa$dirs$emgconfigtool"
	# not used any more
	#defdexpa$classpath$emg			<- "set dexpa$classpath$emg"

	### Files ################################################################
	defdexpa$files <- list()
	defdexpa$files$paramconfigs		<- paste(defdexpa$dirs$config, "DEX_Param_Configs.csv", sep="/")
	defdexpa$files$runinfos			<- paste(defdexpa$dirs$project, "DEX_Runs.csv", sep="/")
	defdexpa$files$emgconfigtool		<- paste(defdexpa$dirs$emgconfigtool, "emg-config-tool.jar", sep="/")
	defdexpa$files$serverjar		<- paste(dexpa$dirs$server, "enavi-market-backend-0.0.1-SNAPSHOT.jar", sep="/")
	defdexpa$files$backendPOM		<- "./market-backend/pom.xml"
	
	### DB Settings ##################################################################
	defdexpa$db <- list()	
	defdexpa$db$host			<- "localhost"

	defdexpa$db$suname			<- "postgres"
	defdexpa$db$supassword			<- "NotSet"

	defdexpa$db$dbname_template		<- "enavi_template"

	defdexpa$db$tablenames$clients		<- "user_account"
	defdexpa$db$tablenames$roles		<- "users_roles"

	defdexpa$db$tablenames$marketproducts	<- "market_product_pattern"
	defdexpa$db$tablenames$mmarketproducts	<- "mmarket_product_pattern"

	defdexpa$db$sshname			<- "demo"
	defdexpa$db$sshverbose		<- TRUE
	defdexpa$db$sshoutput		<- NULL
	
	### XML attributes Names ###########################################################
	defdexpa$xml <- list()
	defdexpa$xml$staticfiles 		<- c("OutsideBuildingRoom.xml", "kassel2012Sensors.xml", 
							"outsideTemperature.xml", "PropertyLocation.xml")
	
	### Figure Settings ###########################################################
	defdexpa$fig <- list()
	defdexpa$fig$resfactor		<- 3
	defdexpa$fig$outputformat 	<- "png" #"jpeg"
	defdexpa$fig$init			<- dexR::output_figure_init
	defdexpa$fig$close			<- dev.off
	defdexpa$fig$numfigs			<- 1
	defdexpa$fig$numcols			<- 1
	defdexpa$fig$height			<- 700
	defdexpa$fig$width			<- 1000
	defdexpa$fig$splitfigs		<- FALSE
	defdexpa$fig$plottitle		<- TRUE
	defdexpa$fig$alpha			<- 0.7
	defdexpa$fig$linewidth		<- 1
	defdexpa$fig$facetlabelsize	<- 12
	defdexpa$fig$legend$ncols	<- 3
	
	### Colour Settings ###########################################################
	defdexpa$colours <- list()
	defdexpa$colours$products 		<- c(	"1" = "blue",
							"2" = "red")
	defdexpa$colours$statuses 		<- c(	"0" = "blue",
							"1" = "green",
							"2" = "blue",
							"3" = "orange",
							"6" = "red")

	### Naming Settings ############################################################
	defdexpa$naming$statuses 		<- c(	"0" = "UNHANDELED",
							"1" = "ACCEPTED",
							"2" = "PARTLY_ACCEPTED",
							"3" = "DECLINED",
							"6" = "INVALID")

	### Market Server Settings ############################################################	
	defdexpa$server$url			<- "http://localhost"
	defdexpa$server$api$products	<- "config-products"
	defdexpa$server$api$start	<- "admin/start"
	defdexpa$server$api$shutdown	<- "admin/shutdown"
	defdexpa$server$api$status	<- "admin/status"
	defdexpa$server$api$submit	<- "api/submit"
	defdexpa$server$username	<- "admin"
	defdexpa$server$password	<- "multimodalES"
	defdexpa$server$profile		<- "requests"
	defdexpa$server$usemvn		<- TRUE
	defdexpa$server$controlinterval <- 2
	defdexpa$server$controls		<- 30
	defdexpa$server$port		<- 8080
	defdexpa$server$startport	<- 8000
	defdexpa$server$portoffset 	<- 0
	defdexpa$server$rseed		<- 0
	defdexpa$server$matchbasetime	<- "false"
	
	### EMG Settings ############################################################	
	defdexpa$emg$url		<- "https://localhost"
	
	defdexpa$emg$port		<- "8443"
	defdexpa$emg$startport		<- 8400
	defdexpa$emg$portoffset 	<- 0
	
	defdexpa$emg$httpport		<- "8088"
	defdexpa$emg$httpstartport	<- 9000
	defdexpa$emg$httpportoffset 	<- 0
	
	defdexpa$emg$rseed	 	<- 1
	
	defdexpa$emg$copyrundir	 	<- FALSE

	defdexpa$emg$emgconfigoutput	<- "emgconfig"

	defdexpa$emg$api$shutdown	<- "rest/admin?target=shutdown&user=rest&pw=rest"
	defdexpa$emg$emgstartuptime	<- 90
	defdexpa$emg$restarttime	<- 40

	defdexpa$emg$startoptions	<- "-clean -uro"

	### Debug Settings ############################################################
	defdexpa$debug <- list()
	# the higher, the more verbose
	defdexpa$debug$global 	<- 0
	defdexpa$debug$db		<- NA
	defdexpa$debug$input		<- NA
	defdexpa$debug$output	<- NA
	defdexpa$debug$fig		<- NA
	
	result <- modifyList(defdexpa, dexpa)
	result
}
#' Get the default dexpa parameter list
#' @return list of default dexpa parameters
#' 
#' @author Sascha Holzhauer
#' @export
param_getDefaultDexpa <- function() {
	param_mergeDefaultDexpa()
}
