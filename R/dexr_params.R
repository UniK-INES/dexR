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
	defdexpa$sim$runnumber				<- 1
	defdexpa$sim$nodesetid <- NA
	defdexpa$sim$nodesetids <- NA
	defdexpa$sim$nodeid   <- 0
	defdexpa$sim$nodeids  <- 0
	defdexpa$sim$duration			<- 2*60*60             # in sec
	defdexpa$sim$timefactor			<- 60.0
	
	defdexpa$sim$raspic = F

	defdexpa$sim$firstdeliverystart		<- list()
	defdexpa$sim$firstdeliverystart$delay	<- 0
	defdexpa$sim$setinitialbasetime <- TRUE
	
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
	defdexpa$dirs$scripts     <- "./scripts"
	
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
	defdexpa$dirs$output$tables		<- paste(defdexpa$dirs$outputdir, "tables/", sep="")
	defdexpa$dirs$output$reports		<- paste(defdexpa$dirs$outputdir, "reports/", sep="")
	defdexpa$dirs$tmp				<- "/tmp"
	defdexpa$dirs$server			<- "set dexpa$dirs$server"
	defdexpa$dirs$emgconfigtool		<- "set dexpa$dirs$emgconfigtool"

	### Files ################################################################
	defdexpa$files <- list()
	defdexpa$files$paramconfigs		<- paste(defdexpa$dirs$config, "DEX_Param_Configs.csv", sep="/")
	defdexpa$files$runinfos			<- paste(defdexpa$dirs$project, "DEX_Runs.csv", sep="/")
	defdexpa$files$emgconfigtool		<- paste(defdexpa$dirs$emgconfigtool, "emg-config-tool-jar-with-dependencies.jar", sep="/")
	defdexpa$files$serverjar		<- paste(dexpa$dirs$server, "enavi-market-backend-0.0.1-SNAPSHOT.jar", sep="/")
	defdexpa$files$backendPOM		<- "./market-backend/pom.xml"
	
	### DB Settings ##################################################################
	defdexpa$db <- list()	
	defdexpa$db$host			<- "localhost"
  	defdexpa$db$port			<- "5432"
  
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
	defdexpa$fig$linewidth		<- 0.5
	defdexpa$fig$facetlabelsize	<- 12
	defdexpa$fig$legend$ncols	<- 3
	defdexpa$fig$labelsubs <- NULL
	defdexpa$fig$skiptitles <- F
	
	defdexpa$fig$show <- list()
	
	defdexpa$fig$show$costs <- list()
	defdexpa$fig$show$costs$summed_delivery <- T
	defdexpa$fig$show$costs$costgini_gen_delivery <- T
	defdexpa$fig$show$costs$costgini_load_delivery <- T
	
	defdexpa$fig$show$energy <- list()
	defdexpa$fig$show$energy$summed_delivery <- T
	defdexpa$fig$show$energy$residual_status_delivery <- T
	
	defdexpa$fig$show$requests <- list()
	defdexpa$fig$show$requests$product_submission <- T
	defdexpa$fig$show$requests$product_delivery <- T
	defdexpa$fig$show$requests$status_submission <- T
	defdexpa$fig$show$requests$clients_delivery <- T
	
	### Report Settings ###########################################################
	defdexpa$analyse <- list()
	defdexpa$analyse$intervalsdifftoaccept = 100
	
	### Report Settings ###########################################################
	defdexpa$reports <- list()
	defdexpa$reports$author <- "Uni Kassel"
	
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
							"6" = "INVALID",
							"7" = "INVALID_TOO_EARLY",
							"8" = "INVALID_TOO_LATE",
							"9" = "INVALID_STARTTIME",
							"10"= "INVALID_ENDTIME",
							"11"= "INVALID_PRODUCT",
							"12"= "INVALID_PRICE",
							"13"= "INVALID_ENERGY")

	### Market Server Settings ############################################################	
	defdexpa$server$url			<- "https://localhost"
	defdexpa$server$api$products	<- "config-products"
	defdexpa$server$api$start	<- "admin/start"
	defdexpa$server$api$shutdown	<- "admin/shutdown"
	defdexpa$server$api$status	<- "admin/status"
	defdexpa$server$api$status	<- "admin/version"
	defdexpa$server$api$submit	<- "api/submit"
	defdexpa$server$username	<- "admin"
	defdexpa$server$password	<- "multimodalES"
	defdexpa$server$profile		<- "requests"
	defdexpa$server$param     <- NULL
	defdexpa$server$cp        <- NULL
	defdexpa$server$usemvn		<- TRUE
	defdexpa$server$controlinterval <- 2
	defdexpa$server$controls		<- 30
	defdexpa$server$port		<- 8080
	defdexpa$server$startport	<- 8000
	defdexpa$server$portoffset 	<- 0
	defdexpa$server$rseed		<- 0
	defdexpa$server$matchbasetime	<- "false"
	defdexpa$server$logconfigfile <- "logback_t460.properties"
	defdexpa$server$sslcert <- "dex.p12"
	
	### EMG Settings ############################################################	
	defdexpa$emg$url		<- "https://localhost"
	
	defdexpa$emg$port		<- "8443"
	defdexpa$emg$startport		<- 8400
	defdexpa$emg$portoffset 	<- 0
	
	defdexpa$emg$propertiesfile <- "config/sh_ogema.properties"
	defdexpa$emg$httpport		<- "8088"
	defdexpa$emg$httpstartport	<- 9000
	defdexpa$emg$httpportoffset 	<- 0
	
	defdexpa$emg$rseed	 	<- 1
	
	defdexpa$emg$minuserid	 	<- 5
	
	defdexpa$emg$copyrundir	 	<- FALSE

	defdexpa$emg$emgconfigoutput	<- "emgconfig"

	defdexpa$emg$api$shutdown	<- "rest/admin?target=shutdown&user=rest&pw=rest"
	defdexpa$emg$emgstartuptime	<- 90
	defdexpa$emg$restarttime	<- 40

	defdexpa$emg$startoptions	<- "-clean -uro"
	
	### Raspberry Pi Cluster Settings #############################################
	defdexpa$raspic$user <- "outsider"
	defdexpa$raspic$server <- "192.168.1.111"
	defdexpa$raspic$serverconfigpath <- "/pxe/meta/simulation/"
	defdexpa$raspic$runemgcommand <- "/pxe/meta/sim_to_nodes"
	
	### OpSim Settings ############################################################
	defdexpa$opsim = list()
	defdexpa$opsim$control = list()
  defdexpa$opsim$control$rundir = "set opsim rundir"
	defdexpa$opsim$control$jre = NA
	defdexpa$opsim$control$args = "--module-path /usr/share/openjfx/lib --add-modules=javafx.base,javafx.controls,javafx.fxml,javafx.graphics,javafx.media,javafx.swing,javafx.web"
	defdexpa$opsim$control$jar = "iwes-opsim-mcp-gui-2.0.33-jar-with-dependencies.jar"
	defdexpa$opsim$control$logfile = "opsim.log"
	
	### Schedule Service Settings ############################################################
	defdexpa$opsim$sservice = list()
	defdexpa$opsim$sservice$rundir = "set schedule service rundir"
	defdexpa$opsim$sservice$jre = NA
	defdexpa$opsim$sservice$args = ""
	defdexpa$opsim$sservice$jar = "set jar for schedule service"
	defdexpa$opsim$sservice$logfile = "sservice.log"
	
	defdexpa$opsim$sservice$url = "https://localhost"
	defdexpa$opsim$sservice$port = 9443
	defdexpa$opsim$sservice$apiemptydb = "/CIM"
	defdexpa$opsim$sservice$apikey = "1234"

	### Panda power net simulation Settings ############################################################
	defdexpa$opsim$netsim = list()
	defdexpa$opsim$netsim$rundir = "set schedule service rundir"
	defdexpa$opsim$netsim$python = NA
	defdexpa$opsim$netsim$module = "Start_Grid_Proxy_Sched.py"
	defdexpa$opsim$netsim$pythonpath = ""
	defdexpa$opsim$netsim$logfile = "netsim.log"
	
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
  dexpa <- param_mergeDefaultDexpa()
  # enable HTTP requests to DEX server
  httr::set_config(httr::config(cainfo=system.file("config/certificates", dexpa$server$sslcert,package="dexR")))
  httr::set_config(httr::config(ssl_verifypeer = 0L))
	return(dexpa)
}
