## ---- eval=F------------------------------------------------------------------
#  Sys.getenv("GIT_DIR")
#  Sys.setenv("GIT_DIR"="<path to git repositry including enavi projeckt, eg. ~/git>")

## ---- eval=FALSE--------------------------------------------------------------
#  demo(config, package="dexR", echo=F)

## ---- eval=FALSE--------------------------------------------------------------
#  dexpa$db$host			<- "localhost"
#  dexpa$db$port			<- "5432"
#  dexpa$db$dbname			<- "enavi"
#  dexpa$db$username		<- "enavi"
#  dexpa$db$password		<- "enavi!"
#  
#  dexpa$db$suname			<- "postgres"
#  dexpa$db$supassword		<- "supassword"

## ---- eval=FALSE--------------------------------------------------------------
#  preserve <- list()
#  preserve$dexpa$db$supassword         <- "anotherSUpassword"

## ---- eval=FALSE--------------------------------------------------------------
#  demo(run, package="dexR", echo=F)

## ---- eval=FALSE--------------------------------------------------------------
#  dexpa <- dexR::param_getDefaultDexpa()
#  dexpa$dirs$project <- "<substitute by path to project>/project"
#  dexpa$dirs$config  <- "<substitute by path to project>/project/config"
#  dexR::setup_environment(dexpa)

## ---- eval=FALSE--------------------------------------------------------------
#  source("/home/USER/dexr/scripts/dexpa-machine_machine.R")
#  dexpa$sim$version <- "TestA
#  dexpa <- dexR::setup_project_version(dexpa)

## ---- eval=FALSE--------------------------------------------------------------
#  source("/home/USER/dexr/scripts/dexpa-machine_machine.R")
#  dexpa$sim$version <- "TestA"	
#  dexpa$sim$id 	  <- "Testrun01"
#  
#  dexR::hl_config_copycsvtemplates(dexpa)

## ---- eval=FALSE--------------------------------------------------------------
#  ################################################################
#  # General SIMulation Properties:
#  ################################################################
#  
#  if (!exists("dexpa")) dexpa <- list()
#  dexpa$sim <- list()
#  
#  dexpa$sim$version			<- "version"
#  dexpa$sim$id				<- c("NN")
#  dexpa$sim$duration			<- 2*60*60             # in sec
#  dexpa$sim$timefactor			<- 60.0
#  
#  dexpa$sim$firstdeliverystart		<- list()
#  dexpa$sim$firstdeliverystart$delay	<- 0
#  
#  dexpa$sim$starttime_min		<- as.numeric(strptime("01/01/1970 00:00", "%d/%m/%Y %H:%M"))*1000
#  dexpa$sim$starttime_max		<- as.numeric(strptime("31/12/2099 24:00", "%d/%m/%Y %H:%M"))*1000
#  
#  dexpa$sim$filter$requests		<- function(dexpa, data) {
#  						dplyr::filter(data, TRUE)
#  					   }
#  
#  dexpa$sim$filter$clearings		<- function(dexpa, data) {
#  						dplyr::filter(data, TRUE)
#  					   }
#  ### Basic ################################################################
#  dexpa$remoteserver			<- FALSE
#  
#  ### Directories ################################################################
#  
#  dexpa$dirs <- list()
#  dexpa$dirs$project			<- "./"
#  dexpa$dirs$config			<- "./config"
#  
#  dexpa$dirs$freemarkertemplate	<- system.file("config/freemarker", package="dexR")
#  dexpa$dirs$csvtemplates		<- system.file("config/csv", package="dexR")
#  dexpa$dirs$xmltemplatesstatic	<- system.file("config/xml_static", package="dexR")
#  
#  # If empty, the a path relativ to dexpa$dirs$emgconfigtool is used!
#  dexpa$dirs$emgrundir			<- NULL
#  dexpa$dirs$emgnoderundir		<- "/local/ogema-run-dir"
#  dexpa$dirs$emglogdir			<- "data/logs"
#  
#  dexpa$dirs$output <- list()
#  dexpa$dirs$outputdir			<- paste(dexpa$dirs$project, "output/version/", sep="")	
#  dexpa$dirs$output$rdata		<- paste(dexpa$dirs$outputdir, "rData/", sep="")
#  dexpa$dirs$output$dbdumps		<- paste(dexpa$dirs$outputdir, "dbdumps/", sep="")
#  dexpa$dirs$output$dbdumpsremote	<- "/var/dex/dbdumps/"
#  dexpa$dirs$output$logs		<- paste(dexpa$dirs$outputdir, "logs/", sep="")
#  dexpa$dirs$output$figures		<- paste(dexpa$dirs$outputdir, "figures/", sep="")
#  dexpa$dirs$output$reports		<- paste(dexpa$dirs$outputdir, "reports/", sep="")
#  dexpa$dirs$tmp				<- "/tmp"
#  dexpa$dirs$server			<- "set dexpa$dirs$server"
#  dexpa$dirs$emgconfigtool		<- "set dexpa$dirs$emgconfigtool"
#  
#  ### Files ################################################################
#  dexpa$files <- list()
#  dexpa$files$paramconfigs		<- paste(dexpa$dirs$config, "DEX_Param_Configs.csv", sep="/")
#  dexpa$files$runinfos			<- paste(dexpa$dirs$project, "DEX_Runs.csv", sep="/")
#  dexpa$files$emgconfigtool		<- paste(dexpa$dirs$emgconfigtool, "emg-config-tool.jar", sep="/")
#  dexpa$files$serverjar		<- paste(dexpa$dirs$server, "enavi-market-backend-0.0.1-SNAPSHOT.jar", sep="/")
#  dexpa$files$backendPOM		<- "./market-backend/pom.xml"
#  
#  ### DB Settings ##################################################################
#  dexpa$db <- list()	
#  dexpa$db$host			<- "localhost"
#  
#  dexpa$db$suname			<- "postgres"
#  dexpa$db$supassword			<- "NotSet"
#  
#  dexpa$db$dbname_template		<- "enavi_template"
#  
#  dexpa$db$tablenames$clients		<- "user_account"
#  dexpa$db$tablenames$roles		<- "users_roles"
#  
#  dexpa$db$tablenames$marketproducts	<- "market_product_pattern"
#  dexpa$db$tablenames$mmarketproducts	<- "mmarket_product_pattern"
#  
#  dexpa$db$sshname			<- "demo"
#  dexpa$db$sshverbose		<- TRUE
#  dexpa$db$sshoutput		<- NULL
#  
#  ### XML attributes Names ###########################################################
#  dexpa$xml <- list()
#  dexpa$xml$staticfiles 		<- c("OutsideBuildingRoom.xml", "kassel2012Sensors.xml",
#  						"outsideTemperature.xml", "PropertyLocation.xml")
#  
#  ### Figure Settings ###########################################################
#  dexpa$fig <- list()
#  dexpa$fig$resfactor		<- 3
#  dexpa$fig$outputformat 	<- "png" #"jpeg"
#  dexpa$fig$init			<- dexR::output_figure_init
#  dexpa$fig$close			<- dev.off
#  dexpa$fig$numfigs			<- 1
#  dexpa$fig$numcols			<- 1
#  dexpa$fig$height			<- 700
#  dexpa$fig$width			<- 1000
#  dexpa$fig$splitfigs		<- FALSE
#  dexpa$fig$plottitle		<- TRUE
#  dexpa$fig$alpha			<- 0.7
#  dexpa$fig$linewidth		<- 1
#  dexpa$fig$facetlabelsize	<- 12
#  dexpa$fig$legend$ncols	<- 3
#  
#  ### Colour Settings ###########################################################
#  dexpa$colours <- list()
#  dexpa$colours$products 		<- c(	"1" = "blue",
#  						"2" = "red")
#  dexpa$colours$statuses 		<- c(	"0" = "blue",
#  						"1" = "green",
#  						"2" = "blue",
#  						"3" = "orange",
#  						"6" = "red")
#  
#  ### Naming Settings ############################################################
#  dexpa$naming$statuses 		<- c(	"0" = "UNHANDELED",
#  						"1" = "ACCEPTED",
#  						"2" = "PARTLY_ACCEPTED",
#  						"3" = "DECLINED",
#  						"6" = "INVALID")
#  
#  ### Market Server Settings ############################################################	
#  dexpa$server$url			<- "http://localhost"
#  dexpa$server$api$products	<- "config-products"
#  dexpa$server$api$start	<- "admin/start"
#  dexpa$server$api$shutdown	<- "admin/shutdown"
#  dexpa$server$api$status	<- "admin/status"
#  dexpa$server$api$submit	<- "api/submit"
#  dexpa$server$username	<- "admin"
#  dexpa$server$password	<- "multimodalES"
#  dexpa$server$profile		<- "requests"
#  dexpa$server$usemvn		<- TRUE
#  dexpa$server$controlinterval <- 2
#  dexpa$server$controls		<- 30
#  dexpa$server$port		<- 8080
#  dexpa$server$startport	<- 8000
#  dexpa$server$portoffset 	<- 0
#  dexpa$server$rseed		<- 0
#  dexpa$server$matchbasetime	<- "false"
#  
#  ### EMG Settings ############################################################	
#  dexpa$emg$url			<- "https://localhost"
#  
#  dexpa$emg$port			<- "8443"
#  dexpa$emg$startport		<- 8400
#  dexpa$emg$portoffset 		<- 0
#  
#  dexpa$emg$httpport		<- "8088"
#  dexpa$emg$httpstartport		<- 9000
#  dexpa$emg$httpportoffset 	<- 0
#  
#  dexpa$emg$rseed	 		<- 1
#  
#  dexpa$emg$copyrundir	 	<- FALSE
#  
#  dexpa$emg$emgconfigoutput	<- "emgconfig"
#  
#  dexpa$emg$api$shutdown		<- "rest/admin?target=shutdown&user=rest&pw=rest"
#  dexpa$emg$emgstartuptime	<- 90
#  dexpa$emg$restarttime		<- 40
#  
#  dexpa$emg$startoptions		<- "-clean -uro"
#  
#  ### Debug Settings ############################################################
#  dexpa$debug <- list()
#  # the higher, the more verbose
#  dexpa$debug$global 		<- 0
#  dexpa$debug$db		<- NA
#  dexpa$debug$input		<- NA
#  dexpa$debug$output		<- NA
#  dexpa$debug$fig		<- NA

## ---- eval=FALSE--------------------------------------------------------------
#  dexpa$db$host		<- "localhost"
#  dexpa$db$port		<- "5432"
#  dexpa$db$dbname		<- "dexr"
#  dexpa$db$username	<- "username"
#  dexpa$db$password	<- "password"

