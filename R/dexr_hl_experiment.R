#' Run backend server to experiment
#' @param dexpa parameter object
#' @param outfilesys log file
#' @param basetime in ms, assigned to \code{de.unik.enavi.market.time.basetime}
#' @param offset in ms, assigned to \code{de.unik.enavi.market.time.offset}
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
hl_experiment_runbackend <- function(dexpa, outfilesys = "", basetime = as.numeric(round(Sys.time(),"mins"))*1000,
		offset = round(basetime - as.numeric(Sys.time())*1000)) {
	infoData <- list()
	futile.logger::flog.info("Starting Market Backend server (output to %s) with offset=%s/factor=%d/basetime=%s...", 
			outfilesys,
			format(as.POSIXct(offset/1000, tz="GTM", origin = "1970-01-01"), "%H:%M:%S"),
			dexpa$sim$timefactor,
			as.POSIXlt(basetime/1000, origin = "1970-01-01"),
			name = "dexr.hl.experiment.runbackend")
	
	# Instatiate server:
	system2(wait=FALSE, "mvn", args=paste("-f ", dexpa$dirs$backend, " spring-boot:run ",
					"-Dde.unik.enavi.market.testing.load=FALSE ",
					"-Dde.unik.enavi.market.time.factor=", dexpa$sim$timefactor, " ",
					"-Dde.unik.enavi.market.time.basetime=", format(basetime, scientific = FALSE), " ", 
					"-Dde.unik.enavi.market.time.offset=", format(offset, scientific = FALSE), sep=""),
			stdout=outfilesys, stderr=outfilesys)
	
	control = 0
	while (!server_isrunning(dexpa) && control < 30) {
		# wait...
		futile.logger::flog.info("Again, check availability of market backend server...",
				name = "dexr.hl.experiment")
		Sys.sleep(2)
		control = control + 1
	}
	
	if (control >= 30) {
		R.oo::throw.default("Starting market backend server NOT successful!")
	}
	
	# Import market products;
	if (tools::file_ext(dexpa$files$paramconfigs)=="ods") {
		paramConfigs <- readODS::read_ods(dexpa$files$paramconfigs, sheet = 1)
	} else {
		paramConfigs <- read.csv(dexpa$files$paramconfigs, header = TRUE, sep = ",", quote = "\"",
				dec = ".", fill = TRUE, comment.char = "")
	}

	# Check Runs.csv for requested ID:
	idMatch <- match(dexpa$sim$id, paramConfigs$ID)
	if(is.na(idMatch)) {
		futile.logger::flog.warn("ID %s not present in config table (%s)!", dexpa$sim$id, dexpa$files$paramconfigs, 
				name = "dexr.hl.experiment")
	}
	hl_config_marketProducts2db(dexpa, 
			firstDeliveryPeriodStart = as.POSIXlt(basetime/1000, origin = "1970-01-01"),
			sourcefile = if(!is.na(idMatch)) paramConfigs[idMatch, "products"] else 
						paste("DEX_Param_MarketProducts_", dexpa$sim$id, ".csv", sep=""))
	
	# Insert client data:
	infoData$numClients <- hl_config_clients2db(dexpa,
			sourcefile = if(!is.na(idMatch)) paramConfigs[idMatch, "clients"] else 
						paste("DEX_Param_EnaviClient_", dexpa$sim$id, ".csv", sep=""))
	
	# Start backend server:
	message = server_start(dexpa)
	futile.logger::flog.info(message, name = "dexr.hl.experiment")
	return(infoData)
}
#' Run EMGs to experiment
#' @param dexpa 
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
hl_experiment_runemg <- function(dexpa, outfilesys = "") {
	## create XML configuration:
	if (outfilesys == "") {
		outfilesystxt = "CONSOLE"
	} else {
		outfilesystxt = outfilesys
	}
	futile.logger::flog.info("Creating EMG XML configuration (output to %s)...", outfilesystxt, name = "dexr.hl.experiment")
	path = getwd();
	
	setwd(paste(dexpa$dirs$emgconfigtool, "/../..", sep=""))
	
	if (tools::file_ext(dexpa$files$paramconfigs)=="ods") {
		paramConfigs <- readODS::read_ods(dexpa$files$paramconfigs, sheet = 1, col_names = TRUE)
	} else {
		paramConfigs <- read.csv(dexpa$files$paramconfigs, header = TRUE, sep = ",", quote = "\"",
			dec = ".", fill = TRUE, comment.char = "")
	}
	
	# Check Runs.csv for requested ID:
	idMatch <- match(dexpa$sim$id, paramConfigs$ID)
	if(is.na(idMatch)) {
		futile.logger::flog.warn("ID %s not present in config table (%s)!", dexpa$sim$id, dexpa$files$paramconfigs, 
				name = "dexr.hl.experiment.runemg")
	}
	if (!is.na(idMatch)) {
		# Read CSV sources from DEX_Param_Configs.csv:
				system2(wait=TRUE, "mvn", args = paste(" exec:java "," -Dexec.mainClass=de.unik.ines.enavi.ctool.EmgConfigManager"," -Dexec.args=\"", 
			" -i ", dexpa$sim$id,
			" -o '", dexpa$dirs$config, "'",
			" -t '", dexpa$dirs$freemarkertemplate, "'",
			" -c '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/", paramConfigs[idMatch,"clients"], sep=""), "'",
			" -l '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/", paramConfigs[idMatch,"loads"], sep=""), "'",
			" -g '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/", paramConfigs[idMatch,"generations"], sep=""), "'",
			" -b '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/", paramConfigs[idMatch,"buildings"], sep=""), "'",
			" -p '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/", paramConfigs[idMatch,"pvplants"], sep=""), "'",
			" -w '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/", paramConfigs[idMatch,"windplants"], sep=""), "'",
			" -s '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/", paramConfigs[idMatch,"loadProfiles"], sep=""), "'",
			" -d '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/", paramConfigs[idMatch,"storages"], sep=""), "'",
			" -dd '",paste(dexpa$dirs$config, "/", dexpa$sim$id, "/", paramConfigs[idMatch,"devicesStorage"], sep=""), "'",
			" -r '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/", paramConfigs[idMatch,"requestConfig"], sep=""), "'",
			" -sc '",paste(dexpa$dirs$config, "/", dexpa$sim$id, "/", paramConfigs[idMatch,"ogemaConfig"], sep=""), "'",
		 "\"", sep=""), stdout=outfilesys, stderr=outfilesys)
	} else {	
		system2(wait=TRUE, "mvn", args = paste(" exec:java "," -Dexec.mainClass=de.unik.ines.enavi.ctool.EmgConfigManager"," -Dexec.args=\"", 
			" -i ", dexpa$sim$id,
			" -o '", dexpa$dirs$config, "'",
			" -t '", dexpa$dirs$freemarkertemplate, "'",
			" -c '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/DEX_Param_EnaviClient_", dexpa$sim$id, ".csv", sep=""), "'",
			" -l '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/DEX_Param_Loads_", dexpa$sim$id, ".csv", sep=""), "'",
			" -g '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/DEX_Param_Generations_", dexpa$sim$id, ".csv", sep=""), "'",
			" -b '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/DEX_Param_DevicesBuilding_", dexpa$sim$id, ".csv", sep=""), "'",
			" -p '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/DEX_Param_DevicesPVplant_", dexpa$sim$id, ".csv", sep=""), "'",
			" -w '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/DEX_Param_DevicesWindplant_", dexpa$sim$id, ".csv", sep=""), "'",
			" -s '", paste(dexpa$dirs$config, "/",  dexpa$sim$id, "/DEX_Param_LoadProfiles_", dexpa$sim$id, ".csv", sep=""), "'",
			" -d '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/DEX_Param_Storages_", dexpa$sim$id, ".csv", sep=""), "'",
			" -dd '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/DEX_Param_DevicesStorage_", dexpa$sim$id, ".csv", sep=""), "'",
			" -r '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/DEX_Param_RequestConfig_", dexpa$sim$id, ".csv", sep=""), "'",
			" -sc '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/DEX_Param_OgemaConfig_", dexpa$sim$id, ".csv", sep=""), "'",
		 "\"", sep=""), stdout=outfilesys, stderr=outfilesys)
	}

	# copy static XML files:
	for (f in dexpa$xml$staticfiles) {
		file.copy(from = paste(dexpa$dirs$xmltemplatesstatic, f, sep="/"), to = paste(dexpa$dirs$config, "/", dexpa$sim$id, sep=""),
				overwrite = TRUE)
	}

	futile.logger::flog.info("Starting EMG...", name = "dexr.hl.experiment.emg")
	
	
	#Sys.setenv(VMOPTS = paste("-Dorg.ogema.app.resadmin.replay_oncleanstart_path=", dexpa$dirs$config, dexpa$sim$id, sep=""))
	#setwd(dexpa$dirs$emgrundir)
	#system2(wait=FALSE, "bash", args = paste(
	#	"./start.sh -clean --properties config/sh_ogema.properties", sep=""))

	setwd(dexpa$dirs$emgconfigtool)	

	system2(wait=FALSE, "java", args = paste("de.unik.ines.enavi.ctool.RunEmg", 
			paste(dexpa$dirs$config, "/", dexpa$sim$id, sep=""), dexpa$dirs$emgrundir),
			stdout=outfilesys, stderr=outfilesys)

	# https://www.rdocumentation.org/packages/sys/versions/1.5/topics/exec
	#	pid = sys::exec_background("java", args = c("de.unik.ines.enavi.ctool.RunEmg", 	
	#					paste(dexpa$dirs$config, "/", dexpa$sim$id, sep=""), dexpa$dirs$emgrundir),
	#			std_out=outfilesys, std_err=outfilesys)
	
	setwd(path)
	futile.logger::flog.info("Wait during EMG startup (%d sec)...", dexpa$emg$emgstartuptime, name = "dexr.hl.experiment")
	Sys.sleep(dexpa$emg$emgstartuptime)
	
#	max_sleep = dexpa$emg$emgstartuptime
#	num_iterations = as.integer(max_sleep / 5) + 1
#	# make sure not to wait for the full startup time in case an error occurs
#	for (i in 1:num_iterations) {
#		Sys.sleep(5)
#		status = sys::exec_status(pid, wait = FALSE)
#		if (!is.na(status)) {
#			if (status != 0) {
#				stop(paste("EMG finished with an error: ",status))
#			}
#			futile.logger::flog.info("EMG started successfully.", name = "dexr.hl.experiment.emg")
#			break
#		}	    
#	}	
}
#' Stop EMG clients. Disables SSL verification.
#' @param dexpa 
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
hl_experiment_stopemg <- function(dexpa) {
	futile.logger::flog.info("Stopping EMG...", name = "dexr.hl.experiment")
	try(httr::POST(paste(dexpa$emg$url,dexpa$emg$api$shutdown,sep="/"), httr::config(ssl_verifypeer = 0)))
	futile.logger::flog.info("Emg stopped.", name = "dexr.hl.experiment")
}
#' Concuct experiment from starting backend server to creation of full report
#' @param dexpa 
#' @return report, database
#' 
#' @author Sascha Holzhauer
#' @export
hl_experiment <- function(dexpa, shutdownmarket = F, basetime = as.numeric(round(Sys.time(),"mins"))*1000,
		offset = round(basetime - as.numeric(Sys.time())*1000), outputfile = "", outfilemarket = "", outfileemg = "") {
	
	futile.logger::flog.info("Basetime: %f", basetime)
	
	futile.logger::flog.info("Perform experiment for %s (output to %s)...", dexpa$sim$id, outputfile,
			name="dexr.hl.experiment")
	futile.logger::flog.info("Expected to finish at about %s.", format(Sys.time() + round(dp1$sim$duration/dp1$sim$timefactor), tz="CEST"),
			name="dexr.hl.experiment.duration")

	if (outputfile != "") {
		shbasic::sh.ensurePath(outputfile, stripFilename = T)
		con <- file(outputfile)
		sink(con, append=TRUE)
		sink(con, append=TRUE, type="message")
	}
	
	infoData <- hl_experiment_runbackend(dexpa, outfilesys = outfilemarket, basetime = basetime, offset = offset)
	
	hl_experiment_runemg(dexpa, outfilesys = outfileemg)
	
	futile.logger::flog.info("Wait for simulation to complete (Duration: %d / factor: %d = %f", dexpa$sim$duration, 
			dexpa$sim$timefactor, dexpa$sim$duration/dexpa$sim$timefactor, name = "dexr.hl.experiment")
	Sys.sleep(dexpa$sim$duration/dexpa$sim$timefactor)
	
	hl_experiment_stopemg(dexpa)
	
	dexR::input_db_db2dump(dexpa, dumpdir = paste("dump_", dexpa$sim$id, sep=""))

	try(dexR::createFullReport(dexpa, outputfile = paste("StageA_FullReport_", dexpa$sim$id, ".pdf", sep="")))
	
	server_shutdown(dexpa)
	
	if (outputfile != "") {
		sink()
		sink()
		sink()
		sink(type="message")
	}
	
	#require(readODS)
	#runinfos <- read_ods(dexpa$files$runinfos, sheet = 1)
	runinfos <- read.csv(dexpa$files$runinfos, header = TRUE, sep = ",", quote = "\"",
			dec = ".", fill = TRUE, comment.char = "")
	runinfo <- list()
	runinfo$RunNumber 	<- runinfos[nrow(runinfos), "RunNumber"] + 1
	runinfo$ID 			<- dexpa$sim$id
	runinfo$Date		<- format(Sys.time(), "%D")
	runinfo$Time		<- format(Sys.time(), "%H:%M:%S")
	runinfo$Stage		<- "SIM"
	runinfo$dexrVersion <- packageDescription("dexR")$Version
	
	runinfo$TF			<- dexpa$sim$timefactor
	runinfo$Basetime	<- format(as.POSIXlt(basetime/1000, origin = "1970-01-01"))
	runinfo$Offset		<- format(as.POSIXct(abs(offset/1000), origin = "1970-01-01"), "%jd %H:%M:%S")
	runinfo$Duration	<- dexpa$sim$duration/(60*60)
	runinfo$NumClients	<- infoData$numClients
	
	runinfos 			<- data.table::rbindlist(list(runinfos, runinfo), fill=T, use.names=T)
	
	
	
	#write_ods(runinfos, dexpa$files$runinfos)
	#write_ods(runinfos, "/daten/INES/X/DEX/data/DEX_Runs2.ods")
	write.csv(runinfos, dexpa$files$runinfos, row.names = F)
}
