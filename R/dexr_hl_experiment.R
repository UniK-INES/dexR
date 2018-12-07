#' Run backend server to experiment
#' @param dexpa parameter object
#' @param outfilesys log file
#' @param basetime in ms, assigned to \code{de.unik.enavi.market.time.basetime}
#' @param offset in ms, assigned to \code{de.unik.enavi.market.time.offset}
#' @param startServer
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
hl_experiment_runbackend <- function(dexpa, outfilesys = "", basetime = as.numeric(round(Sys.time(),"mins"))*1000,
		offset = round(basetime - as.numeric(Sys.time())*1000), startServer=TRUE) {
	infoData <- list()
	futile.logger::flog.info("Starting Market Backend server (output to %s) with offset=%s/factor=%d/basetime=%s and with profile %s...", 
			outfilesys,
			format(as.POSIXct(offset/1000, tz="GTM", origin = "1970-01-01"), "%H:%M:%S"),
			dexpa$sim$timefactor,
			as.POSIXlt(basetime/1000, origin = "1970-01-01"),
			dexpa$server$profile,
			name = "dexr.hl.experiment.runbackend")
	
	# Instatiate server:
	# It's important that the -D parameters are before the <application>.jar otherwise they are not recognized.
	if (dexpa$server$usemvn) {
		system2(wait=FALSE, "mvn", args=paste("-f ", dexpa$dirs$backend, " spring-boot:run ",
						"-Dspring.profiles.active=", dexpa$server$profile, " ",
						"-Dspring.datasource.url=jdbc:postgresql://", dexpa$db$host,":", dexpa$db$port, "/", dexpa$db$dbname, " ",
						"-Dserver.port=", dexpa$server$port, " ",
						"-Dde.unik.enavi.market.testing.load=FALSE ",
						"-Dde.unik.enavi.market.time.factor=", dexpa$sim$timefactor, " ",
						"-Dde.unik.enavi.market.time.basetime=", format(basetime, scientific = FALSE), " ", 
						"-Dde.unik.enavi.market.time.matchbasetime=", dexpa$server$matchbasetime, " ",
						"-Dde.unik.enavi.market.time.offset=", format(offset, scientific = FALSE), sep=""),
				stdout=outfilesys, stderr=outfilesys)
	} else {
		system2(wait=FALSE, "java", args=paste("-jar ", dexpa$files$serverjar, " ",
						"--spring.profiles.active=", dexpa$server$profile, " ",
						"--spring.datasource.url=jdbc:postgresql://", dexpa$db$host,":", dexpa$db$port, "/", dexpa$db$dbname, " ",
						"--server.port=", dexpa$server$port, " ",
						"--de.unik.enavi.market.testing.load=FALSE ",
						"--de.unik.enavi.market.time.factor=", dexpa$sim$timefactor, " ",
						"--de.unik.enavi.market.time.basetime=", format(basetime, scientific = FALSE), " ", 
						"--de.unik.enavi.market.time.matchbasetime=", dexpa$server$matchbasetime, " ",
						"--de.unik.enavi.market.time.offset=", format(offset, scientific = FALSE), sep=""),
				stdout=outfilesys, stderr=outfilesys)
	}
	control = 0
	while (!server_isrunning(dexpa) && control < dexpa$server$controls) {
		# wait...
		futile.logger::flog.info("Again, check availability of market backend server...",
				name = "dexr.hl.experiment")
		Sys.sleep(dexpa$server$controlinterval)
		control = control + 1
	}
	
	if (control >= dexpa$server$controls) {
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
	
	if (startServer) {
		# Start backend server:
		message = server_start(dexpa)
		futile.logger::flog.info(message, name = "dexr.hl.experiment")	
	}

	return(infoData)
}
#' Create configuration for EMGs to experiment
#' @param dexpa 
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
hl_experiment_configemg <- function(dexpa, outfilesys = "") {
	
	## create XML configuration:
	if (outfilesys == "") {
		outfilesystxt = "CONSOLE"
	} else {
		outfilesystxt = outfilesys
	}
	
	futile.logger::flog.info("Creating EMG XML configuration (output to %s)...", outfilesystxt, name = "dexr.hl.experiment")
	
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
		system2(wait=TRUE, "java", args = paste(" -cp ",
						dexpa$files$emgconfigtool, " de.unik.ines.enavi.ctool.EmgConfigManager",
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
						" -po '",dexpa$server$port, "'",
						sep=""), stdout=outfilesys, stderr=outfilesys)
	} else {	
		system2(wait=TRUE, "java", args = paste(" -cp ",
						dexpa$files$emgconfigtool, " de.unik.ines.enavi.ctool.EmgConfigManager",
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
						" -po '",dexpa$server$port, "'",
						sep=""), stdout=outfilesys, stderr=outfilesys)
	}
		
	# copy static XML files:
	for (f in dexpa$xml$staticfiles) {
		file.copy(from = paste(dexpa$dirs$xmltemplatesstatic, f, sep="/"), to = paste(dexpa$dirs$config, "/", dexpa$sim$id, sep=""),
				overwrite = TRUE)
	}
}
#' Run EMGs to experiment
#' @param dexpa 
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
hl_experiment_runemg <- function(dexpa, outfileemg = "", outfilesys = "") {
	
	hl_experiment_configemg(dexpa, outfilesys= if (is.null(dexpa$emg$emgconfigoutput)) "" else 
						paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "_", dexpa$emg$emgconfigoutput, ".log", sep=""))

	futile.logger::flog.info("Starting EMG...", name = "dexr.hl.experiment.emg")
	
	
	# copy rundir to node locally for every instance:
	if (dexpa$emg$copyrundir) {
		futile.logger::flog.debug("Copying EMG rundir from %s to %s...",
				dexpa$dirs$emgrundir,
				paste(dexpa$dirs$emgnoderundir, dexpa$sim$id, sep="_"),
				name = "dexr.hl.experiment.emg")
		shbasic::sh.ensurePath(paste(dexpa$dirs$emgnoderundir, dexpa$sim$id, sep="_"))
		file.copy(from=dexpa$dirs$emgrundir, to=paste(dexpa$dirs$emgnoderundir, dexpa$sim$id, sep="_"), 
				overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
		dexpa$dirs$emgrundir = paste(dexpa$dirs$emgnoderundir, "_", dexpa$sim$id, "/rundir-enavi", sep="")
	}
	
	#Sys.setenv(VMOPTS = paste("-Dorg.ogema.app.resadmin.replay_oncleanstart_path=", dexpa$dirs$config, dexpa$sim$id, sep=""))
	#setwd(dexpa$dirs$emgrundir)
	#system2(wait=FALSE, "bash", args = paste(
	#	"./start.sh -clean --properties config/sh_ogema.properties", sep=""))	

	system2(wait=FALSE, "java", args = paste(" -cp ",
					dexpa$files$emgconfigtool, "de.unik.ines.enavi.ctool.RunEmg", 
			paste(dexpa$dirs$config, "/", dexpa$sim$id, sep=""),
			dexpa$emg$rseed,
			dexpa$dirs$emgrundir,
			paste(dexpa$server$url,":", dexpa$server$port, "/", dexpa$server$api$submit, sep=""),
			dexpa$emg$port,
			dexpa$emg$httpport,
			dexpa$emg$startoptions),
			stdout=outfileemg, stderr=outfileemg)

	# https://www.rdocumentation.org/packages/sys/versions/1.5/topics/exec
	#	pid = sys::exec_background("java", args = c("de.unik.ines.enavi.ctool.RunEmg", 	
	#					paste(dexpa$dirs$config, "/", dexpa$sim$id, sep=""), dexpa$dirs$emgrundir),
	#			std_out=outfilesys, std_err=outfilesys)
	
	hl_experiment_awaitemgstartup(dexpa)
	
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
#' Sleep during EMG startup
#' @param dexpa
#' @param waittime (default: dexpa$emg$emgstartuptime)
#' @return 
#' 
#' @author Sascha Holzhauer
#' @export
hl_experiment_awaitemgstartup <- function(dexpa, waittime = dexpa$emg$emgstartuptime) {
	futile.logger::flog.info("Wait during EMG startup (%d sec)...", waittime, name = "dexr.hl.experiment")
	Sys.sleep(waittime)	
}
#' Stop EMG clients. Disables SSL verification.
#' @param dexpa 
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
hl_experiment_stopemg <- function(dexpa) {
	futile.logger::flog.info("Stopping EMG...", name = "dexr.hl.experiment")
	try(httr::POST(paste(dexpa$emg$url, ":", dexpa$emg$port, "/", dexpa$emg$api$shutdown,sep=""), 
					httr::config(ssl_verifypeer = 0)))
	futile.logger::flog.info("Emg stopped.", name = "dexr.hl.experiment")
	
	if (dexpa$emg$copyrundir) {
		# copy log file to home dir:
		futile.logger::flog.info("Copy log files (in %s) to home dir (%s)...",
				paste(dexpa$dirs$emgnoderundir,dexpa$dirs$emglogdir, sep="/"),
				paste(dexpa$dirs$output$logs, "/emg/", dexpa$sim$id, sep="/"),
				name = "dexr.hl.experiment")
		shbasic::sh.ensurePath(paste(dexpa$dirs$output$logs, "/emg/", dexpa$sim$id, sep="/"))
		file.copy(from=paste(dexpa$dirs$emgnoderundir, dexpa$dirs$emglogdir, sep="/"), 
				to=paste(dexpa$dirs$output$logs, "/emg/", dexpa$sim$id, sep=""), 
				overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
		
		file.remove(paste(dexpa$dirs$emgnoderundir, dexpa$sim$id, sep="_"))
		futile.logger::flog.info("Emg-Rundir deleted.", name = "dexr.hl.experiment")
	}
}
#' Append current run information to runInfos file.
#' @param dexpa 
#' @param basetime 
#' @param offset 
#' @param infoData 
#' @return append runInfos file
#' 
#' @author Sascha Holzhauer
#' @export
hl_write_runinfos <- function(dexpa, basetime, offset, infoData) {
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
	runinfo$Basetime	<- format(as.POSIXlt(basetime/1000, origin = "1970-01-01"), tz="UTC")
	# %j gives 1 for the first day - not correct for duration
	runinfo$OffsetHR	<- paste(if (offset<0) "-" else "", format(as.POSIXct(abs(offset)/1000, origin = "1969-12-31", tz="UTC"), 
					"%jd %HH %MM %SS"), sep="")
	runinfo$Offset		<- offset
	runinfo$Duration	<- dexpa$sim$duration/(60*60)
	runinfo$NumClients	<- infoData$numClients
	
	runinfos 			<- data.table::rbindlist(list(runinfos, runinfo), fill=T, use.names=T)
	
	#write_ods(runinfos, dexpa$files$runinfos)
	write.csv(runinfos, dexpa$files$runinfos, row.names = F)
}
#' Concuct experiment from starting backend server to creation of full report
#' @param dexpa 
#' @return report, database
#' 
#' @author Sascha Holzhauer
#' @export
hl_experiment <- function(dexpa, shutdownmarket = F, basetime = as.numeric(round(Sys.time(),"mins"))*1000,
		offset = round(basetime - as.numeric(Sys.time())*1000), outputfile = "", outfilemarket = "", outfileemg = "", outfile) {
	
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
	
	infoData <- hl_experiment_runbackend(dexpa, outfilesys = outfilemarket, basetime = basetime, offset = offset, startServer=F)
	
	hl_experiment_runemg(dexpa, outfileemg = outfileemg, outfilesys = outputfile)
	
	message = server_start(dexpa)
	futile.logger::flog.info(message, name = "dexr.hl.experiment")	
	
	# hl_experiment_awaitemgstartup(dexpa, waittime = dexpa$emg$restarttime)
	
	futile.logger::flog.info("Wait for simulation to complete (Duration: %d / factor: %d = %f)", 
			(dexpa$sim$duration + dexpa$sim$firstdeliverystart$delay), 
			dexpa$sim$timefactor, (dexpa$sim$duration + dexpa$sim$firstdeliverystart$delay), name = "dexr.hl.experiment")
	Sys.sleep((dexpa$sim$duration + dexpa$sim$firstdeliverystart$delay)/dexpa$sim$timefactor)
	
	hl_experiment_stopemg(dexpa)
	
	dexR::input_db_db2dump(dexpa, dumpdir = paste("dump_", dexpa$sim$id, sep=""), remoteServer=dexpa$remoteserver, 
			outputfile= if (is.null(dexpa$db$sshoutput)) "" else 
						paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "_", dexpa$db$sshoutput, ".log", sep=""))

	try(dexR::createFullReport(dexpa, outputfile = paste("StageA_FullReport_", dexpa$sim$id, ".pdf", sep="")))
	
	server_shutdown(dexpa)
	
	if (outputfile != "") {
		sink()
		sink()
		sink()
		sink(type="message")
	}
	
	hl_write_runinfos(dexpa, basetime, offset, infoData)
}
#' Runs experiments and create the required config folder (for cluster execution)
#' 
#' Also sets reasonable default values for log files for this application.
#' 
#' @param dexpa 
#' @param basetime 
#' @param offset 
#' @param outputfile 
#' @param outfilemarket 
#' @param outfileemg  
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
hl_experiment_cluster <- function(dexpa, basetime = as.numeric(round(Sys.time(),"mins"))*1000,
		offset = round(basetime - as.numeric(Sys.time())*1000), 
		outputfile = paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "_", dexpa$emg$rseed, ".log", sep=""), 
		outfilemarket = paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "_", dexpa$emg$rseed, "_market.log", sep=""),
		outfileemg = paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "_", dexpa$emg$rseed, "_emg.log", sep="")) {
	
	shbasic::sh.ensurePath(paste(dexpa$dirs$config, dexpa$sim$id,sep="/"))
	
	dexpa$db$dbname		= dexpa$sim$id
	dexpa$server$port 	= dexpa$server$startport +  as.numeric(strsplit(dexpa$sim$id, "-")[[1]][2]) + dexpa$server$portoffset
	futile.logger::flog.debug("Set dexpa$server$port to: %d", dexpa$server$port, name="dexr.hl.experiment.cluster")
	
	dexpa$emg$port 		= dexpa$emg$startport +  as.numeric(strsplit(dexpa$sim$id, "-")[[1]][2]) + dexpa$emg$portoffset
	futile.logger::flog.debug("Set dexpa$emg$port to: %d", dexpa$emg$port, name="dexr.hl.experiment.cluster")
	
	dexpa$emg$httpport 		= dexpa$emg$httpstartport +  as.numeric(strsplit(dexpa$sim$id, "-")[[1]][2]) + dexpa$emg$httpportoffset
	futile.logger::flog.debug("Set dexpa$emg$httpport to: %d", dexpa$emg$httpport, name="dexr.hl.experiment.cluster")
	
	dexR::input_db_createdb(dexpa)
	
	dexR::hl_experiment(dexpa=dexpa, shutdownmarket = T, basetime = basetime, offset = offset, 
			outputfile = outputfile, outfilemarket = outfilemarket, outfileemg = outfileemg)
	
	dexR::input_db_dropdb(dexpa)
}
