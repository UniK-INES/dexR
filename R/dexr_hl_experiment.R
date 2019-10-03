#' Run backend server to experiment
#' 
#' Default for first delivery period start is the basetime.
#' 
#' @param dexpa parameter object
#' @param outfilesys log file
#' @param basetime in ms, assigned to \code{de.unik.enavi.market.time.basetime}
#' @param offset in ms, assigned to \code{de.unik.enavi.market.time.offset}
#' @param startServer TRUE if the server should also be started
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
hl_experiment_runbackend <- function(dexpa, outfilesys = "", basetime = as.numeric(round(Sys.time(),"mins"))*1000,
		offset = round(basetime - as.numeric(Sys.time())*1000), startServer=TRUE) {
	infoData <- list()
	futile.logger::flog.info("Starting Market Backend server (output to %s) with offset=%s/factor=%d/basetime=%s and with profile %s and %s...", 
			outfilesys,
			format(as.POSIXct(offset/1000, tz="GTM", origin = "1970-01-01"), "%H:%M:%S"),
			dexpa$sim$timefactor,
			as.POSIXlt(basetime/1000, origin = "1970-01-01"),
			dexpa$server$profile,
			paste("-Dlogback.configuration.file=", dexpa$server$logconfigfile, sep=""),
			name = "dexr.hl.experiment.runbackend")
	
	control <- hl_experiment_bootbackend(dexpa = dexpa, basetime = basetime, offset = offset, outfilesys = outfilesys);
	
	if (control >= dexpa$server$controls) {
		R.oo::throw.default("Starting market backend server NOT successful!")
	}
	
	paramConfigs <- dexR::input_csv_configparam(dexpa)
	
	hl_config_marketProducts2db(dexpa, 
			firstDeliveryPeriodStart = as.POSIXlt(basetime/1000, origin = "1970-01-01"),
			sourcefile = if(nrow(paramConfigs)>0) unique(paramConfigs["products"]) else 
						paste("DEX_Param_MarketProducts_", dexpa$sim$id, ".csv", sep=""))
	
	# Insert client data:
	infoData$numClients <- hl_config_clients2db(dexpa, paramConfigs = paramConfigs)
	
	if (startServer) {
		# Start backend server:
		message = server_start(dexpa)
		futile.logger::flog.info(message, name = "dexr.hl.experiment")	
	}

	return(infoData)
}
#' Boots backend server (no configuration, no starting)
#' 
#' @param dexpa 
#' 
#' @author Sascha Holzhauer
#' @export
hl_experiment_bootbackend <- function(dexpa, basetime, offset, outfilesys) {
	firstDelivery <- basetime + (max(dexpa$sim$firstdeliverystart$delay,
						dexpa$emg$restarttime * dexpa$sim$timefactor)) * 1000
	
	initialbasetime = basetime + (firstDelivery - (basetime + dexpa$sim$timefactor * dexpa$emg$emgstartuptime))/2
	
	paramConfigs = input_csv_configparam(dexpa)
	if (nrow(paramConfigs) > 0 && !is.na(paramConfigs["dexprofile"])) {
		dexpa$server$profile <- paramConfigs["dexprofile"]
	}
	
	futile.logger::flog.debug("Start DEX market backend with profile %s. Initial base time is %s.",
			dexpa$server$profile,
			format(as.POSIXct(initialbasetime/1000, tz="GTM", origin = "1970-01-01"), "%d/%m/%y %H:%M:%S"),
			name = "dexr.hl.experiment")
	
	# Instatiate server:
	# It's important that the -D parameters are before the <application>.jar otherwise they are not recognized.
	
	if (dexpa$server$usemvn) {
		arguments = paste('-f "', dexpa$files$backendPOM, '" spring-boot:run ',
				"-Dspring.profiles.active=", dexpa$server$profile, " ",
				"-Dspring.datasource.url=jdbc:postgresql://", dexpa$db$host,":", dexpa$db$port, "/", dexpa$db$dbname, " ",
				"-Dserver.port=", dexpa$server$port, " ",
				"-Dde.unik.enavi.market.testing.load=FALSE ",
				"-Dde.unik.enavi.market.time.factor=", dexpa$sim$timefactor, " ",
				"-Dde.unik.enavi.market.time.basetime=", format(basetime, scientific = FALSE), " ",
				if (dexpa$sim$setinitialbasetime) {paste(
				"-Dde.unik.enavi.market.time.basetime.initial=", format(initialbasetime, scientific = FALSE), " ",sep="")}, 
				"-Dde.unik.enavi.market.time.matchbasetime=", dexpa$server$matchbasetime, " ",
				"-Dde.unik.enavi.market.time.offset=", format(offset, scientific = FALSE), " ",
				'-Dlogback.configuration.file="', dexpa$server$logconfigfile, '"',  sep="")
		
		futile.logger::flog.debug("System2 command is %s.",
				paste("mvn", arguments),
				name = "dexr.hl.experiment")
		
		system2(wait=FALSE, "mvn", args=arguments,
				stdout=outfilesys, stderr=outfilesys)
	} else {
		system2(wait=FALSE, 'java', args=paste('-Dlogback.configuration.file="', dexpa$server$logconfigfile, '"', 
						' -jar "', dexpa$files$serverjar, '" ',
						'--spring.profiles.active=', dexpa$server$profile, ' ',
						'--spring.datasource.url=jdbc:postgresql://', dexpa$db$host,':', dexpa$db$port, '/', dexpa$db$dbname, ' ',
						'--server.port=', dexpa$server$port, ' ',
						'--de.unik.enavi.market.testing.load=FALSE ',
						'--de.unik.enavi.market.time.factor=', dexpa$sim$timefactor, ' ',
						'--de.unik.enavi.market.time.basetime=', format(basetime, scientific = FALSE), ' ',
						if (dexpa$sim$setinitialbasetime) {paste(
						'--Dde.unik.enavi.market.time.basetime.initial=', format(initialbasetime, scientific = FALSE), ' ',sep='')},
						'--de.unik.enavi.market.time.matchbasetime=', dexpa$server$matchbasetime, ' ',
						'--de.unik.enavi.market.time.offset=', format(offset, scientific = FALSE), sep=''),
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
	return(control)
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
		shbasic::sh.ensurePath(outfilesys, stripFilename = T)
		futile.logger::flog.info("outfilesys: %s (in hl_experiment_configemg)", outfilesys, name = "dexr.hl.experiment")
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
	
	# Check Runs.csv for requested simulation ID and node set ID:
	idMatch <- which(dexpa$sim$id == paramConfigs$ID)
	
	if(length(idMatch) > 1) {
		# more than one Node Set ID per Simulation ID:
		idMatch <- idMatch[match(dexpa$sim$nodesetid, paramConfigs[idMatch, "NodeSetId"])]
	}
	
	if(is.na(idMatch)) {
		futile.logger::flog.warn("ID %s not present in config table (%s)! Applying defaults in %s...", 
				dexpa$sim$id, 
				dexpa$files$paramconfigs,
				paste(dexpa$dirs$config, "/", dexpa$sim$id, sep=""),
				name = "dexr.hl.experiment.runemg")
	}

	iddirpart <- if(!is.na(dexpa$sim$nodesetid)) paste(dexpa$sim$id, "_", dexpa$sim$nodesetid, sep="") else dexpa$sim$id
	clientprefix <- if(!is.na(dexpa$sim$nodeid)) paste("n", dexpa$sim$nodeid, "_", sep="") else ""
			
	if (!is.na(idMatch)) {
		args = paste(' -cp "',
				dexpa$files$emgconfigtool, '" de.unik.ines.enavi.ctool.EmgConfigManager',
				' -i "', iddirpart, '"',
				' -cf "',clientprefix, '"',
				' -o "', dexpa$dirs$config, '"',
				' -t "', dexpa$dirs$freemarkertemplate, '"',
				' -c "', paste(dexpa$dirs$config, '/', combine_sourcedirfile(dexpa$sim$id, paramConfigs[idMatch,'clients']), sep=''), '"',
				' -l "', paste(dexpa$dirs$config, '/', combine_sourcedirfile(dexpa$sim$id, paramConfigs[idMatch,'loads']), sep=''), '"',
				' -g "', paste(dexpa$dirs$config, '/', combine_sourcedirfile(dexpa$sim$id, paramConfigs[idMatch,'generations']), sep=''), '"',
				' -b "', paste(dexpa$dirs$config, '/', combine_sourcedirfile(dexpa$sim$id, paramConfigs[idMatch,'buildings']), sep=''), '"',
				' -p "', paste(dexpa$dirs$config, '/', combine_sourcedirfile(dexpa$sim$id, paramConfigs[idMatch,'pvplants']), sep=''), '"',
				' -w "', paste(dexpa$dirs$config, '/', combine_sourcedirfile(dexpa$sim$id, paramConfigs[idMatch,'windplants']), sep=''), '"',
				' -s "', paste(dexpa$dirs$config, '/', combine_sourcedirfile(dexpa$sim$id, paramConfigs[idMatch,'loadProfiles']), sep=''), '"',
				' -d "', paste(dexpa$dirs$config, '/', combine_sourcedirfile(dexpa$sim$id, paramConfigs[idMatch,'storages']), sep=''), '"',
				' -dd "',paste(dexpa$dirs$config, '/', combine_sourcedirfile(dexpa$sim$id, paramConfigs[idMatch,'devicesStorage']), sep=''), '"',
				' -r "', paste(dexpa$dirs$config, '/', combine_sourcedirfile(dexpa$sim$id, paramConfigs[idMatch,'requestConfig']), sep=''), '"',
				' -sc "',paste(dexpa$dirs$config, '/', combine_sourcedirfile(dexpa$sim$id, paramConfigs[idMatch,'ogemaConfig']), sep=''), '"',
				' -pr "',paste(dexpa$dirs$config, '/', combine_sourcedirfile(dexpa$sim$id, paramConfigs[idMatch,'windpvpricing']), sep=''), '"',
				' -u "', dexpa$server$url, '"',
				' -po "',dexpa$server$port, '"',
				sep="")
		futile.logger::flog.debug("Arguments when calling EmgConfigManager: %s", args, name = "dexr.hl.experiment.emg")
		
		# Read CSV sources from DEX_Param_Configs.csv:
		system2(wait=TRUE, "java", args, stdout=outfilesys, stderr=outfilesys)
	} else {
		args = paste(' -cp "',
				dexpa$files$emgconfigtool, '" de.unik.ines.enavi.ctool.EmgConfigManager',
				' -i "', iddirpart, '"',
				' -cp "',clientprefix, '"',
				' -o "', dexpa$dirs$config, '"',
				' -t "', dexpa$dirs$freemarkertemplate, '"',
				' -c "', paste(dexpa$dirs$config, '/', dexpa$sim$id, '/DEX_Param_EnaviClient_', dexpa$sim$id, '.csv', sep=''), '"',
				' -l "', paste(dexpa$dirs$config, '/', dexpa$sim$id, '/DEX_Param_Loads_', dexpa$sim$id, '.csv', sep=''), '"',
				' -g "', paste(dexpa$dirs$config, '/', dexpa$sim$id, '/DEX_Param_Generations_', dexpa$sim$id, '.csv', sep=''), '"',
				' -b "', paste(dexpa$dirs$config, '/', dexpa$sim$id, '/DEX_Param_DevicesBuilding_', dexpa$sim$id, '.csv', sep=''), '"',
				' -p "', paste(dexpa$dirs$config, '/', dexpa$sim$id, '/DEX_Param_DevicesPVplant_', dexpa$sim$id, '.csv', sep=''), '"',
				' -w "', paste(dexpa$dirs$config, '/', dexpa$sim$id, '/DEX_Param_DevicesWindplant_', dexpa$sim$id, '.csv', sep=''), '"',
				' -s "', paste(dexpa$dirs$config, '/',  dexpa$sim$id, '/DEX_Param_LoadProfiles_', dexpa$sim$id, '.csv', sep=''), '"',
				' -d "', paste(dexpa$dirs$config, '/', dexpa$sim$id, '/DEX_Param_Storages_', dexpa$sim$id, '.csv', sep=''), '"',
				' -dd "', paste(dexpa$dirs$config, '/', dexpa$sim$id, '/DEX_Param_DevicesStorage_', dexpa$sim$id, '.csv', sep=''), '"',
				' -r "', paste(dexpa$dirs$config, '/', dexpa$sim$id, '/DEX_Param_RequestConfig_', dexpa$sim$id, '.csv', sep=''), '"',
				' -sc "', paste(dexpa$dirs$config, '/', dexpa$sim$id, '/DEX_Param_OgemaConfig_', dexpa$sim$id, '.csv', sep=''), '"',
				' -pr "', paste(dexpa$dirs$config, '/', dexpa$sim$id, '/DEX_Param_WindPvPricing_', dexpa$sim$id, '.csv', sep=''), '"',
				' -u "', dexpa$server$url, '"',
				' -po "',dexpa$server$port, '"',
				sep="")
		
		futile.logger::flog.debug("Arguments when calling EmgConfigManager: %s", args, name = "dexr.hl.experiment.emg")
		
		system2(wait=TRUE, "java", args, stdout=outfilesys, stderr=outfilesys)
	}
		
	# copy static XML files:
	for (f in dexpa$xml$staticfiles) {
		file.copy(from = paste(dexpa$dirs$xmltemplatesstatic, f, sep="/"), to = paste(dexpa$dirs$config, "/", dexpa$sim$id, "_", 
						dexpa$sim$nodesetid, "/", sep=""),
				overwrite = TRUE)
	}
}
#' Run EMGs to experiment
#' @param dexpa 
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
hl_experiment_runemg <- function(dexpa, outfileemg = "", outfilesys = "", pauseafterxmlcreation = F) {
	
	if (!dir.exists(paste(dexpa$dirs$config, "/", dexpa$sim$id, "_", dexpa$sim$nodeid, sep=""))) {
		hl_experiment_configemg(dexpa, outfilesys= if (is.null(dexpa$emg$emgconfigoutput)) "" else 
							paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, "_", dexpa$emg$emgconfigoutput, ".log", sep=""))
	}
	
	if (pauseafterxmlcreation) {
		decision <- svDialogs::dlg_message(paste("Press 'OK' when ready to run!", sep= ""), "okcancel")$res
		if (decision == "cancel") {
			futile.logger::flog.warn("Program canceled.", 
					name = "dexr.hl.opsim")
			stop("Program canceled.")
		}
	}
	
	paramConfigs = dexR::input_csv_configparam(dexpa)
	if (nrow(paramConfigs) > 0 && !is.na(paramConfigs["emgproperties"])) {
		dexpa$emg$propertiesfile <- paramConfigs["emgproperties"]
	}
	
	futile.logger::flog.info("Starting EMG with properties file %s...", 
			dexpa$emg$propertiesfile,
			name = "dexr.hl.experiment.emg")
	
	
	# copy rundir to node locally for every instance:
	if (!dexpa$sim$raspic && (dexpa$emg$copyrundir || dexpa$sim$multiplenodes)) {
		newrundir = paste(dexpa$dirs$emgnoderundir, "/", dexpa$sim$id, "_", dexpa$sim$nodesetid, "_", dexpa$sim$nodeid, "/", sep="")
		futile.logger::flog.debug("Copying EMG rundir from %s to %s...",
				dexpa$dirs$emgrundir,
				newrundir,
				name = "dexr.hl.experiment.emg")
		shbasic::sh.ensurePath(newrundir)
		file.copy(from=dexpa$dirs$emgrundir, to=newrundir, 
				overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
		dexpa$dirs$emgrundir = paste(newrundir, "/rundir-enavi", sep="")
	}
	
	#Sys.setenv(VMOPTS = paste("-Dorg.ogema.app.resadmin.replay_oncleanstart_path=", dexpa$dirs$config, dexpa$sim$id, sep=""))
	#setwd(dexpa$dirs$emgrundir)
	#system2(wait=FALSE, "bash", args = paste(
	#	"./start.sh -clean --properties config/sh_ogema.properties", sep=""))	

	if (dexpa$sim$raspic) {
		dexR::hl_raspic_transferemgconfig(dexpa)
	} else {}
	args = paste(' -cp ',
			paste('"', dexpa$files$emgconfigtool, '"', sep=""), "de.unik.ines.enavi.ctool.RunEmg", 
			paste('"', dexpa$dirs$config, "/", dexpa$sim$id, "_", dexpa$sim$nodeid, '"', sep=""),
			dexpa$emg$rseed,
			paste('"', dexpa$dirs$emgrundir, '"', sep=""),
			paste(dexpa$server$url,":", dexpa$server$port, "/", dexpa$server$api$submit, sep=""),
			dexpa$emg$port,
			dexpa$emg$httpport,
			dexpa$emg$propertiesfile,
			paste('"',dexpa$emg$startoptions,'"', sep=""))
	
	futile.logger::flog.debug("Arguments when calling RunEMG: %s. Output to %s.", args, outfileemg,
			name = "dexr.hl.experiment.emg")
	
	system2(wait=FALSE, "java", args,
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
#' @param dexpa parameter list
#' @param waittime (default: dexpa$emg$emgstartuptime)
#' @return -
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
	futile.logger::flog.info("Stopping EMG at %s:%s/%s...", dexpa$emg$url, dexpa$emg$port, dexpa$emg$api$shutdown, name = "dexr.hl.experiment")
	try(httr::POST(paste(dexpa$emg$url, ":", dexpa$emg$port, "/", dexpa$emg$api$shutdown,sep=""), 
					httr::config(ssl_verifypeer = 0)))
	futile.logger::flog.info("Emg stopped.", name = "dexr.hl.experiment")
	
	if (dexpa$emg$copyrundir) {
		# copy log file to home dir:
		futile.logger::flog.info("Copy log files %s (in %s) to home dir (%s)...",
		    paste(list.files(paste(paste(dexpa$dirs$emgnoderundir, dexpa$sim$id, sep="_"), dexpa$dirs$emglogdir, sep="/")), collapse="|"),
				paste(paste(dexpa$dirs$emgnoderundir, dexpa$sim$id, sep="_"),dexpa$dirs$emglogdir, sep="/"),
				paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/emg/", sep="/"),
				name = "dexr.hl.experiment")
	  
	  
		shbasic::sh.ensurePath(paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/emg/", sep="/"))
		file.copy(from=paste(paste(dexpa$dirs$emgnoderundir, dexpa$sim$id, sep="_"), dexpa$dirs$emglogdir, sep="/"), 
				to=paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/emg/", sep=""), 
				overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
		
		file.remove(paste(dexpa$dirs$emgnoderundir, dexpa$sim$id, sep="_"))
		futile.logger::flog.info("Emg-Rundir deleted.", name = "dexr.hl.experiment")
	}
}
#' Ensures file exists an redirects output.
#' 
#' @param dexpa 
#' @param outputfile  
#' @return 
#' 
#' @author Sascha Holzhauer
#' @export
hl_experiment_ensureFileLogging <- function(dexpa, outputfile) {
	if (outputfile != "") {
		futile.logger::flog.info("Ensures path for %s...", outputfile,
				name="dexr.hl.experiment")
		shbasic::sh.ensurePath(outputfile, stripFilename = T)
		con <- file(outputfile)
		sink(con, append=TRUE)
		sink(con, append=TRUE, type="message")
		
		futile.logger::flog.info("Perform experiment for %s (output to %s)...", dexpa$sim$id, outputfile,
				name="dexr.hl.experiment")
		futile.logger::flog.info("Expected to finish at about %s.", format(Sys.time() + 
								round((dexpa$sim$duration + dexpa$sim$firstdeliverystart$delay)/dexpa$sim$timefactor), tz="CEST"),
				name="dexr.hl.experiment.duration")
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
	runinfo$ServerVersion <-dexR::server_version(dexpa) 
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
#' Stop EMGs, dump database, create full report, shutdown server, and redirect output.
#' 
#' @param dexpa 
#' @return 
#' 
#' @author Sascha Holzhauer
#' @export
hl_closeexperiment <- function(dexpa, outputfile = "", basetime, offset = round(basetime - as.numeric(Sys.time())*1000), 
		infoData=NULL, nodeids = c("")) {
	
	for (nodeid in nodeids) {
		futile.logger::flog.info("Stopping EMG for node ID %s...", nodeid, name="dexr.hl.experiment.close")
		dexpa$emg$copyrundir = F
		dexpa$emg$port = emggetport(dexpa, nodeid)
		dexpa$emg$httpport = emggethttpport(dexpa, nodeid)
		hl_experiment_stopemg(dexpa)
	}
	
	if (!is.null(infoData)) {
		hl_write_runinfos(dexpa = dexpa, basetime = basetime, offset = offset, infoData=infoData)
	}
	
	dexR::input_db_db2dump(dexpa, dumpdir = paste("dump_", dexpa$sim$id, sep=""), remoteServer=dexpa$remoteserver, 
			outputfile= if (is.null(dexpa$db$sshoutput)) "" else 
						paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, "_", dexpa$db$sshoutput, ".log", sep=""))
	
	try(dexR::createFullReport(dexpa, outputfile = paste("StageA_FullReport_", dexpa$sim$id, ".pdf", sep="")))
	
	server_shutdown(dexpa)
	
	if (outputfile != "") {
		sink()
		sink()
		sink()
		sink(type="message")
	}
}
#' Concuct experiment from starting backend server to creation of full report
#' @param dexpa 
#' @return report, database
#' 
#' @author Sascha Holzhauer
#' @export
hl_experiment <- function(dexpa, basetime = as.numeric(round(Sys.time(),"mins"))*1000,
		offset = round(basetime - as.numeric(Sys.time())*1000),
		outputfile = paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, "_", dexpa$emg$rseed, ".log", sep=""),
		outfilemarket = paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, "_", dexpa$emg$rseed, "_market.log", sep=""),
		outfileemg = paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, "_", dexpa$emg$rseed, "_emg.log", sep=""),
		outfile, runmarket = T, shutdown = T, createdb = F, pauseafterxmlcreation = F) {
	
	futile.logger::flog.info("Perform experiment for %s (output to %s)...", dexpa$sim$id, outputfile,
			name="dexr.hl.experiment")
	futile.logger::flog.info("Expected to finish at about %s.", format(Sys.time() + 
							round((dexpa$sim$duration + dexpa$sim$firstdeliverystart$delay)/dexpa$sim$timefactor), tz="CEST"),
			name="dexr.hl.experiment.duration")

	if (createdb) {
		dexR::input_db_createdb(dexpa)
	}
	
	dexR::hl_experiment_ensureFileLogging(dexpa, outputfile)
	
	# whether there are multiple nodes needs to be checked before market backend configuration (client names):
	allnodeids = c()
	nodesetids = dexR::input_csv_configparam(dexpa)[,"NodeSetId"]
	dexpa$sim$nodesetids = nodesetids
	
	if (length(nodesetids)==0) {
		nodesetids = c(NA)
	}
	
	for (i in 1:length(nodesetids)) {
		# i = 1
		futile.logger::flog.info("Process node set ID %s...", nodesetids[i], name="dexr.hl.experiment")
		dexpa$sim$notesetid <- nodesetids[i]
		nodeids = strsplit(dexR::input_csv_configparam(dexpa, checkNodeSetId = T)[,"Nodes"], ';', fixed=T)[[1]]
		allnodeids = c(allnodeids, nodeids)
		
		# make sure nodeids are not twice in an experiment configuration:
		if (length(unique(allnodeids)) != length(allnodeids)) {
			R.oo::throw.default("Duplicate node IDs in experiment configuration: ", paste(allnodeids, collapse="/"), "!")
		}
	}
	
	dexpa$sim$multiplenodes = length(nodesetids) > 1 || (!is.na(nodeids) && length(nodeids) > 1)
	
	if (runmarket) {
		infoData <- hl_experiment_runbackend(dexpa, outfilesys = outfilemarket, basetime = basetime, offset = offset, startServer=F)
	}
	
	for (nodesetid in nodesetids) {
		# nodesetid = nodesetids[2]
		dexpa$sim$notesetid <- nodesetid
		nodeids = strsplit(dexR::input_csv_configparam(dexpa, checkNodeSetId = T)[,"Nodes"], ';', fixed=T)[[1]]
		
		# remove NodeSet-config-directory:
		futile.logger::flog.info("NotSetId %s: Remove config directory %s...",
				as.character(nodesetid),
				paste(dexpa$dirs$config, "/", dexpa$sim$id,  if (nodesetid != "") "_", nodesetid, sep=""),
				name="dexr.hl.experiment")
		unlink(paste(dexpa$dirs$config, "/", dexpa$sim$id, if (nodesetid != "") "_", nodesetid, sep=""), recursive = TRUE, force = FALSE)
		
		if (is.na(nodeids)) nodeids = c("")
		
		if (dexpa$sim$raspic) {
			session <- dexR::hl_raspic_transferemgconfig(dexpa)
			dexR::hl_raspic_runemg(dexpa, session)
			dexR::hl_raspic_closesession(dexpa, session)
		} else {
		
			for (nodeid in nodeids) {
				# nodeid = nodeids[1]
				dexpan = dexpa
				dexpan$sim$nodesetid = nodesetid
				dexpan$sim$nodeid = nodeid
				dexpan$emg$port = dexR::emggetport(dexpa, nodeid)
				dexpan$emg$httpport = dexR::emggethttpport(dexpa, nodeid)
				dexpan$emg$emgconfigoutput = if(is.null(dexpa$emg$emgconfigoutput)) "" else paste(dexpa$emg$emgconfigoutput, 
									if (!is.na(nodeid)) nodeid, sep="_")
				hl_experiment_runemg(dexpan, 
						outfileemg = paste(tools::file_path_sans_ext(outfileemg), if (!is.na(nodesetid))  "_n", if (!is.na(nodesetid)) nodesetid, 
								if (nodeid != "") "-", nodeid, ".log", sep=""),
						pauseafterxmlcreation = pauseafterxmlcreation)
			}
		}
	}
	
	message = server_start(dexpa)
	futile.logger::flog.info(message, name = "dexr.hl.experiment")	
	
	# hl_experiment_awaitemgstartup(dexpa, waittime = dexpa$emg$restarttime)
	
	futile.logger::flog.info("Wait for simulation to complete (Duration: %d / factor: %d = %f)", 
			(dexpa$sim$duration + dexpa$sim$firstdeliverystart$delay), 
			dexpa$sim$timefactor, (dexpa$sim$duration + dexpa$sim$firstdeliverystart$delay) / dexpa$sim$timefactor, name = "dexr.hl.experiment")
	
	if (shutdown) {
		Sys.sleep((dexpa$sim$duration + dexpa$sim$firstdeliverystart$delay)/dexpa$sim$timefactor)
		dexR::hl_closeexperiment(dexpa, outputfile = outputfile, basetime = basetime, offset = offset, infoData = infoData, nodeids = allnodeids)
	}
	return(allnodeids)
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
		outputfile = paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, "_", dexpa$emg$rseed, ".log", sep=""), 
		outfilemarket = paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, "_", dexpa$emg$rseed, "_market.log", sep=""),
		outfileemg = paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, "_", dexpa$emg$rseed, "_emg.log", sep="")) {
	
	shbasic::sh.ensurePath(paste(dexpa$dirs$config, dexpa$sim$id,sep="/"))
	
	dexpa$db$dbname		= dexpa$sim$id
	dexpa$server$port 	= dexpa$server$startport +  as.numeric(strsplit(dexpa$sim$id, "-")[[1]][2]) + dexpa$server$portoffset
	futile.logger::flog.debug("Set dexpa$server$port to: %d", dexpa$server$port, name="dexr.hl.experiment.cluster")
	
	dexpa$emg$port 		= dexpa$emg$startport +  as.numeric(strsplit(dexpa$sim$id, "-")[[1]][2]) + dexpa$emg$portoffset
	futile.logger::flog.debug("Set dexpa$emg$port to: %d", dexpa$emg$port, name="dexr.hl.experiment.cluster")
	
	dexpa$emg$httpport 		= dexpa$emg$httpstartport +  as.numeric(strsplit(dexpa$sim$id, "-")[[1]][2]) + dexpa$emg$httpportoffset
	futile.logger::flog.debug("Set dexpa$emg$httpport to: %d", dexpa$emg$httpport, name="dexr.hl.experiment.cluster")
	
	dexR::input_db_createdb(dexpa)
	
	dexR::hl_experiment(dexpa=dexpa, shutdown = T, basetime = basetime, offset = offset, 
			outputfile = outputfile, outfilemarket = outfilemarket, outfileemg = outfileemg)
	
	dexR::input_db_dropdb(dexpa)
}
