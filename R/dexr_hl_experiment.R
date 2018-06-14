#' Run backend server to experiment
#' @param dexpa 
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
hl_experiment_runbackend <- function(dexpa, outfilesys = "", basetime = as.numeric(round(Sys.time(),"mins"))*1000) {
	futile.logger::flog.info("Starting Market Backend server (output to %s)...", outfilesys, 
			name = "dexr.hl.experiment")
	# Instatiate server:
	system2(wait=FALSE, "mvn", args=paste("-f ", dexpa$dirs$backend, " spring-boot:run ",
					"-Dde.unik.enavi.market.testing.load=FALSE ",
					"-Dde.unik.enavi.market.time.factor=", dexpa$sim$timefactor, " ",
					"-Dde.unik.enavi.market.time.basetime=", round(as.double(basetime)*1000), sep=""),
			stdout=outfilesys, stderr=outfilesys)
	
	control = 0
	while (!server_isrunning(dexpa) && control < 20) {
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
	hl_config_marketProducts2db(dexpa)
	
	# Insert client data:
	hl_config_clients2db(dexpa)
	
	# Start backend server:
	message = server_start(dexpa)
	futile.logger::flog.info(message, name = "dexr.hl.experiment")
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
	system2(wait=TRUE, "mvn", args = paste(" exec:java "," -Dexec.mainClass=de.unik.ines.enavi.ctool.EmgConfigManager"," -Dexec.args=\"", 
		" -i ", dexpa$sim$id,
		" -o '", dexpa$dirs$config, "'",
		" -t '", dexpa$dirs$freemarkertemplate, "'",
		" -c '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/DEX_Param_EnaviClient_", dexpa$sim$id, ".csv", sep=""), "'",
		" -l '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/DEX_Param_Loads_", dexpa$sim$id, ".csv", sep=""), "'",
		" -g '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/DEX_Param_Generations_", dexpa$sim$id, ".csv", sep=""), "'",
		" -b '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/DEX_Param_DevicesBuilding_", dexpa$sim$id, ".csv", sep=""), "'",
		" -p '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/DEX_Param_DevicesPVplant_", dexpa$sim$id, ".csv", sep=""), "'",
		" -s '", paste(dexpa$dirs$config, "/",  dexpa$sim$id, "/DEX_Param_LoadProfiles_", dexpa$sim$id, ".csv", sep=""), "'",
		" -d '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/DEX_Param_Storages_", dexpa$sim$id, ".csv", sep=""), "'",
		" -dd '", paste(dexpa$dirs$config, "/", dexpa$sim$id, "/DEX_Param_DevicesStorage_", dexpa$sim$id, ".csv", sep=""), "'",
	 "\"", sep=""))


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

	#system2(wait=FALSE, "java", args = paste("de.unik.ines.enavi.ctool.RunEmg ", round(dexpa$sim$duration/dexpa$sim$timefactor), " ",
	#	dexpa$dirs$config, dexpa$sim$id, sep=""),
	#	stdout=outfilesys, stderr=outfilesys)

	# https://www.rdocumentation.org/packages/sys/versions/1.5/topics/exec
	pid = sys::exec_background("java", args = c("de.unik.ines.enavi.ctool.RunEmg", 
					paste(dexpa$dirs$config, "/", dexpa$sim$id, sep=""), dexpa$dirs$emgrundir),
			std_out=outfilesys, std_err=outfilesys)
	
	setwd(path)
	futile.logger::flog.info("Wait during EMG startup (%d sec)...", dexpa$emg$emgstartuptime, name = "dexr.hl.experiment")
	max_sleep = dexpa$emg$emgstartuptime
	num_iterations = as.integer(max_sleep / 5) + 1
	# make sure not to wait for the full startup time in case an error occurs
	for (i in 1:num_iterations) {
		Sys.sleep(5)
		status = sys::exec_status(pid, wait = FALSE)
		if (!is.na(status)) {
			if (status != 0) {
				stop(paste("Emg finished with an error: ",status))
			}
			futile.logger::flog.info("EMG started successfully.", name = "dexr.hl.experiment.emg")
			break
		}	    
	}	
}
#' Stop EMG clients. Disables SSL verification.
#' @param dexpa 
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
hl_experiment_stopemg <- function(dexpa) {
	futile.logger::flog.info("Stopping EMG...", name = "dexr.hl.experiment")
	httr::POST(paste(dexpa$emg$url,dexpa$emg$api$shutdown,sep="/"), httr::config(ssl_verifypeer = 0))
	futile.logger::flog.info("Emg stopped.", name = "dexr.hl.experiment")
}
#' Concuct experiment from starting backend server to creation of full report
#' @param dexpa 
#' @return report, database
#' 
#' @author Sascha Holzhauer
#' @export
hl_experiment <- function(dexpa, shutdownmarket = F, basetime = as.numeric(round(Sys.time(),"mins"))*1000,
		outputfile = "", outfilemarket = "", outfileemg = "") {
	
	futile.logger::flog.info("Perform experiment for %s (output to %s)...", dexpa$sim$id, outputfile,
			name="dexr.hl.experiment")

	if (outputfile != "") {
		shbasic::sh.ensurePath(outputfile, stripFilename = T)
		con <- file(outputfile)
		sink(con, append=TRUE)
		sink(con, append=TRUE, type="message")
	}
	
	hl_experiment_runbackend(dexpa, outfilesys = outfilemarket, basetime = basetime)
	
	hl_experiment_runemg(dexpa, outfilesys = outfileemg)
	
	futile.logger::flog.info("Wait for simulation to complete (Duration: %d / factor: %d = %d", dexpa$sim$duration, 
			dexpa$sim$timefactor, dexpa$sim$duration/dexpa$sim$timefactor, name = "dexr.hl.experiment")
	Sys.sleep(dexpa$sim$duration/dexpa$sim$timefactor)
	
	hl_experiment_stopemg(dexpa)
	
	dexR::input_db_db2dump(dexpa, dumpdir = paste("dump_", dexpa$sim$id, sep=""))

	try(dexR::createFullReport(dexpa, outputfile = paste("StageA_FullReport_", dexpa$sim$id, ".pdf", sep="")))
	
	server_shutdown(dexpa)
	
	if (outputfile != "") {
		sink()
		sink()
		sink(type="message")
	}
}
