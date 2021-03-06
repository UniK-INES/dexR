#' Runs OpSim control manager
#' 
#' @param dexpa 
#' @param outfileopsim 
#' @return run OpSim control manager
#' 
#' @author Sascha Holzhauer
#' @export
hl_opsim_runmanager <- function(dexpa, outfileopsim = paste(dexpa$dirs$output$logs, dexpa$sim$id, dexpa$opsim$control$logfile, sep="/")) {
	
	futile.logger::flog.info("Run opsim (%s) in %s\nwith JRE %s \nand arguments %s.\nLogging to %s...",
			dexpa$opsim$control$jar,
			dexpa$opsim$control$rundir,
			dexpa$opsim$control$jre,
			dexpa$opsim$control$args,
			outfileopsim,
			name = "dexr.hl.opsim.control")
	
	futile.logger::flog.debug("Set working directory to %s...", 
			dexpa$opsim$control$rundir,
			name = "dexr.hl.opsim.control")
	
	setwd(dexpa$opsim$control$rundir)
	
	shbasic::sh.ensurePath(outfileopsim, stripFilename = T)
	
	java <- if (is.na(dexpa$opsim$control$jre)) "java" else paste(dexpa$opsim$control$jre, "bin", "java", sep="/")
	
	system2(wait=FALSE, java, 
			args = paste(dexpa$opsim$control$args, ' -jar "',
					dexpa$opsim$control$jar, '"', sep=""),
			stdout=outfileopsim, stderr=outfileopsim)
}
#' Run Schedule Service
#' @param dexpa 
#' @param outfilesservice 
#' @return run Schedule Service
#' 
#' @author Sascha Holzhauer
#' @export
hl_opsim_runscheduleservice <- function(dexpa, outfilesservice = paste(dexpa$dirs$output$logs, dexpa$sim$id,
				dexpa$opsim$sservice$logfile, sep="/")) {

	futile.logger::flog.info("Run Schedule Service (%s)\nin %s\nwith JRE %s \narguments %s.\nLogging to %s...",
			dexpa$opsim$sservice$jar,
			dexpa$opsim$sservice$rundir,
			dexpa$opsim$sservice$jre,
			dexpa$opsim$sservice$args,
			outfilesservice,
			name = "dexr.hl.opsim.sservice")
	
	futile.logger::flog.debug("Set working directory to %s...", 
			dexpa$opsim$sservice$rundir,
			name = "dexr.hl.opsim.sservice")
	
	setwd(dexpa$opsim$sservice$rundir)
	
	shbasic::sh.ensurePath(outfilesservice, stripFilename = T)
	
	java <- if (is.na(dexpa$opsim$sservice$jre)) "java" else paste(dexpa$opsim$sservice$jre, "bin", "java", sep="/")
	args <- paste(dexpa$opsim$sservice$args, ' -jar "', dexpa$opsim$sservice$jar, '"', sep="")
	# args <- c(dexpa$opsim$sservice$args, paste(' -jar', dexpa$opsim$sservice$jar))
	
	#proc <- processx::process$new(java, args, stdout = outfilesservice, stderr=outfilesservice)
	#proc <- processx::process$new("java", "--jar /home/sascha/git/enavi/scheduleService/target/scheduleService-1.0.0.jar", stdout = outfilesservice, stderr=outfilesservice)
	
	system2(wait=FALSE, java, 
			args = args,
			stdout=outfilesservice, stderr=outfilesservice)
	
	#return(proc)
}
#' Run panda power net simulation in python
#' @param dexpa 
#' @param outfilenetsim 
#' @return run panda power
#' 
#' @author Sascha Holzhauer
#' @export
hl_opsim_runnetsim <- function(dexpa, outfilenetsim = paste(dexpa$dirs$output$logs, dexpa$sim$id,
				dexpa$opsim$netsim$logfile, sep="/")) {
	futile.logger::flog.info("Run Net simulation (%s)\nwith %s\nin %s.\nLogging to %s...",
			dexpa$opsim$netsim$module,
			dexpa$opsim$netsim$python,
			dexpa$opsim$netsim$rundir,
			outfilenetsim,
			name = "dexr.hl.opsim.netsim")
	
	futile.logger::flog.debug("Set working directory to %s...", 
			dexpa$opsim$netsim$rundir,
			name = "dexr.hl.opsim.netsim")
	
	setwd(dexpa$opsim$netsim$rundir)
	python <- if (is.na(dexpa$opsim$netsim$python)) "python" else dexpa$opsim$netsim$python
	
	shbasic::sh.ensurePath(outfilenetsim, stripFilename = T)
	
	# PYTHONPATH??
	Sys.setenv("PYTHONPATH"=dexpa$opsim$netsim$pythonpath)
	system2(wait=FALSE, python, 
			args = dexpa$opsim$netsim$module,
			stdout=outfilenetsim, stderr=outfilenetsim)
}
#' Start market backend server (creating database, not shutting down).
#' 
#' @param dexpa 
#' @return
#' 
#' @author Sascha Holzhauer
#' @export
hl_opsim_startmarket <- function(dexpa, basetime = as.numeric(round(strptime("30/09/19 12:00", "%d/%m/%y %H:%M"),"mins"))*1000,
		runemg=T, shutdown = F) {
	dexR::hl_experiment(dexpa, outputfile=paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, ".log", sep=""),
			outfilemarket = paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, "_market.log", sep=""),
			outfileemg = paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, "_emg.log", sep=""),
			basetime = basetime, shutdown = shutdown, createdb = T, runemg=runemg)
}
#' Run OpSim control manager, schedule service, net simulation, DEX market backend, and EMG clients
#' 
#' @param dexpa 
#' @return run DEX with OpSIM 
#' 
#' @author Sascha Holzhauer
#' @export
hl_opsim <- function(dexpa, startmanager = T, startsservice = T, startnetsim = T, emptydb = T, runemg=T,
		basetime = as.numeric(round(strptime("30/09/19 12:00", "%d/%m/%y %H:%M"),"mins"))*1000,
		outputfile = paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, ".log", sep=""),
		shutdown = F) {
	
	dexpa <- dexR::hl_experiment_ensureFileLogging(dexpa, outputfile)
	
	if (startmanager) {
		dexR::hl_opsim_runmanager(dexpa)
		
		decision <- svDialogs::dlg_message("Press 'OK' when Control Manager ready!", "okcancel")$res
		if (decision == "cancel") {
			futile.logger::flog.warn("Program canceled.", 
					name = "dexr.hl.opsim")
			stop("Program canceled.")
		}
	}	

	
	if (startsservice) {
		# proc_sservice = dexR::hl_opsim_runscheduleservice(dexpa)
		dexR::hl_opsim_runscheduleservice(dexpa)
	}
	
	if (startnetsim) {
		dexR::hl_opsim_runnetsim(dexpa)
	}
	
	decision <- svDialogs::dlg_message(paste("Press 'OK' when ", if (startsservice) "Schedule Service", if (startnetsim && startsservice) " and ", 
						if (startnetsim) "Net Simulation", " is/are ready and OpSim is running!", sep= ""), "okcancel")$res
	if (decision == "cancel") {
		futile.logger::flog.warn("Program canceled.", 
				name = "dexr.hl.opsim")
		stop("Program canceled.")
	}
	
	if (emptydb) {
		futile.logger::flog.info("Empty database of schedule service...", name = "dexr.hl.opsim.sservice")
		response = try(httr::DELETE(paste(dexpa$opsim$sservice$url,":", dexpa$opsim$sservice$port, dexpa$opsim$sservice$apiemptydb,sep=""),
						httr::add_headers(apikey=dexpa$opsim$sservice$apikey), body='"market"', httr::content_type_json()), silent=F)
		futile.logger::flog.debug("Emptied database with status code %d", response$status_code, name = "dexr.hl.opsim.sservice")
	}
	
	dexR::hl_opsim_startmarket(dexpa, basetime = basetime, runemg=runemg, shutdown = F)
	
	if (shutdown) {
		sec10 <- ((dexpa$sim$duration + dexpa$sim$firstdeliverystart$delay)/dexpa$sim$timefactor) / 10
		for (i in 1:sec10) {
			Sys.sleep(10)
		}
	} else {
		decision <- svDialogs::dlg_message("Press 'OK' to shut down market backend server!", "okcancel")$res
		if (decision == "cancel") {
			futile.logger::flog.warn("Program canceled.", 
					name = "dexr.hl.opsim")
			stop("Program canceled.")
		}
	}
	dexR::hl_closeexperiment(dexpa, basetime = basetime)

	## drop DBs:
	lapply(DBI::dbListConnections(DBI::dbDriver("PostgreSQL")), DBI::dbDisconnect)
	dexR::input_db_dropdb(dexpa)	
	
	## TODO shut down OpSim controller / check processx
	
	if (startnetsim) {
	## TODO shut down pandapower	
	}
	
	if (startsservice) {
		#proc_sservice.kill()
	}
}