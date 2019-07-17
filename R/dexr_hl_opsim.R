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
	java <- if (is.na(dexpa$opsim$control$jre)) "java" else paste(dexpa$opsim$control$jre, "bin", "java", sep="/")
	
	system2(wait=FALSE, java, 
			args = paste(dexpa$opsim$control$args, ' -jar ',
					dexpa$opsim$control$jar),
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

	futile.logger::flog.info("Run Schedule Service (%s)\nin %s\nwith JRE %s.\nLogging to %s...",
			dexpa$opsim$sservice$jar,
			dexpa$opsim$sservice$rundir,
			dexpa$opsim$sservice$jre,
			outfilesservice,
			name = "dexr.hl.opsim.sservice")
	
	futile.logger::flog.debug("Set working directory to %s...", 
			dexpa$opsim$sservice$rundir,
			name = "dexr.hl.opsim.sservice")
	
	setwd(dexpa$opsim$sservice$rundir)
	java <- if (is.na(dexpa$opsim$sservice$jre)) "java" else paste(dexpa$opsim$sservice$jre, "bin", "java", sep="/")
	
	system2(wait=FALSE, java, 
			args = paste(' -jar ',
					dexpa$opsim$sservice$jar),
			stdout=outfilesservice, stderr=outfilesservice)
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
	
	# PYTHONPATH??
	Sys.setenv("PYTHONPATH"=dexpa$opsim$netsim$pythonpath)
	system2(wait=FALSE, python, 
			args = dexpa$opsim$netsim$module,
			stdout=outfilenetsim, stderr=outfilenetsim)
}
#' Run OpSim control manager, schedule service, net simulation, DEX market backend, and EMG clients
#' 
#' @param dexpa 
#' @return run DEX with OpSIM 
#' 
#' @author Sascha Holzhauer
#' @export
hl_opsim <- function(dexpa, basetime = as.numeric(round(strptime("30/09/19 12:00", "%d/%m/%y %H:%M"),"mins"))*1000) {
	dexR::hl_opsim_runmanager(dexpa)

	decision <- svDialogs::dlg_message("Press button when Control Manager ready!", "okcancel")$res
	if (decision == "cancel") {
		futile.logger::flog.warn("Program canceled.", 
				name = "dexr.hl.opsim")
		stop("Program canceled.")
	}
	
	dexR::hl_opsim_runscheduleservice(dexpa)
	dexR::hl_opsim_runnetsim(dexpa)
	
	decision <- svDialogs::dlg_message("Press button when Schedule Service and Net Simulation are ready!", "okcancel")$res
	if (decision == "cancel") {
		futile.logger::flog.warn("Program canceled.", 
				name = "dexr.hl.opsim")
		stop("Program canceled.")
	}
	
	dexR::hl_experiment(dexpa, outputfile=paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, ".log", sep=""),
			outfilemarket = paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, "_market.log", sep=""),
			outfileemg = paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, "_emg.log", sep=""),
			basetime = basetime, shutdown = F, createdb = T)
	
	decision <- svDialogs::dlg_message("Press button to shut down market backend server!", "okcancel")$res
	if (decision == "cancel") {
		futile.logger::flog.warn("Program canceled.", 
				name = "dexr.hl.opsim")
		stop("Program canceled.")
	}
	
	dexpa$db$dbname 	<- dexpa$sim$id
	dexR::input_db_db2dump(dexpa, dumpdir=paste("dump_", dexpa$sim$id, sep=""))
	dexR::server_shutdown(dexpa)
	
	## drop DBs:
	dexR::input_db_dropdb(dexpa)	
	lapply(DBI::dbListConnections(DBI::dbDriver("PostgreSQL")), DBI::dbDisconnect)
}