#' Runs experiments on the rasperry Pi cluster
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
hl_raspic <- function(dexpa, basetime = as.numeric(round(Sys.time(),"mins"))*1000,
		offset = round(basetime - as.numeric(Sys.time())*1000), 
		outputfile = paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, "_", dexpa$emg$rseed, ".log", sep=""), 
		outfilemarket = paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, "_", dexpa$emg$rseed, "_market.log", sep=""),
		outfileemg = paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, "_", dexpa$emg$rseed, "_emg.log", sep="")) {
	
	dexpa$sim$raspic = T
	
	shbasic::sh.ensurePath(paste(dexpa$dirs$config, dexpa$sim$id,sep="/"))
	
	dexpa$db$dbname		= dexpa$sim$id
	dexpa$server$port 	= dexpa$server$startport +  as.numeric(strsplit(dexpa$sim$id, "-")[[1]][2]) + dexpa$server$portoffset
	futile.logger::flog.debug("Set dexpa$server$port to: %d", dexpa$server$port, name="dexr.hl.experiment.raspic")	
	
	dexR::hl_experiment(dexpa, outputfile=paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, ".log", sep=""),
			outfilemarket = paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, "_market.log", sep=""),
			outfileemg = paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, "_emg.log", sep=""),
			basetime = as.numeric(round(strptime("30/09/19 12:00", "%d/%m/%y %H:%M"),"mins"))*1000, runmarket = F, 
			shutdown = T, createdb = T, raspic=T)
	
	dexR::input_db_dropdb(dexpa)
}
#' Transfer config dir to raspberry Pi cluster
#' 
#' @param dexpa 
#' @return session
#' 
#' @author Sascha Holzhauer
#' @author Christoph Hanauer
#' @export
hl_raspic_transferemgconfig <- function(dexpa) {
	
	# probably obsolete: dexpa$raspic$serverconfigpath since we are using a temp dir
	
	futile.logger::flog.info("Transferring EMG config folder to Raspberry Pi cluster at %s...", dexpa$raspic$server,
			name="dexr.hl.experiment.raspic")
	session <- ssh::ssh_connect(paste(dexpa$raspic$user, dexpa$raspic$server, sep="@"))
	
	# Create character vector
	output <- textConnection("sim_directory", "w")
	
	# Unique simulation id
	iddirpart <- if(!is.na(dexpa$sim$nodesetid)) paste(dexpa$sim$id, "_", dexpa$sim$nodesetid, sep="") else dexpa$sim$id
		
	# Tell the server module we need a temporary directory for sim iddirpart
	out <- ssh::ssh_exec_wait(session, command = paste(dexpa$raspic$preparedircommand, ' "', iddirpart, '"', sep=""), std_out = output)
	
	path <- paste(dexpa$dirs$config, iddirpart, sep="/")
	files <- dir(path)
	
	for (f in files) {
		# tail(sim..) holds the temp directory as last element
		out <- ssh::scp_upload(session, paste(path, f, sep="/"), tail(sim_directory, n=1), verbose = TRUE)
		futile.logger::flog.debug(out, name="dexr.hl.experiment.raspic")
	}
	# At this point we have a temporary directory for the simulation and all files are uploaded
	
	# Copying the simulation to our desired nodes ToDo
	noderange = ""
	
	# We distribute the simulation to the nodes
	out <- ssh::ssh_exec_wait(session, command = paste(dexpa$raspic$distributecommand, ' "', tail(sim_directory, n=1), '"', ' "', noderange, '"', sep=""))
	
	return(session)
}
#' Run EMGs on raspberry Pi cluster
#' 
#' @param dexpa 
#' @return 
#' 
#' @author Sascha Holzhauer
#' @author Christoph Hanauer
#' @export
hl_raspic_runemg <- function(dexpa, session) {
	futile.logger::flog.info("Starting EMGs on Raspberry Pi cluster at %s...", dexpa$raspic$server,
			name="dexr.hl.experiment.raspic")
	iddirpart <- if(!is.na(dexpa$sim$nodesetid)) paste(dexpa$sim$id, "_", dexpa$sim$nodesetid, sep="") else dexpa$sim$id
	noderange = ""
	# out <- ssh::ssh_exec_wait(session, command = paste(dexpa$raspic$runemgcommand, '"', iddirpart, '"', '"', noderange, '"', sep=""))
	out <- ssh::ssh_exec_wait(session, command = paste(dexpa$raspic$runemgcommand, ' "', iddirpart, '"', ' "', noderange, '"', sep=""))
	futile.logger::flog.debug(out, name="dexr.hl.experiment.raspic")
}
#' Close SSH session to raspberry Pi cluster
#' 
#' @param dexpa 
#' @return 
#' 
#' @author Sascha Holzhauer
#' @author Christoph Hanauer
#' @export
hl_raspic_closesession <- function(dexpa, session) {
	futile.logger::flog.info("Closing SSH session to Raspberry Pi cluster at %s...", dexpa$raspic$server,
			name="dexr.hl.experiment.raspic")
	ssh::ssh_disconnect(session)
}