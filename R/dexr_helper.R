#' Convert milliseconds to POSIXct
#' @param ms a numeric vector of milliseconds (big integers of 13 digits)
#' @param t0 a string of the format "yyyy-mm-dd", specifying the date that corresponds to 0 millisecond
#' @param timezone a string specifying a timezone that can be recognized by R
#' @return POSIXct vector representing calendar dates and times   
#' 
#' @author Sascha Holzhauer
#' @export
ms_to_date = function(ms, t0="1970-01-01", timezone="Europe/Berlin") {      
	sec = ms / 1000
	as.POSIXct(sec, origin=t0, tz=timezone)
}
#' The given/default dexpa object is dublicated, and sim$id and dp$dbname are subsituted by the given ID.
#' @param id id
#' @return dexpa
#' 
#' @author Sascha Holzhauer
#' @export
create_dexpa <- function(id, dexpa = dexR::param_getDefaultDexpa()) {
	dexpa$sim$id <- id
	dexpa$db$dbname <- id
	return(dexpa)
}
#' Create a list of <code>dexpa</code> objects from vector of ids
#' 
#' The given/default dexpa object is dublicated, and sim$id and dp$dbname are subsituted by one of the given IDs.
#' @param ids vector of ids
#' @return list of dexpas
#' 
#' @author Sascha Holzhauer
#' @export
create_dexpas <- function(ids, dexpa = dexR::param_getDefaultDexpa()) {
	dexpas <- list()
	for (id in ids) {
		dp <- dexpa
		dp$sim$id <- id
		dp$db$dbname <- id
		dexpas=c(dexpas, setNames(list(dp), id))
	}
	return(dexpas)
}
#' Convert time string to milliseconds
#' 
#' @param timestring 
#' @param format 
#' @param timezone  
#' @return time in ms
#' 
#' @author Sascha Holzhauer
#' @export
str_to_ms = function(timestring, format = "%Y-%m-%d %H:%M", timezone="Europe/Berlin") {
	as.numeric(strptime(timestring,format = format, tz=timezone))*1000
}
#' Copies machine-specific and project-specific config file <code>dexpa-machine_machine.R</code> to location 
#' <code>dexpa$dirs$config</code> for the user to edit. Creates folder if not existing.
#'
#' @param dexpa 
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
setup_environment <- function(dexpa) {
	
	shbasic::sh.ensurePath(dexpa$dirs$scripts, stripFilename = F)
	
	futile.logger::flog.info("Copy dexpa-machine_machine.R to %s...",
			dexpa$dirs$scripts,
			"dexr.setup"
	)
	
	file.copy(from= system.file("config/R/dexpa-machine_machine.R", package="dexR"), 
			to=dexpa$dirs$scripts, 
			overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
	

	futile.logger::flog.info("Copy dexpa-project.R to %s...",
			dexpa$dirs$scripts,
			"dexr.setup"
	)
	
	file.copy(from= system.file("config/R/dexpa-project.R", package="dexR"), 
			to=dexpa$dirs$scripts, 
			overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
	

	futile.logger::flog.info("Finished!",
			"dexr.setup"
	)
}
#' Copies config files <code>dexpa-pversion_pversion</code>, run file (<code>DEX_Runs.csv</code>), and 
#' config table files (<code>DEX_Param_Configs.xxx</code>) to a new project version dir for the user to edit. Given
#' <code>dexpa</code> needs to have the project version assigned.
#' 
#' NOTE: The machine-specific config file should have been executed before running this procedure!
#' 
#' NOTE: The returned dexpa object needs to be assigned back since the source procedure operates locally within this function.
#'
#' @param dexpa 
#' @return dexpa
#' 
#' @author Sascha Holzhauer
#' @export
setup_project_version <- function(dexpa) {
	
	targetfile = paste(dexpa$dirs$scripts, dexpa$sim$version, paste("dexpa-pversion_", dexpa$sim$version, ".R", sep=""), sep="/")
	shbasic::sh.ensurePath(targetfile, stripFilename = T)

	
	futile.logger::flog.info("Copy dexpa-pversion_pversion to %s...",
			targetfile,
			"dexr.setup"
	)
	
	file.copy(from= system.file("config/R/dexpa-pversion_pversion.R", package="dexR"), 
			to=targetfile, 
			overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
	
	futile.logger::flog.info("Execute %s ...",
			targetfile,
			"dexr.setup"
	)
	
	source(targetfile, local=T)
	
	shbasic::sh.ensurePath(dexpa$dirs$project, stripFilename = F)
	shbasic::sh.ensurePath(dexpa$dirs$config, stripFilename = F)
	
	
	futile.logger::flog.info("Copy DEX_Param_Configs.ods and DEX_Param_Configs.csv to %s...",
			dexpa$dirs$config,
			"dexr.setup"
	)
	
	file.copy(from= c(system.file("config/main/DEX_Param_Configs.ods", package="dexR"),
					system.file("config/main/DEX_Param_Configs.csv", package="dexR")),
			to=dexpa$dirs$config, 
			overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
	
	shbasic::sh.ensurePath(dexpa$files$runinfos, stripFilename = T)
	
	futile.logger::flog.info("Copy DEX_Runs.csv to %s...",
			dirname(dexpa$files$runinfos),
			"dexr.setup"
	)
	
	file.copy(from= system.file("data/DEX_Runs.csv", package="dexR"), 
			to=dirname(dexpa$files$runinfos), 
			overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
	
	
	futile.logger::flog.info("Finished!",
			"dexr.setup"
	)
	
	return(dexpa)
}

#' Import dumpfiles into DB to use for sample figures 
#' @return list of dexpa objects
#' 
#' @author Sascha Holzhauer
#' @export
demo_prepare_db4figures <- function() {
	library("RPostgreSQL")
	source(system.file("config/R/dexpa-figures.R", package="dexR"))

	dexpa$dirs$output$dbdumps <- system.file("data/dbdumps", package="dexR")

	dp1		<- dexpa
	dp1$sim$id 	<- "enavi_07-08"
	dp1$db$dbname 	<- dp1$sim$id
	
	dp2		<- dexpa
	dp2$sim$id 	<- "enavi_07-09"
	dp2$db$dbname 	<- dp2$sim$id

	dexR::input_db_dump2db(dp1, dumpfile="dump_enavi_07-08")
	dexR::input_db_dump2db(dp2, dumpfile="dump_enavi_07-09")
	return(list("dp1" = dp1, "dp2" = dp2))
}
#' Determine EMG port in multi-node (and other) setting
#' @param dexpa 
#' @param nodeid 
#' @return EMG port for specific node ID
#' 
#' @author Sascha Holzhauer
#' @export
emggetport <- function(dexpa, nodeid) {
	return(dexpa$emg$startport +  as.numeric(strsplit(dexpa$sim$id, "-")[[1]][2]) + as.numeric(nodeid) + 
					dexpa$emg$portoffset)
}
