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
emggethttpport <- function(dexpa, nodeid) {
	return(dexpa$emg$httpstartport + (as.numeric(dexpa$sim$runnumber) -1) * length(dexpa$sim$nodeids) + if (nodeid == "") 0 else as.numeric(nodeid) + 
					dexpa$emg$httpportoffset)
}
#' Determine EMG port in multi-node (and other) setting
#' @param dexpa 
#' @param nodeid 
#' @return EMG port for specific node ID
#' 
#' @author Sascha Holzhauer
#' @export
emggetport <- function(dexpa, nodeid) {
	return(dexpa$emg$startport +   (as.numeric(dexpa$sim$runnumber) -1) * length(dexpa$sim$nodeids) + if (nodeid == "") 0 else 
						as.numeric(nodeid) + dexpa$emg$portoffset)
}
#' Sums the expression in <code>dataexp</code> up separately for PC/Win/StorageOut/StorageIn
#' 
#' @param data 
#' @param dataexp 
#' @return 
#' 
#' @author Sascha Holzhauer
#' @export
requests_identify_type <- function(data, dataexp='df[r, if(df$status==2) "energy_accepted" else "energy_requested"]') {
	data <- plyr::ddply(data, c("id"), function(df) {
				# df <- data[data$id == data[1,"id"],]
				# identify shortest delivery period:
				shortestDelivery <- min(df$end_time - df$start_time)
				minStartTime 	 <- min(df$start_time)
				maxEndTime		 <- max(df$end_time)
				
				# create interval vector of shortest delivery period:
				intervals <- seq(minStartTime, maxEndTime, by = shortestDelivery)
				futile.logger::flog.debug("consider %d intervals and %d reqeusts...", 
						length(intervals), nrow(df), name="dexr.helper.types")
				
				intervals <- lubridate::interval(intervals[1:(length(intervals)-1)], intervals[(1+1):length(intervals)])
				result <- data.frame(start_time = intervals, 
						PV = rep(0, length(intervals)),
						Wind  = rep(0, length(intervals)),
						StorageOut  = rep(0, length(intervals)),
						StorageIn  = rep(0, length(intervals)))
				
				# aggregate energy:
				# TODO more efficient implementation!
				for (r in 1:nrow(df)) {
					# r = 1
					# assign the according energy to all intervals that are within the delivery period:
					if (grepl("Pv", df[r, "cid"])) {
						result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
								"PV"] = result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
										"PV"] + eval(parse(text=dataexp))
					} else if (grepl("Wind", df[r, "cid"])) {
						result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
								"Wind"] = result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
										"Wind"] + eval(parse(text=dataexp))			
					} else if (grepl("Storage", df[r, "cid"]) & (df[r, "energy_requested"] < 0)) {
						result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
								"StorageOut"] = result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
										"StorageOut"] + eval(parse(text=dataexp))
					} else if (grepl("Storage", df[r, "cid"]) & (df[r, "energy_requested"] > 0)) {
						result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
								"StorageIn"] = result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
										"StorageIn"] + eval(parse(text=dataexp))
					} 
				}
				result$start_time <- lubridate::int_start(result$start_time)
				result
			})
}
#' Check differences in market information
#' @param dexpas 
#' @return true if market information differ between passed dexpas
#' 
#' @author Sascha Holzhauer
#' @export
param_marketinfodiffer <- function(dexpas) {
	# dexpas = create_dexpas(ids 
	data <- data.frame()
	for (dp in dexpas) {
		data <- rbind(data, cbind(input_db_param_marketinfo(dp)[,c("uid")],
			input_csv_runinfos(dp)[,c("TF", "Basetime", "Offset", "Duration")],
			input_db_param_marketinfo(dp)[,-c(1)]))
	}
	return(nrow(unique(data))>1)
}
#' Check differences in market product configuration
#' @param dexpas 
#' @return true if market product configurations differ between passed dexpas
#' 
#' @author Sascha Holzhauer
#' @export
param_marketproductsdiffer <- function(dexpas) {
	tocomp = ""
	for (dp in dexpas) {
		paramConfigs <- dexR::input_csv_configparam(dp)
		if (tocomp == "") {
			tocomp = paramConfigs["products"]
		} else {
			if (length(unique(c(unlist(tocomp), unlist(paramConfigs["products"]))))> 1) {
				return(TRUE)
			} 
		}
	}
	return(FALSE)
}
#' Check differences in client configuration
#' @param dexpas 
#' @return true if client configurations differ between passed dexpas
#' 
#' @author Sascha Holzhauer
#' @export
param_clientsdiffer <- function(dexpas) {	
	tocompclients = ""
	tocomploads = ""
	tocomploadprofiles = ""
	tocompgenerations = ""
	tocomppvplant = ""
	tocompwindplants = ""
	tocompstorages = ""
	tocomprequestconfigs = ""
	
	for (dp in dexpas) {
		# dp = dexpas[[1]]
		# dp = dexpas[[3]]
		paramConfigs <- dexR::input_csv_configparam(dp)
		for (i in 1:nrow(paramConfigs)) {
			if (tocompclients == "") {
				tocompclients = paramConfigs[i, "clients"]
				tocomploads = paramConfigs[i, "loads"]
				tocomploadprofiles = paramConfigs[i, "loadProfiles"]
				tocompgenerations = paramConfigs[i, "generations"]
				tocomppvplant = paramConfigs[i, "pvplants"]
				tocompwindplants = paramConfigs[i, "windplants"]
				tocompstorages = paramConfigs[i, "devicesStorage"]
				tocomprequestconfigs = paramConfigs[i, "requestConfig"]
			} else {
				if (tocompclients != paramConfigs[i, "clients"] || 
					tocomploads != paramConfigs[i, "loads"] ||
					tocomploadprofiles != paramConfigs[i, "loadProfiles"] ||
					tocompgenerations != paramConfigs[i, "generations"] ||
					tocomppvplant != paramConfigs[i, "pvplants"] ||
					tocompwindplants != paramConfigs[i, "windplants"] ||
					tocompstorages != paramConfigs[i, "devicesStorage"] ||
					tocomprequestconfigs != paramConfigs[i, "requestConfig"]) {
					return(TRUE)
				}
			} 
		}
	}
	return(FALSE)
}
