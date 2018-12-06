#' Retrieves requests data from DB and creates figure of the number of received requests per 
#' product pattern and status by delivery start time.
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_energy_requested_byStatusByStartT <- function(dexpa) {
	data <- input_db_requests(dexpa)
	if (nrow(data) > 0) {
		data <- dexpa$sim$filter$requests(dexpa, data)
		output_figure_energy_requested_byStatusByStartT(dexpa, data)
	} else {
		futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s!",
				dexpa$db$dbname,
				name = "dexr.hl.requests")
	}
}
#' Retrieves requests data from DB and creates figure of the number of received requests per 
#' product pattern and status by delivery start time.
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_energy_requested_sumByStartT <- function(dexpa) {
	data <- input_db_requests(dexpa)
	if (nrow(data) > 0) {
		data <- dexpa$sim$filter$requests(dexpa, data)
		output_figure_energy_requested_sumByStartT(dexpa, data)
	} else {
		futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s!",
				dexpa$db$dbname,
				name = "dexr.hl.requests")
	}
}
#' Retrieves requests data from DB and creates figure of requested energy per 
#' status by delivery start time.
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_energy_requested_comp_byStatusByStartT <- function(dexpas, skiplegend=F, type="residual") {
	data = data.frame()
	for (dp in dexpas) {
		# dp = dexpas[[1]]
		# dp = dexpas[[2]]
		d <- input_db_requests(dp)
		if (nrow(d) == 0) {
			# R.oo::throw.default("No requests in DB for ID ", dp$id, "!")
			futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s for ID %s!",
					dp$db$dbname,
					dp$id,
					name = "dexr.hl.requests")
		} else {
			d$id <- input_db_runID(dp)
			data <- rbind(data, d)
		}
	}
	if (nrow(data) > 0) {
		data <- dexpa$sim$filter$requests(dexpa, data)
		output_figure_energy_requested_comp_byStatusByStartT(dexpas[[1]], data, type=type, skiplegend=skiplegend)
	}
}
#' Retrieves requests data from DB and creates figure of requested energy by delivery start time.
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_energy_requested_comp_sumByStartT <- function(dexpas) {
	data = data.frame()
	for (dp in dexpas) {
		# dp = dexpas[[1]]
		d <- input_db_requests(dp)
		if (nrow(d) == 0) {
			# R.oo::throw.default("No requests in DB for ID ", dp$id, "!")
			futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s for ID %s!",
					dp$db$dbname,
					dp$id,
					name = "dexr.hl.requests")
		} else {
			d$id <- input_db_runID(dp)
			data <- rbind(data, d)
		}
	}
	if (nrow(data) > 0) {
		data <- dexpa$sim$filter$requests(dexpa, data)
		output_figure_energy_requested_comp_sumByStartT(dexpas[[1]], data)
	}
}
#' Retrieves requests data from DB and creates figure of requested energy by delivery start time 
#' (generation, load, and residual as lines).
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_energy_requested_comp_sumLoadGenByStartT <- function(dexpas) {
	data = data.frame()
	products <- input_db_param_products(dexpas[[1]])
	for (dp in dexpas) {	
		d <- input_db_requests(dp)
		if (nrow(d) == 0) {
			# R.oo::throw.default("No requests in DB for ID ", dp$id, "!")
			futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s for ID %s!",
					dp$db$dbname,
					dp$id,
					name = "dexr.hl.requests")
		} else {
			d$id <- input_db_runID(dp)	
			
			# filter requests (ACCEPTED, UNHANDLED, und DECLINED@last auction of each delivery interval)
			
			# DECLINED && submission_time > (start_time + closing_time - auction_interval)
			# d = d[1:5,]
			d[d$status %in% c(0,1,2) | (d$status==3 & d$submission_time > d$start_time - 
					# lubridate obviously ignores negative durations
					(lubridate::as.duration(paste(products[match(d$product_id,products$description), "closing_time"],"in",sep="")) + 
					lubridate::as.duration(paste(products[match(d$product_id,products$description), "auction_interval"],"in",sep="")))),]
			data <- rbind(data, d)
		}
	}
	if (nrow(data) > 0) {
		data <- dexpa$sim$filter$requests(dexpa, data)
		dexR::output_figure_energy_requested_comp_sumByLoadGenByStartT(dexpas[[1]], data)
	}
}

