#' Retrieves requests data from DB and creates figure of the number of received requests per 
#' product pattern and status by delivery start time.
#' @param dexpa parameter
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_requests_numRequests_byStatusByStartT <- function(dexpa, ...) {
	data <- input_db_requests(dexpa)
	if (nrow(data) > 0) {
		data <- dexpa$sim$filter$requests(dexpa, data)
		output_figure_requests_numRequests_byStatusByStartT(dexpa, data, ...)
	} else {
		futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s!",
				dexpa$db$dbname,
				name = "dexr.hl.requests")
	}
}
#' Retrieves requests data from DB and creates figure of the number of received 
#' requests per product pattern and status by submission time.
#' @param dexpa parameter
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_requests_numRequests_byStatusBySubmT <- function(dexpa, ...) {
	data <- input_db_requests(dexpa)
	if (nrow(data) > 0) {
		data <- dexpa$sim$filter$requests(dexpa, data)
		output_figure_requests_numRequests_byStatusBySubmT(dexpa, data, ...) 
	} else {
		futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s!",
				dexpa$db$dbname,
				name = "dexr.hl.requests")
	}
}
#' Retrieves requests data from DB and creates figure of the number of received requests per 
#' request type (Wind/PV/Storage/Load) by delivery start time.
#' 
#' @param dexpa parameter
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_requests_numRequests_byTypeByStartT <- function(dexpa, ...) {
	hl_figure_requests_numRequests_comp_byTypeByStartT(setNames(list(dexpa), dexpa$sim$id), ...)
}
#' Retrieves requests data from DB and creates figure of the number of received requests per 
#' product pattern by delivery start time.
#' 
#' @param dexpa parameter
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_requests_numRequests_byProductByStartT <- function(dexpa, ...) {
	hl_figure_requests_numRequests_comp_byProductByStartT(setNames(list(dexpa), dexpa$sim$id), ...)
}
#' Retrieves requests data from DB and creates figure of the number of received 
#' requests per product pattern by submission time.
#' 
#' @param dexpa parameter
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_requests_numRequests_byProductBySubmT <- function(dexpa, ...) {
	hl_figure_requests_numRequests_comp_byProductBySubmT(setNames(list(dexpa), dexpa$sim$id), ...)
}
#' Retrieves requests data from DB and creates figure of the number of received requests per 
#' status by delivery start time.
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_requests_numRequests_comp_byStatusByStartT <- function(dexpas, ...) {
	data = data.frame()
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
			data <- rbind(data, d)
		}
	}
	if (nrow(data) > 0) {
		data <- dexpa$sim$filter$requests(dexpa, data)
		output_figure_requests_numRequests_comp_byStatusByStartT(dexpas[[1]], data, ...)
	} else {
		futile.logger::flog.warn("No requests retrieved from PostgreSQL databases %s for IDs %s!",
			paste(lapply(dexpas, function(dp) dp$db$dbname), collapse="/"),
			paste(lapply(dexpas, function(dp) dp$id), collapse="/"),
			name = "dexr.hl.requests")
	}
}
#' Retrieves requests data from DB and creates figure of the number of received requests per 
#' request type (Wind/PV/Storage/Load) by delivery start time.
#' @param dexpa parameter
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_requests_numRequests_comp_byTypeByStartT <- function(dexpas, ...) {
	data = data.frame()
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
			data <- rbind(data, d)
		}
	}
	if (nrow(data) > 0) {
		data <- dexpa$sim$filter$requests(dexpa, data)
		output_figure_requests_numRequests_comp_byTypeByStartT(dexpas[[1]], data, ...)
	} else {
		futile.logger::flog.warn("No requests retrieved from PostgreSQL databases %s for IDs %s!",
				paste(lapply(dexpas, function(dp) dp$db$dbname), collapse="/"),
				paste(lapply(dexpas, function(dp) dp$id), collapse="/"),
				name = "dexr.hl.requests")
	}
}
#' Retrieves requests data from DB and creates figure of the number of submitting clients per 
#' request type (Wind/PV/Storage/Load) by delivery start time.
#' @param dexpa parameter
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_requests_numClients_comp_byTypeByStartT <- function(dexpas, ...) {
	data = data.frame()
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
			data <- rbind(data, d)
		}
	}
	if (nrow(data) > 0) {
		data <- dexpa$sim$filter$requests(dexpa, data)
		output_figure_requests_numClients_comp_byTypeByStartT(dexpas[[1]], data, ...)
	} else {
		futile.logger::flog.warn("No requests retrieved from PostgreSQL databases %s for IDs %s!",
				paste(lapply(dexpas, function(dp) dp$db$dbname), collapse="/"),
				paste(lapply(dexpas, function(dp) dp$id), collapse="/"),
				name = "dexr.hl.requests")
	}
}
#' Retrieves requests data from DB and creates figure of the number of received 
#' requests per status by submission time.
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_requests_numRequests_comp_byStatusBySubmT <- function(dexpas, ...) {
	data = data.frame()
	for (dp in dexpas) {
		# dp <- dexpas[[1]]
		# dp <- dexpas[[2]]
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
		output_figure_requests_numRequests_comp_byStatusBySubmT(dexpas[[1]], data, ...) 
	} else {
		futile.logger::flog.warn("No requests retrieved from PostgreSQL databases %s for IDs %s!",
			paste(lapply(dexpas, function(dp) dp$db$dbname), collapse="/"),
			paste(lapply(dexpas, function(dp) dp$id), collapse="/"),
			name = "dexr.hl.requests")
	}
}
#' Retrieves requests data from DB and creates figure of the number of received requests per 
#' product pattern by delivery start time.
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_requests_numRequests_comp_byProductByStartT <- function(dexpas, ...) {
	data = data.frame()
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
			data <- rbind(data, d)
		}
	}
	if (nrow(data) > 0) {
		data <- dexpa$sim$filter$requests(dexpa, data)
		output_figure_requests_numRequests_comp_byProductByStartT(dexpas[[1]], data, ...)
	} else {
		futile.logger::flog.warn("No requests retrieved from PostgreSQL databases %s for IDs %s!",
			paste(lapply(dexpas, function(dp) dp$db$dbname), collapse="/"),
			paste(lapply(dexpas, function(dp) dp$id), collapse="/"),
			name = "dexr.hl.requests")
	}
}
#' Retrieves requests data from DB and creates figure of the number of received 
#' requests per product pattern by submission time.
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_requests_numRequests_comp_byProductBySubmT <- function(dexpas, ...) {
	data = data.frame()
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
			data <- rbind(data, d)
		}			
		
	}
	if (nrow(data) > 0) {
		data <- dexpa$sim$filter$requests(dexpa, data)
		output_figure_requests_numRequests_comp_byProductBySubmT(dexpas[[1]], data, ...) 
	} else {
		# R.oo::throw.default("No requests in DB for ID ", dp$id, "!")
		futile.logger::flog.warn("No requests retrieved from PostgreSQL databases %s for IDs %s!",
			paste(lapply(dexpas, function(dp) dp$db$dbname), collapse="/"),
			paste(lapply(dexpas, function(dp) dp$id), collapse="/"),
			name = "dexr.hl.requests")
	}
}
#' Retrieves requests data from DB and creates figure of the number of clients
#' per status by delivery start time.
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_requests_numClients_comp_byStatusByStartT <- function(dexpas, ...) {
	data = data.frame()
	for (dp in dexpas) {
		# dp <- dexpas[[1]]
		# dp <- dexpas[[2]]
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
		output_figure_requests_numClients_comp_byStatusByStartT(dexpas[[1]], data, ...) 
	} else {
		futile.logger::flog.warn("No requests retrieved from PostgreSQL databases %s for IDs %s!",
				paste(lapply(dexpas, function(dp) dp$db$dbname), collapse="/"),
				paste(lapply(dexpas, function(dp) dp$id), collapse="/"),
				name = "dexr.hl.requests")
	}
}
#' Retrieves requests data from DB and creates figure of the number of clients by delivery start time.
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_requests_numClients_comp_ByStartT <- function(dexpas, ...) {
	data = data.frame()
	for (dp in dexpas) {
		# dp <- dexpas[[1]]
		# dp <- dexpas[[2]]
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
		output_figure_requests_numClients_comp_ByStartT(dexpas[[1]], data, ...) 
	} else {
		futile.logger::flog.warn("No requests retrieved from PostgreSQL databases %s for IDs %s!",
				paste(lapply(dexpas, function(dp) dp$db$dbname), collapse="/"),
				paste(lapply(dexpas, function(dp) dp$id), collapse="/"),
				name = "dexr.hl.requests")
	}
}