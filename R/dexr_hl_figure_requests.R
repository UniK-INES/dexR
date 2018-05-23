#' Retrieves requests data from DB and creates figure of the number of received requests per 
#' product pattern and status by delivery start time.
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_requests_numRequests_byStatusByStartT <- function(dexpa) {
	data <- input_db_requests(dexpa)
	if (nrow(data) > 0) {
		output_figure_requests_numRequests_byStatusByStartT(dexpa, data)
	} else {
		futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s!",
				dexp$db$dbname,
				name = "dexr.hl.requests")
	}
}
#' Retrieves requests data from DB and creates figure of the number of received 
#' requests per product pattern and status by submission time.
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_requests_numRequests_byStatusBySubmT <- function(dexpa) {
	data <- input_db_requests(dexpa)
	if (nrow(data) > 0) {
		output_figure_requests_numRequests_byStatusBySubmT(dexpa, data) 
	} else {
		futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s!",
				dexp$db$dbname,
				name = "dexr.hl.requests")
	}
}
#' Retrieves requests data from DB and creates figure of the number of received requests per 
#' status by delivery start time.
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_requests_numRequests_comp_byStatusByStartT <- function(dexpas) {
	data = data.frame()
	for (dp in dexpas) {	
		d <- input_db_requests(dp)
		d$id <- input_db_runID(dp)
		data <- rbind(data, d)
	}
	if (nrow(data) > 0) {
		output_figure_requests_numRequests_comp_byStatusByStartT(dexpas[1], data)
	} else {
		futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s!",
				dexp$db$dbname,
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
hl_figure_requests_numRequests_comp_byStatusBySubmT <- function(dexpas) {
	data = data.frame()
	for (dp in dexpas) {	
		d <- input_db_requests(dp)
		d$id <- input_db_runID(dp)		
		data <- rbind(data, d)
	}
	if (nrow(data) > 0) {
		output_figure_requests_numRequests_comp_byStatusBySubmT(dexpas[1], data) 
	} else {
		futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s!",
				dexp$db$dbname,
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
hl_figure_requests_numRequests_comp_byProductByStartT <- function(dexpas) {
	data = data.frame()
	for (dp in dexpas) {	
		d <- input_db_requests(dp)
		d$id <- input_db_runID(dp)		
		data <- rbind(data, d)
	}
	if (nrow(data) > 0) {
		output_figure_requests_numRequests_comp_byProductByStartT(dexpas[1], data)
	} else {
		futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s!",
				dexp$db$dbname,
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
hl_figure_requests_numRequests_comp_byProductBySubmT <- function(dexpas) {
	data = data.frame()
	for (dp in dexpas) {	
		d <- input_db_requests(dp)
		d$id <- input_db_runID(dp)
		data <- rbind(data, d)
	}
	if (nrow(data) > 0) {
		output_figure_requests_numRequests_comp_byProductBySubmT(dexpas[1], data) 
	} else {
		futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s!",
				dexp$db$dbname,
				name = "dexr.hl.requests")
	}
}
