#' Retrieve data and store figure about the number of considered requests per clearing by clearing time
#' @param dexpa 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_clearing_numConsideredRequests <- function(dexpa, ...) {
	data <- input_db_clearings(dexpa)
	if (nrow(data)==0) {
		R.oo::throw.default("DB contains no clearing information (table clearing_info) for DB ", dexpa$db$dbname, " for start time
			between", format(as.POSIXct(dexpa$sim$starttime_min, tz="GTM", origin = "1970-01-01"), "%H:%M:%S"), " and ",
				format(as.POSIXct(dexpa$sim$starttime_max, tz="GTM", origin = "1970-01-01"), "%H:%M:%S"),"!", )
	}
	data <- dexpa$sim$filter$clearings(dexpa, data)
	output_figure_clearing_numConsideredRequests_byCTbyProduct(dexpa, data, ...)
}
#' Retrieve data and store figure about the clearing price per clearing by clearing time and by product
#' @param dexpa 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_clearing_clearingPriceByCTbyProduct <- function(dexpa, ...) {
	data <- input_db_clearings(dexpa)
	data <- dexpa$sim$filter$clearings(dexpa, data)
	output_figure_clearing_prices_byCTbyProduct(dexpa, data, ...)
}
#' Retrieve data and store figure about the number of considered requests per clearing by clearing time
#' @param dexpa 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_clearing_comp_numConsideredRequests <- function(dexpas, ...) {
	data = data.frame()
	for (dp in dexpas) {	
		d <- input_db_clearings(dp)
		if (nrow(d)==0) {
			futile.logger::flog.warn("DB contains no clearing information (table clearing_info) for DB %s for start between %s and %s!", 
				dp$db$dbname,
				format(as.POSIXct(dexpa$sim$starttime_min, tz="GTM", origin = "1970-01-01"), "%d/%m/%y %H:%M:%S"),
				format(as.POSIXct(dexpa$sim$starttime_max, tz="GTM", origin = "1970-01-01"), "%d/%m/%y %H:%M:%S"))
		} else {
			d$id <- input_db_runID(dp)
			data <- rbind(data, d)
		}
	}
	if (nrow(data)>0) {
		data <- dexpas[[1]]$sim$filter$clearings(dexpa, data)
		output_figure_clearing_comp_numConsideredRequests_byCTbyProduct(dexpas[[1]], data = data, ...)
	} else {
		futile.logger::flog.warn("No clearing information (table clearing_info) retrieved from PostgreSQL databases %s for IDs %s!",
			paste(lapply(dexpas, function(dp) dp$db$dbname), collapse="/"),
			paste(lapply(dexpas, function(dp) dp$id), collapse="/"),
			name = "dexr.hl.requests")
	}
}
#' Retrieve data and store figure about the clearing price per clearing by clearing time and by product
#' @param dexpa 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_clearing_comp_clearingPriceByCTbyProduct <- function(dexpas, ...) {
	data = data.frame()
	for (dp in dexpas) {	
		d <- input_db_clearings(dp)
		if (nrow(d)==0) {
			futile.logger::flog.warn("DB contains no clearing information (table clearing_info) for DB %s for start between %s and %s!", 
				dp$db$dbname,
				format(as.POSIXct(dexpa$sim$starttime_min, tz="GTM", origin = "1970-01-01"), "%d/%m/%y %H:%M:%S"),
				format(as.POSIXct(dexpa$sim$starttime_max, tz="GTM", origin = "1970-01-01"), "%d/%m/%y %H:%M:%S"))
		} else {
			d$id <- input_db_runID(dp)
			data <- rbind(data, d)
		}
	}
	if (nrow(data)>0) {
		output_figure_clearing_comp_prices_byCTbyProduct(dexpas[[1]], data = data, ...)
	} else {
		futile.logger::flog.warn("No clearing information (table clearing_info) retrieved from PostgreSQL databases %s for IDs %s!",
			paste(lapply(dexpas, function(dp) dp$db$dbname), collapse="/"),
			paste(lapply(dexpas, function(dp) dp$id), collapse="/"),
			name = "dexr.hl.requests")
	}
}
