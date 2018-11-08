#' Retrieve data and store figure about the number of considered requests per clearing by clearing time
#' @param dexpa 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_clearing_numConsideredRequests <- function(dexpa) {
	data <- input_db_clearings(dexpa)
	if (nrow(data)==0) {
		R.oo::throw.default("DB contains no clearing information (table clearing_info)!")
	}
	output_figure_clearing_numConsideredRequests_byCTbyProduct(dexpa, data)
}
#' Retrieve data and store figure about the clearing price per clearing by clearing time and by product
#' @param dexpa 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_clearing_clearingPriceByCTbyProduct <- function(dexpa) {
	data <- input_db_clearings(dexpa)
	output_figure_clearing_prices_byCTbyProduct(dexpa, data)
}
#' Retrieve data and store figure about the number of considered requests per clearing by clearing time
#' @param dexpa 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_clearing_comp_numConsideredRequests <- function(dexpas) {
	data = data.frame()
	for (dp in dexpas) {	
		d <- input_db_clearings(dp)
		if (nrow(d)==0) {
			R.oo::throw.default("DB contains no clearing information (table clearing_info) for DB ", dexpa$db$dbname, "!")
		} else {
			d$id <- input_db_runID(dp)
			data <- rbind(data, d)
		}
	}	
	output_figure_clearing_comp_numConsideredRequests_byCTbyProduct(dexpas[[1]], data = data)
}
#' Retrieve data and store figure about the clearing price per clearing by clearing time and by product
#' @param dexpa 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_clearing_comp_clearingPriceByCTbyProduct <- function(dexpas) {
	data = data.frame()
	for (dp in dexpas) {	
		d <- input_db_clearings(dp)
		if (nrow(d)==0) {
			R.oo::throw.default("DB contains no clearing information (table clearing_info) for DB ", dexpa$db$dbname, "!")
		} else {
			d$id <- input_db_runID(dp)
			data <- rbind(data, d)
		}
	}
	output_figure_clearing_comp_prices_byCTbyProduct(dexpas[[1]], data = data)
}
