#' Retrieve data and store figure about the number of considered requests per clearing by clearing time
#' @param dexpa 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_clearing_numConsideredRequests <- function(dexpa) {
	data <- input_db_clearings(dexpa)
	if (nrow(data)==0) {
		R.oo::throw.default("DB contains to clearing information (table clearing_info)!")
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
hl_figure_clearing_comp_numConsideredRequests <- function(dexpa, dp2) {
	data1 <- input_db_clearings(dexpa)
	data1$id <- input_db_runID(dexpa)
	data2 <- input_db_clearings(dp2)
	data2$id <- input_db_runID(dp2)
	
	output_figure_clearing_comp_numConsideredRequests_byCTbyProduct(dexpa, data = rbind(data1, data2))
}
#' Retrieve data and store figure about the clearing price per clearing by clearing time and by product
#' @param dexpa 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_clearing_comp_clearingPriceByCTbyProduct <- function(dexpa, dp2) {
	data1 <- input_db_clearings(dexpa)
	data1$id <- input_db_runID(dexpa)
	data2 <- input_db_clearings(dp2)
	data2$id <- input_db_runID(dp2)
	output_figure_clearing_comp_prices_byCTbyProduct(dexpa, data = rbind(data1, data2))
}