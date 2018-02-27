#' Retrieve data and store figure about the number of considered requests per clearing by clearing time
#' @param dexpa 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_clearing_numConsideredRequests <- function(dexpa) {
	data <- input_db_clearings(dexpa)
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