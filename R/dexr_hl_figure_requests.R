#' Retrieves requests data from DB and creates figure of the number of received requests per 
#' product pattern and status by delivery start time.
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_requests_numRequests_byStatusByStartT <- function(dexpa) {
	data <- input_db_requests(dexpa)
	output_figure_requests_numRequests_byStatusByStartT(dexpa, data) 
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
	output_figure_requests_numRequests_byStatusBySubmT(dexpa, data) 
}