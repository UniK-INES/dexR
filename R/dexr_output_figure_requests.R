#' Output figure: Number of submitted requests per submission time and status.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_requests_numRequests_byStatusByStartT <- function(dexpa, data) {
	# count requests
	data <- plyr::ddply(data, c("status", "start_time"), function(d) {
				d$num_requests = nrow(d)
				d
			})
	
	output_figure_bars(dexpa, data, y_column = "num_requests", title = "Number of requests by status and delivery start time",
			fill_column = NULL, fill_legendtitle = NULL, fill_legenditemnames = NULL,
			facet_column = "product_id", facet_ncol = 1, filename = "dex_requests_numRequests_byStatusByCT",
			alpha=1.0, ggplotaddons = NULL, x_column = "start_time", group_column = "status", 
			group_colors = dexpa$colours$statuses,
			position = "stack", returnplot = FALSE)
}
#' Output figure: Number of considered requests by submission time and status.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_requests_numRequests_byStatusBySubmT <- function(dexpa, data) {
	# count requests
	data <- plyr::ddply(data, c("status", "submission_time"), function(d) {
				d$num_requests = nrow(d)
				d
			})
	
	output_figure_bars(dexpa, data, y_column = "num_requests", title = "Number of requests by status and submission time",
			fill_column = NULL, fill_legendtitle = NULL, fill_legenditemnames = NULL,
			facet_column = "product_id", facet_ncol = 1, filename = "dex_requests_numRequests_byStatusBySubmT",
			alpha=1.0, ggplotaddons = NULL, x_column = "submission_time", group_column = "status", 
			group_colors = dexpa$colours$statuses,
			position = "stack", returnplot = FALSE)
}