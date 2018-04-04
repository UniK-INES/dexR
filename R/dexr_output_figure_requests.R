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
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Number of Requests")
			),  x_column = "start_time", group_column = "status", 
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
			alpha=1.0, ggplotaddons = list(
						ggplot2::xlab("Submission time"),
						ggplot2::ylab("Number of Requests")
					), 
			x_column = "submission_time", group_column = "status", 
			group_colors = dexpa$colours$statuses,
			position = "stack", returnplot = FALSE)
}
#' Output figure: Number of submitted requests per submission time and status.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_requests_numRequests_comp_byStatusByStartT <- function(dexpa, data) {
	# count requests
	data <- plyr::ddply(data, c("id", "status", "start_time"), function(d) {
				d$num_requests = nrow(d)
				d
			})
	data$status <- dexpa$naming$statuses[match(data$status, names(dexpa$naming$statuses))]
	output_figure_bars(dexpa, data, y_column = "num_requests", title = "Number of requests by status and delivery start time",
			fill_column = "id", fill_legendtitle = "Run ID", fill_legenditemnames = NULL,
			facet_column = "status", facet_ncol = 1, filename = "dex_requests_numRequests_byStatusByCT",
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Number of Requests"),
					ggplot2::theme(
							legend.position = "bottom"
					)
			),  x_column = "start_time", group_column = "id", 
			position = "dodge", returnplot = FALSE)
}
#' Output figure: Number of considered requests by submission time and status.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_requests_numRequests_comp_byStatusBySubmT <- function(dexpa, data) {
	# count requests
	attr(data$submission_time, "tzone") <- "Europe/Berlin"
	data$submission_time <- as.POSIXct(round(data$submission_time,"mins"))
	data <- plyr::ddply(data, c("id", "status", "submission_time"), function(d) {
				d$num_requests = nrow(d)
				d
			})
	data$status <- dexpa$naming$statuses[match(data$status, names(dexpa$naming$statuses))]
	output_figure_bars(dexpa, data, y_column = "num_requests", title = "Number of requests by status and submission time",
			fill_column = "id", fill_legendtitle = "Run ID", fill_legenditemnames = NULL,
			facet_column = "status", facet_ncol = 1, filename = "dex_requests_numRequests_byStatusBySubmT",
			alpha=1.0, 
			ggplotaddons = list(
					ggplot2::xlab("Submission time"),
					ggplot2::ylab("Number of Requests"),
					ggplot2::theme(
							legend.position = "bottom"
					)	
			), 
			x_column = "submission_time", group_column = "id", 
			position = "dodge", 
			returnplot = FALSE)
}
#' Output figure: Number of submitted requests per submission time and product.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_requests_numRequests_comp_byProductByStartT <- function(dexpa, data) {
	# count requests
	data <- plyr::ddply(data, c("id", "product_id", "start_time"), function(d) {
				d$num_requests = nrow(d)
				d
			})
	
	output_figure_bars(dexpa, data, y_column = "num_requests", title = "Number of requests by product and delivery start time",
			fill_column = "id", fill_legendtitle = "Run ID", fill_legenditemnames = NULL,
			facet_column = "product_id", facet_ncol = 1, filename = "dex_requests_numRequests_byProductByCT",
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Number of Requests"),
					ggplot2::theme(
							legend.position = "bottom"
					)
			),  x_column = "start_time", group_column = "id", 
			position = "dodge", returnplot = FALSE)
}
#' Output figure: Number of considered requests by submission time and product.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_requests_numRequests_comp_byProductBySubmT <- function(dexpa, data) {
	# count requests
	attr(data$submission_time, "tzone") <- "Europe/Berlin"
	data$submission_time <- as.POSIXct(round(data$submission_time,"mins"))
	data <- plyr::ddply(data, c("id", "product_id", "submission_time"), function(d) {
				d$num_requests = nrow(d)
				d
			})

	output_figure_bars(dexpa, data, y_column = "num_requests", title = "Number of requests by product and submission time",
			fill_column = "id", fill_legendtitle = "Run ID", fill_legenditemnames = NULL,
			facet_column = "product_id", facet_ncol = 1, filename = "dex_requests_numRequests_byProductBySubmT",
			alpha=1.0, 
			ggplotaddons = list(
					ggplot2::xlab("Submission time"),
					ggplot2::ylab("Number of Requests"),
					ggplot2::theme(
							legend.position = "bottom"
					)	
			), 
			x_column = "submission_time", group_column = "id", 
			position = "dodge", 
			returnplot = FALSE)
}
