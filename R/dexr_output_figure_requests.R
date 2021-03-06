#' Output figure: Number of submitted requests per submission time and status.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_requests_numRequests_byStatusByStartT <- function(dexpa, data,
		filename = "dex_requests_numRequests_byStatusByCT") {
	# count requests
	data <- plyr::ddply(data, c("status", "start_time"), function(d) {
				d$num_requests = nrow(d)
				d
			})
	
	output_figure_bars(dexpa, data, y_column = "num_requests", title = "Number of requests by status and delivery start time",
			fill_column = NULL, fill_legendtitle = NULL, fill_legenditemnames = NULL,
			facet_column = "product_id", facet_ncol = 1, filename = filename,
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
output_figure_requests_numRequests_byStatusBySubmT <- function(dexpa, data, subtimerounding = "1 minute",
		filename = "dex_requests_numRequests_byStatusBySubmT") {
	# count requests
	# aggregate submission time (as it is too diverse to show, otherwise)
	data$submission_time <- lubridate::round_date(data$submission_time, subtimerounding)
	
	data <- plyr::ddply(data, c("status", "submission_time"), function(d) {
				d$num_requests = nrow(d)
				d
			})
	
	output_figure_bars(dexpa, data, y_column = "num_requests", title = "Number of requests by status and submission time",
			fill_column = NULL, fill_legendtitle = NULL, fill_legenditemnames = NULL,
			facet_column = "product_id", facet_ncol = 1, filename = filename,
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
output_figure_requests_numRequests_comp_byStatusByStartT <- function(dexpa, data,
		filename = "dex_requests_numRequests_byStatusByST", skiplegend=F) {
	# count requests
	data <- plyr::ddply(data, c("id", "status", "start_time"), function(d) {
				d$num_requests = nrow(d)
				d
			})
	data$status <- dexpa$naming$statuses[match(data$status, names(dexpa$naming$statuses))]
	output_figure_bars(dexpa, data, y_column = "num_requests", title = "Number of requests by status and delivery start time",
			fill_column = "id", fill_legendtitle = "Run ID", fill_legenditemnames = NULL,
			facet_column = "status", facet_ncol = 1, filename = filename,
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Number of Requests"),
					if (skiplegend) ggplot2::theme(legend.position="none") else 
								list(ggplot2::theme(legend.position = "bottom"
					)) 	
			),  x_column = "start_time", group_column = "id", 
			position = "dodge", returnplot = FALSE)
}
#' Output figure: Number of submitting clients per submission time and status.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_requests_numClients_comp_byStatusByStartT <- function(dexpa, data,
		filename = "dex_requests_numClients_byStatusByST", skiplegend=F) {
	# count requests
	data <- plyr::ddply(data, c("id", "status", "start_time"), function(d) {
				d$num_clients = length(unique(d$username))
				d
			})
	data$status <- dexpa$naming$statuses[match(data$status, names(dexpa$naming$statuses))]
	output_figure_bars(dexpa, data, y_column = "num_clients", 
			title = "Number of submitting clients by status and delivery start time",
			fill_column = "id", fill_legendtitle = "Run ID", fill_legenditemnames = NULL,
			facet_column = "status", facet_ncol = 1, filename = filename,
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Number of Clients"),
					if (skiplegend) ggplot2::theme(legend.position="none") else 
								list(ggplot2::theme(legend.position = "bottom"
										)) 	
			),  x_column = "start_time", group_column = "id", 
			position = "dodge", returnplot = FALSE)
}
#' Output figure: Number of submitting clients per submission time and status.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_requests_numClients_comp_ByStartT <- function(dexpa, data,
		filename = "dex_requests_numClients_ByST", skiplegend=F) {
	# count requests
	data <- plyr::ddply(data, c("id", "start_time"), function(d) {
				d$num_clients = length(unique(d$username))
				d
			})
	data$status <- dexpa$naming$statuses[match(data$status, names(dexpa$naming$statuses))]
	output_figure_bars(dexpa, data, y_column = "num_clients", 
			title = "Number of submitting clients by delivery start time",
			fill_column = "id", fill_legendtitle = "Run ID", fill_legenditemnames = NULL,
			filename = filename,
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Number of Clients"),
					if (skiplegend) ggplot2::theme(legend.position="none") else 
								list(ggplot2::theme(legend.position = "bottom"
										)) 	
			),  x_column = "start_time", group_column = "id", 
			position = "dodge", returnplot = FALSE)
}
#' Output figure: Number of submitted requests per submission time and request type.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_requests_numRequests_comp_byTypeByStartT <- function(dexpa, data,
		filename = "dex_requests_numRequests_byTypeByCT", skiplegend=F) {
	# count requests
	data <- requests_num_identify_type(dexpa, data)
	output_figure_bars(dexpa, data, y_column = "Number", title = "Number of requests by type and delivery start time",
			fill_column = "id", fill_legendtitle = "Run ID", fill_legenditemnames = NULL,
			facet_column = "Type", facet_ncol = 1, filename = filename,
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Number of Requests"),
					if (skiplegend) ggplot2::theme(legend.position="none") else 
								list(ggplot2::theme(legend.position = "bottom"
					)) 	
			),  x_column = "start_time", group_column = "id", 
			position = "dodge", returnplot = FALSE)
}
#' Output figure: Number of submitting clients per submission time and request type.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_requests_numClients_comp_byTypeByStartT <- function(dexpa, data,
		filename = "dex_requests_numClients_byTypeByCT", skiplegend=F) {
	# count requests
	data <- clients_num_identify_type(dexpa, data)
	output_figure_bars(dexpa, data, y_column = "Number", title = "Number of clients by type and delivery start time",
			fill_column = "id", fill_legendtitle = "Run ID", fill_legenditemnames = NULL,
			facet_column = "Type", facet_ncol = 1, filename = filename,
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Number of Clients"),
					if (skiplegend) ggplot2::theme(legend.position="none") else 
								list(ggplot2::theme(legend.position = "bottom"
										)) 	
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
output_figure_requests_numRequests_comp_byStatusBySubmT <- function(dexpa, data,
		filename = "dex_requests_numRequests_byStatusBySubmT", skiplegend=F) {
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
			facet_column = "status", facet_ncol = 1, filename = filename,
			alpha=1.0, 
			ggplotaddons = list(
					ggplot2::xlab("Submission time"),
					ggplot2::ylab("Number of Requests"),
					if (skiplegend) ggplot2::theme(legend.position="none") else 
								list(ggplot2::theme(legend.position = "bottom"
					)) 	
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
output_figure_requests_numRequests_comp_byProductByStartT <- function(dexpa, data,
		filename = "dex_requests_numRequests_byProductByCT", skiplegend=F) {
	# count requests
	data <- plyr::ddply(data, c("id", "product_id", "start_time"), function(d) {
				d$num_requests = nrow(d)
				d
			})
	
	output_figure_bars(dexpa, data, y_column = "num_requests", title = "Number of requests by product and delivery start time",
			fill_column = "id", fill_legendtitle = "Run ID", fill_legenditemnames = NULL,
			facet_column = "product_id", facet_ncol = 1, filename = filename,
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Number of Requests"),
					if (skiplegend) ggplot2::theme(legend.position="none") else 
								list(ggplot2::theme(legend.position = "bottom"
					)) 	
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
output_figure_requests_numRequests_comp_byProductBySubmT <- function(dexpa, data,
		filename = "dex_requests_numRequests_byProductBySubmT", skiplegend=F) {
	# count requests
	attr(data$submission_time, "tzone") <- "Europe/Berlin"
	data$submission_time <- as.POSIXct(round(data$submission_time,"mins"))
	data <- plyr::ddply(data, c("id", "product_id", "submission_time"), function(d) {
				d$num_requests = nrow(d)
				d
			})

	output_figure_bars(dexpa, data, y_column = "num_requests", title = "Number of requests by product and submission time",
			fill_column = "id", fill_legendtitle = "Run ID", fill_legenditemnames = NULL,
			facet_column = "product_id", facet_ncol = 1, filename = filename,
			alpha=1.0, 
			ggplotaddons = list(
					ggplot2::xlab("Submission time"),
					ggplot2::ylab("Number of Requests"),
					if (skiplegend) ggplot2::theme(legend.position="none") else 
								list(ggplot2::theme(legend.position = "bottom"
					)) 	
			), 
			x_column = "submission_time", group_column = "id", 
			position = "dodge", 
			returnplot = FALSE)
}
