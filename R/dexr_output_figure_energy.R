#' @importFrom lubridate %within%
#' @export
lubridate::`%within%`

#' Output figure: Requested energy per submission time and status.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_energy_requested_byStatusByStartT <- function(dexpa, data) {
	# count requests
	
	# TODO apply fix in output_figure_energy_requested_comp_sumByLoadGenByStartT to other functions!
	data <- plyr::ddply(data, c("status", "start_time"), function(d) {
				new = data.frame(
					"energy" = sum(d[, "energy_requested"]),
					"status" = d$status[1],
					"start_time" = d$start_time[1])
				new
			})
	
	output_figure_bars(dexpa, data, y_column = "energy", title = "Requested energy by status and delivery start time",
			fill_column = NULL, fill_legendtitle = NULL, fill_legenditemnames = NULL,
			facet_ncol = 1, filename = "dex_energy_requested_byStatusByCT",
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Requested energy")
			),  x_column = "start_time", group_column = "status", 
			group_colors = dexpa$colours$statuses,
			position = "stack", returnplot = FALSE)
}
#' Output figure: Requested energy sum per submission time.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_energy_requested_sumByStartT <- function(dexpa, data) {
	# count requests
	data <- plyr::ddply(data, c("start_time"), function(d) {
				new = data.frame(
						"energy" = sum(d[, "energy_requested"]),
						"start_time" = d$start_time[1])
				new
			})
	
	output_figure_bars(dexpa, data, y_column = "energy", title = "Requested energy by delivery start time",
			fill_column = NULL, fill_legendtitle = NULL, fill_legenditemnames = NULL,
			facet_ncol = 1, filename = "dex_energy_requested_sumByCT",
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Requested energy")
			),  x_column = "start_time",
			position = "stack", returnplot = FALSE)
}
#' Output figure: Requested energy per submission time and status.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_energy_requested_comp_byStatusByStartT <- function(dexpa, data) {
	# count requests
	data <- plyr::ddply(data, c("id", "status", "start_time"), function(d) {
				new = data.frame(
						"energy" = sum(d[, "energy_requested"]),
						"status" = d$status[1],
						"start_time" = d$start_time[1],
						"id" = d$id)
				new
			})
	data$status <- dexpa$naming$statuses[match(data$status, names(dexpa$naming$statuses))]
	output_figure_bars(dexpa, data, y_column = "energy", title = "Requested energy of requests by status and delivery start time",
			fill_column = "id", fill_legendtitle = "Run ID", fill_legenditemnames = NULL,
			facet_column = "status", facet_ncol = 1, filename = "dex_energy_requested_comp_byStatusByCT",
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Requested energy"),
					ggplot2::theme(
							legend.position = "bottom"
					)
			),  x_column = "start_time", group_column = "id", 
			position = "dodge", returnplot = FALSE)
}
#' Output figure: Requested energy per submission time and status.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_energy_requested_comp_sumByStartT <- function(dexpa, data) {
	# count requests
	data <- plyr::ddply(data, c("id", "start_time"), function(d) {
				new = data.frame(
						"energy" = sum(d[, "energy_requested"]),
						"start_time" = d$start_time[1],
						"id" = d$id)
				new
			})
	output_figure_bars(dexpa, data, y_column = "energy", title = "Requested energy of requests by status and delivery start time",
			fill_column = "id", fill_legendtitle = "Run ID", fill_legenditemnames = NULL,
			facet_ncol = 1, filename = "dex_energy_requested_comp_sumByCT",
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Requested energy"),
					ggplot2::theme(
							legend.position = "bottom"
					)
			),  x_column = "start_time", group_column = "id", 
			position = "dodge", returnplot = FALSE)
}
#' Output figure: Requested energy per submission time and status.
#' 
#' Requirement: Delivery periods of all products must be a multiple of the shortest delivery period (with start and end
#' times matching those of the shorted delivery period product)!
#' 
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_energy_requested_comp_sumByLoadGenByStartT <- function(dexpa, data) {
	# count requests
	# data$id = "Test"
	
	data <- plyr::ddply(data, c("id"), function(df) {
		# df <- data[data$id == data[1,"id"],]
		# identify shortest delivery period:
		shortestDelivery <- min(df$end_time - df$start_time)
		minStartTime 	 <- min(df$start_time)
		maxEndTime		 <- max(df$end_time)
		
		# create interval vector of shortest delivery period:
		intervals <- seq(minStartTime, maxEndTime, by = shortestDelivery)
		intervals <- lubridate::interval(intervals[1:(length(intervals)-1)], intervals[(1+1):length(intervals)])
		result <- data.frame(start_time = intervals, 
				Load = rep(0, length(intervals)),
				Gen  = rep(0, length(intervals)))
		
		# aggregate energy:
		for (r in 1:nrow(df)) {
			# r = 1
			if (df[r, "energy_requested"] > 0) {
				result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
					"Load"] = result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
							"Load"] + df[r, "energy_requested"]
			} else {
				result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
						"Gen"] = result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
								"Gen"] + df[r, "energy_requested"]
			}
		}
		result$start_time <- lubridate::int_start(result$start_time)
		result
	})

	# calculate residuals:
	data$Residual <- data$Load + data$Gen
	data$Gen <- -data$Gen
	
	data <- reshape2::melt(data, id.vars=c("id", "start_time"), variable.name = "Type",
			value.name = "energy")
	
	output_figure_lines(dexpa, data, y_column = "energy", title = "Requested energy of requests by generation/load and delivery start time",
			colour_column = "Type",
			linetype_column = "id", linetype_legendtitle = "Run ID",
			facet_ncol = 1, filename = "dex_energy_requested_comp_sumGenLoadByCTlines",
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Requested energy"),
					ggplot2::scale_x_datetime(),
					ggplot2::theme(
							legend.position = "bottom"
					), ggplot2::guides(linetype = ggplot2::guide_legend(ncol=1))
			),  x_column = "start_time", returnplot = FALSE)
}