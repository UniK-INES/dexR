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
output_figure_energy_requested_byStatusByStartT <- function(dexpa, data, type="residual", skiplegend=F) {
	# count requests
	
	# TODO apply fix in output_figure_energy_requested_comp_sumByLoadGenByStartT to other functions!
	data <- plyr::ddply(data, c("status", "start_time"), function(d) {
				new = data.frame(
					"Energy" = sum(d[
									if (type=="load") d$energy_requested>0
									else if (type=="generation") d$energy_requested<0
									else TRUE, "energy_requested"]),
					"status" = d$status[1],
					"start_time" = d$start_time[1])
				new
			})
	
	output_figure_bars(dexpa, data, y_column = "Energy", title = "Requested energy by status and delivery start time",
			fill_column = NULL, fill_legendtitle = NULL, fill_legenditemnames = NULL,
			facet_ncol = 1, filename = paste("dex_energy_requested_byStatusByCT", 
					shbasic::shbasic_condenseRunids(data[, "id"]), sep="_"),
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Requested energy (kWh)"),
					if (skiplegend) ggplot2::theme(legend.position="none") else NULL
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
						"Energy" = sum(d[, "energy_requested"]),
						"start_time" = d$start_time[1])
				new
			})
	
	output_figure_bars(dexpa, data, y_column = "Energy", title = "Requested energy by delivery start time",
			fill_column = NULL, fill_legendtitle = NULL, fill_legenditemnames = NULL,
			facet_ncol = 1, filename = paste("dex_energy_requested_sumByCT", 
					shbasic::shbasic_condenseRunids(data[, "id"]), sep="_"),
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Requested energy (kWh)")
			),  x_column = "start_time",
			position = "stack", returnplot = FALSE)
}
#' Output figure: Requested energy per submission time and status.
#' 
#' Requirement: Delivery periods of all products must be a multiple of the shortest delivery period (with start and end
#' times matching those of the shortest delivery period product)!
#' 
#' @param dexpa 
#' @param data 
#' @return figure file
#' @seealso \code{\link{output_figure_lines}}
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_energy_requested_sumByLoadGenByStartT <- function(dexpa, data) {
	# count requests
	# data$id = "Test"
	

	# identify shortest delivery period:
	shortestDelivery <- min(data$end_time - data$start_time)
	minStartTime 	 <- min(data$start_time)
	maxEndTime		 <- max(data$end_time)
				
	# create interval vector of shortest delivery period:
	intervals <- seq(minStartTime, maxEndTime, by = shortestDelivery)
	intervals <- lubridate::interval(intervals[1:(length(intervals)-1)], intervals[(1+1):length(intervals)])
	result <- data.frame(start_time = intervals, 
				Load = rep(0, length(intervals)),
				Gen  = rep(0, length(intervals)))
				
	# aggregate energy:
	for (r in 1:nrow(data)) {
		# r = 1
		if (data[r, "energy_requested"] > 0) {
				result[intervals %within% lubridate::interval(data[r, "start_time"],data[r, "end_time"]),
						"Load"] = result[intervals %within% lubridate::interval(data[r, "start_time"],data[r, "end_time"]),
								"Load"] + data[r, if(data$status==2) "energy_accepted" else "energy_requested"]
		} else {
			result[intervals %within% lubridate::interval(data[r, "start_time"],data[r, "end_time"]),
					"Gen"] = result[intervals %within% lubridate::interval(data[r, "start_time"],data[r, "end_time"]),
							"Gen"] + data[r, if(data$status==2) "energy_accepted" else "energy_requested"]
		}
	}
	result$start_time <- lubridate::int_start(result$start_time)
	data <- result
	
	# calculate residuals:
	data$Residual <- data$Load + data$Gen
	data$Gen <- -data$Gen
	
	data <- reshape2::melt(data, id.vars=c("start_time"), variable.name = "Type",
			value.name = "energy")
	
	output_figure_lines(dexpa, data, y_column = "energy", title = "Requested energy of requests by generation/load and delivery start time",
			colour_legendtitle = "Run ID",
			linetype_column = "Type", linetype_legendtitle = "Type",
			facet_ncol = 1, filename = paste("dex_energy_requested_comp_sumGenLoadByCTlines", 
					shbasic::shbasic_condenseRunids(data[, "id"]), sep="_"),
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Requested energy (kWh)"),
					ggplot2::scale_x_datetime(),
					ggplot2::theme(
							legend.position = "bottom"
					), ggplot2::guides(linetype = ggplot2::guide_legend(ncol=1))
			),  x_column = "start_time", returnplot = FALSE)
}
#' Output figure: Requested energy per submission time and status.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_energy_requested_comp_byStatusByStartT <- function(dexpa, data, type="residual", skiplegend=F) {
	# count requests
	data <- plyr::ddply(data, c("id", "status", "start_time"), function(d) {
				new = data.frame(
						"Energy" = sum(d[if (type=="load") d$energy_requested>0
												else if (type=="generation") d$energy_requested<0
												else TRUE, "energy_requested"]),
						"status" = d$status[1],
						"start_time" = d$start_time[1],
						"id" = d$id)
				new
			})
	data$status <- dexpa$naming$statuses[match(data$status, names(dexpa$naming$statuses))]
	output_figure_bars(dexpa, data, y_column = "Energy", title = "Requested energy of requests by status and delivery start time",
			fill_column = "id", fill_legendtitle = "Run ID", fill_legenditemnames = NULL,
			facet_column = "status", facet_ncol = 1, filename = paste("dex_energy_requested_comp_byStatusByCT", 
					shbasic::shbasic_condenseRunids(data[, "id"]), sep="_"),
			alpha=1.0, ggplotaddons = list(
					if (skiplegend) ggplot2::theme(legend.position="none") else ggplot2::theme(
										legend.position = "bottom"
								), 
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Requested energy (kWh)")
					
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
						"Energy" = sum(d[, "energy_requested"]),
						"start_time" = d$start_time[1],
						"id" = d$id)
				new
			})
	output_figure_bars(dexpa, data, y_column = "Energy", title = "Requested energy of requests by delivery start time",
			fill_column = "id", fill_legendtitle = "Run ID", fill_legenditemnames = NULL,
			facet_ncol = 1, filename = paste("dex_energy_requested_comp_sumByCT", 
					shbasic::shbasic_condenseRunids(data[, "id"]), sep="_"),
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Requested energy (kWh)"),
					ggplot2::theme(
							legend.position = "bottom"
					)
			),  x_column = "start_time", group_column = "id", 
			position = "dodge", returnplot = FALSE)
}
#' Output figure: Requested energy per submission time and status.
#' 
#' Requirement: Delivery periods of all products must be a multiple of the shortest delivery period (with start and end
#' times matching those of the shortest delivery period product)!
#' 
#' @param dexpa 
#' @param data 
#' @return figure file
#' @seealso \code{\link{output_figure_lines}}
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_energy_requested_comp_sumByLoadGenByStartT <- function(dexpa, data) {
	
	output_figure_lines(dexpa, data, y_column = "Energy", title = "Requested energy of requests by generation/load and delivery start time",
			colour_column = "id", colour_legendtitle = "Run ID",
			linetype_column = "Type", linetype_legendtitle = "Type",
			facet_ncol = 1, filename = paste("dex_energy_requested_comp_sumGenLoadByCTlines", 
					shbasic::shbasic_condenseRunids(data[, "id"]), sep="_"),
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Requested energy (kWh)"),
					ggplot2::scale_x_datetime(),
					ggplot2::theme(
							legend.position = "bottom"
					), ggplot2::guides(colour = ggplot2::guide_legend(ncol=1), linetype = ggplot2::guide_legend(ncol=1))
			),  x_column = "start_time", returnplot = FALSE)
}
#' Output figure: Requested energy for specific status per location and submission time.
#' 
#' Requirement: Delivery periods of all products must be a multiple of the shortest delivery period (with start and end
#' times matching those of the shortest delivery period product)!
#' 
#' @param dexpa 
#' @param data 
#' @return figure file
#' @seealso \code{\link{output_figure_lines}}
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_energy_status_comp_sumByLocationLoadGenByStartT <- function(dexpa, data, status="REQUESTED", ggplotaddons = NULL) {
	
	output_figure_lines(dexpa, data, 
			y_column = "Energy", 
			title = paste("Energy (", status, ") of requests by location, Gen/Load and delivery", sep=""),
			colour_column = "id", colour_legendtitle = "Run ID",
			linetype_column = "Type", linetype_legendtitle = "Type",
			facet_column = "Location",
			facet_ncol = 1, 
			filename = paste(dexpa$fig$filenameprefix, "energy_", status, "_comp_SumByLocationDelivery_", 
					shbasic::shbasic_condenseRunids(data[, "id"]), dexpa$fig$filenamepostfix, sep=""),
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Energy (kWh)"),
					ggplot2::scale_x_datetime(),
					ggplot2::theme(
							legend.position = "bottom"
					), ggplot2::guides(colour = ggplot2::guide_legend(ncol=1), linetype = ggplot2::guide_legend(ncol=1)),
					ggplotaddons
			),  x_column = "start_time", returnplot = FALSE)
}
#' Output figure: Requested energy for specific status per location and submission time.
#' 
#' Requirement: Delivery periods of all products must be a multiple of the shortest delivery period (with start and end
#' times matching those of the shortest delivery period product)!
#' 
#' @param dexpa 
#' @param data 
#' @return figure file
#' @seealso \code{\link{output_figure_lines}}
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_power_status_comp_sumByLocationLoadGenByStartT <- function(dexpa, data, status="REQUESTED", ggplotaddons = NULL) {
	
	output_figure_lines(dexpa, data, 
			y_column = "Power", 
			title = paste("Power (", status, ") of requests by location, Gen/Load and delivery", sep=""),
			colour_column = "id", colour_legendtitle = "Run ID",
			linetype_column = "Type", linetype_legendtitle = "Type",
			facet_column = "Location",
			facet_ncol = 1, 
			filename = paste(dexpa$fig$filenameprefix, "energy_", status, "_comp_SumByLocationDelivery_", 
					shbasic::shbasic_condenseRunids(data[, "id"]), dexpa$fig$filenamepostfix, sep=""),
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Power (kW)"),
					ggplot2::scale_x_datetime(),
					ggplot2::theme(
							legend.position = "bottom"
					), ggplot2::guides(colour = ggplot2::guide_legend(ncol=1), linetype = ggplot2::guide_legend(ncol=1)),
					ggplotaddons
			),  x_column = "start_time", returnplot = FALSE)
}
#' Output figure: Requested energy balance per area/location for specific status per submission time.
#' 
#' Requirement: Delivery periods of all products must be a multiple of the shortest delivery period (with start and end
#' times matching those of the shortest delivery period product)!
#' 
#' @param dexpa 
#' @param data 
#' @return figure file
#' @seealso \code{\link{output_figure_lines}}
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_power_status_comp_sumByLocationBalanceByStartT <- function(dexpa, data, status="REQUESTED", ggplotaddons = NULL) {
	
	output_figure_lines(dexpa, data, 
			y_column = "Power", 
			title = paste("Power (", status, ") balance of requests by location and delivery", sep=""),
			colour_column = "id", colour_legendtitle = "Run ID",
			linetype_column = "Type", linetype_legendtitle = "Type",
			facet_column = "Location",
			facet_ncol = 1, 
			filename = paste(dexpa$fig$filenameprefix, "energy_", status, "_comp_SumBalanceByLocationDelivery_", 
					shbasic::shbasic_condenseRunids(data[, "id"]), dexpa$fig$filenamepostfix, sep=""),
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Power (kW)"),
					ggplot2::scale_x_datetime(),
					ggplot2::theme(
							legend.position = "bottom"
					), ggplot2::guides(colour = ggplot2::guide_legend(ncol=1), linetype = ggplot2::guide_legend(ncol=1)),
					ggplotaddons
			),  x_column = "start_time", returnplot = FALSE)
}
#' Output figure: Requested generation energy per generation type and submission time and status.
#' 
#' Requirement: Delivery periods of all products must be a multiple of the shortest delivery period (with start and end
#' times matching those of the shortest delivery period product)!
#' 
#' @param dexpa 
#' @param data 
#' @return figure file
#' @seealso \code{\link{output_figure_lines}}
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_energy_requested_comp_sumGenByGenTypeStartT <- function(dexpa, data, skiplegend=F) {
	
	data <- requests_energy_identify_type(dexpa, data)
	
	output_figure_lines(dexpa, data, y_column = "energy", title = "Requested energy per type and delivery start time",
			colour_column = "id", colour_legendtitle = "Run ID",
			facet_column = "Type",
			facet_ncol = 1, filename = paste("dex_energy_requested_comp_sumGenByGenTypeCTlines", 
					shbasic::shbasic_condenseRunids(data[, "id"]), sep="_"),
			alpha=1.0, ggplotaddons = list(
					if (skiplegend) ggplot2::theme(legend.position="none") else ggplot2::theme(
										legend.position = "bottom"
								),
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Energy (kWh)"),
					ggplot2::scale_x_datetime(),
					ggplot2::guides(colour = ggplot2::guide_legend(ncol=1))
			),  x_column = "start_time", returnplot = FALSE)
}
