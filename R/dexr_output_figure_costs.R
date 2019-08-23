#' Output figure: Gini coefficient of costs of accepted energy per KWh sum per delivery start time.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_energycosts_requested_giniByStartT <- function(dexpa, data) {
	data <- prepare_costdata_gini(data)
	
	output_figure_lines(dexpa, data, y_column = "values", title = "Normalised energy, costs, and Gini coef. of costs/KWh by delivery start",
			colour_column = "Type",
			facet_ncol = 1, filename = "dex_energycosts_requested_GiniByDT",
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Energy/costs/Gini coefficient"),
					ggplot2::theme(
							legend.position = "bottom"
					)
			),  x_column = "start_time",
			
			returnplot = FALSE)
}
#' Output figure: Gini coefficient of costs of accepted energy per KWh sum per delivery start time.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_energycosts_requested_comp_giniByStartT <- function(dexpa, data) {
	data <- prepare_costdata_gini(data)
	
	output_figure_lines(dexpa, data, y_column = "values", title = "Normalised energy, costs, and Gini coef. of costs/KWh by delivery start",
			colour_column = "id",
			linetype_column = "Type",
			facet_ncol = 1, filename = "dex_energycosts_requested_comp_GiniByDT",
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Energy/costs/Gini coefficient"),
					ggplot2::theme(
							legend.position = "bottom"
					), ggplot2::guides(colour = ggplot2::guide_legend(ncol=1), 
							linetype = ggplot2::guide_legend(ncol=1))
			),  x_column = "start_time", 
			returnplot = FALSE)	
}
prepare_costdata_gini <- function(data) {
	data <- plyr::ddply(data, c("start_time", "id"), function(d) {
				# d <- data[data$id == data$id[1] & data$start_tim == data$start_time[1],]
				peruserdata <- plyr::ddply(d, c("username"), function(dperuser) {
							new = data.frame(
									"energy" = sum(dperuser[, "energy_accepted"]),
									"costs" = sum(dperuser[, "price_cleared"]),
									"costPerEnergy" = if (sum(dperuser[, "energy_accepted"]) == 0) 0 else sum(dperuser[, "price_cleared"]) / sum(dperuser[, "energy_accepted"]))
							new
						})
				print(peruserdata$costPerEnergy)
				
				result = data.frame(		
						"Energy" = sum(peruserdata[, "energy"]),
						"Costs" = sum(peruserdata[, "costs"]),
						"Gini" =ineq::Gini(peruserdata$costPerEnergy, corr=T)
				)
				result
			})
	
	# Normalise:
	data$Energy = (data$Energy - min(data$Energy)) / (max(data$Energy) - min(data$Energy))
	data$Costs = (data$Costs - min(data$Costs)) / (max(data$Costs) - min(data$Costs))
	
	data <- reshape2::melt(data, id.vars=c("start_time", "id"), variable.name = "Type",
			value.name = "values")
}
#' Output figure: Costs of requested energy sum per delivery start time.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_energycosts_requested_sumByStartT <- function(dexpa, data) {
	# count requests
	data <- plyr::ddply(data, c("start_time"), function(d) {
				new = data.frame(
						"energy" = sum(d[, "energy_requested"]),
						"costs" = sum(d[, "price_cleared"]),
						"start_time" = d$start_time[1])
				new
			})
	
	data <- reshape2::melt(data, id.vars=c("start_time"), variable.name = "Type",
			value.name = "values")
	
	output_figure_lines(dexpa, data, y_column = "values", title = "Requested energy and costs by delivery start time",
			colour_column = "Type",
			facet_ncol = 1, filename = "dex_energycosts_requested_sumByCT",
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Requested energy/costs"),
					ggplot2::theme(
							legend.position = "bottom"
					)
			),  x_column = "start_time",
			
			returnplot = FALSE)
}
#' Output figure: Comparison of costs of requested energy sum per delivery start time.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_energycosts_requested_comp_sumByStartT <- function(dexpa, data) {
	# count requests
	data <- plyr::ddply(data, c("id", "start_time"), function(d) {
				new = data.frame(
						"energy" = sum(d[, "energy_requested"]),
						"costs" = sum(d[, "price_accepted"]),
						"start_time" = d$start_time[1],
						"id" = d$id)
				new
			})
	data <- reshape2::melt(data, id.vars=c("id", "start_time"), variable.name = "Type",
			value.name = "values")
	
	output_figure_lines(dexpa, data, y_column = "values", title = "Requested energy and accepted costs of requests by delivery start time",
			colour_column = "id",
			facet_ncol = 1, filename = "dex_energycosts_requested_comp_sumByCT",
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Requested energy/Accepted costs"),
					ggplot2::theme(
							legend.position = "bottom"
					), ggplot2::guides(colour = ggplot2::guide_legend(ncol=1), 
							linetype = ggplot2::guide_legend(ncol=1))
			),  x_column = "start_time", 
			returnplot = FALSE)
}