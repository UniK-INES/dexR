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
					ggplot2::ylab("Energy (kWh) / costs (EUR) / Gini coefficient"),
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
output_figure_energycosts_requested_comp_giniByStartT <- function(dexpa, data, skiplegend=F) {
	data <- prepare_costdata_gini(data)
	
	output_figure_lines(dexpa, data, y_column = "values", title = "Normalised energy, costs, and Gini coef. of costs/KWh by delivery start",
			colour_column = "id",
			facet_column = "Type",
			facet_ncol = 1, filename = "dex_energycosts_requested_comp_GiniByDT",
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Energy (kWh) / costs (EUR) / Gini coefficient"),
					if (skiplegend) ggplot2::theme(legend.position="none") else list(ggplot2::theme(
												legend.position = "bottom"
										), ggplot2::guides(colour = ggplot2::guide_legend(ncol=1)))
			),  x_column = "start_time", 
			returnplot = FALSE)	
}
prepare_costdata_gini <- function(data, normalise=F) {
	data <- plyr::ddply(data, c("start_time", "id"), function(d) {
				# d <- data[data$id == data$id[1] & data$start_tim == data$start_time[1],]
				peruserdata <- plyr::ddply(d, c("username"), function(dperuser) {
							new = data.frame(
									"energy" = sum(dperuser[, "energy_accepted"]),
									"costs" = sum(dperuser[, "price_cleared"] * dperuser[, "energy_accepted"]),
									"price" = mean(dperuser[dperuser$price_cleared > 0, "price_cleared"]),
									"costPerEnergy" = if (sum(dperuser[, "energy_accepted"]) == 0) 0 else sum(dperuser[, "price_cleared"]) / sum(dperuser[, "energy_accepted"]))
							new
						})
				
				result = data.frame(		
						"Energy" = sum(peruserdata[, "energy"]),
						"Costs" = sum(peruserdata[, "costs"]),
						"Price" = mean(peruserdata[peruserdata$price > 0, "price"]),
						"Gini" =ineq::Gini(peruserdata$costPerEnergy, corr=T)
				)
				result
			})
	
	# Normalise:
	if(normalise) {
		data$Energy = (data$Energy - min(data$Energy)) / (max(data$Energy) - min(data$Energy))
		data$Costs = (data$Costs - min(data$Costs)) / (max(data$Costs) - min(data$Costs))
	}
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
					ggplot2::ylab("Requested energy (kWh) / costs (EUR)"),
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
output_figure_energycosts_requested_comp_sumByStartT <- function(dexpa, data, skiplegend=F) {
	futile.logger::flog.debug("Figure: Energycosts Summed by delviery start time: sum costs...", name="dexr.hl.costs")
	data <- plyr::ddply(data, c("id", "start_time"), function(d) {
				new = data.frame(
						"Energy" = sum(d[, "energy_requested"]),
						"Costs" = sum(d[, "price_cleared"]),
						"Start_time" = d$start_time[1],
						"id" = d$id)
				new
			})
	futile.logger::flog.debug("Figure: Energycosts Summed by delviery start time: reshape data...", name="dexr.hl.costs")
	data <- reshape2::melt(data, id.vars=c("id", "Start_time"), variable.name = "Type",
			value.name = "values")
	
	futile.logger::flog.debug("Figure: Energycosts Summed by delviery start time: plot figure...", name="dexr.hl.costs")
	output_figure_lines(dexpa, data, y_column = "values", title = "Requested energy and accepted costs of requests by delivery start time",
			colour_column = "id", facet_column = "Type",
			facet_ncol = 1, filename = "dex_energycosts_requested_comp_sumByCT",
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Requested energy (kWh) / Accepted costs (EUR)"),
					if (skiplegend) ggplot2::theme(legend.position="none") else list(ggplot2::theme(
												legend.position = "bottom"
										)) 
			),  x_column = "Start_time", 
			returnplot = FALSE)
}
#' Output figure: Comparison of average costs of requested energy per type and per delivery start time.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_energycosts_requested_comp_avgByTypeStartT <- function(dexpa, data, skiplegend=FALSE) {
	futile.logger::flog.debug("Figure: Energycosts Summed by delviery start time: sum costs...", name="dexr.hl.costs")
	
	prices <- plyr::ddply(data, c("id"), function(d) {
				new = data.frame(
						"Prices" = d[d$accepted_energy > 0, "cleared_price"],
						"id" = d$id)
				new
			})
		
	futile.logger::flog.debug("Figure: price histogram: plot figure...", 
			name="dexr.output.figure..costs")
	
	output_figure_lines(dexpa, prices, y_column = "price", title = "Price histogram",
			colour_column = "id", filename = "dex_pricehistogram_comp",
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Price"),
					ggplot2::ylab("Häufigkeit"),
					if (skiplegend) ggplot2::theme(legend.position="none") else list(ggplot2::theme(
										legend.position = "bottom"
								)), 
			),  x_column = "price", 
			returnplot = FALSE)
}
#' Generate histogram of prices of load requests.
#' @param dexpas
#' @param data 
#' @param skiplegend 
#' @return price histogram
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_prices_comp_histogram  <- function(dexpas, data, skiplegend=FALSE, title="Histogram of load prices", 
		ggplotaddons = NULL, returnplot = FALSE, 
		filename = paste("Histogram_Load_Prices_", shbasic::shbasic_condenseRunids(data[, "id"]), sep="_")) {
	futile.logger::flog.debug("Figure: Price histogram - prepare prices...", name="dexr.output.figures.costs")
	
	futile.logger::flog.debug("Figure: Energy prices by type and delviery start time: reshape data...", 
			name="dexr.hl.costs")
	
	dexpa$fig$init(dexpa, outdir = paste(dexpa$dirs$output$figures, "lines", sep="/"), filename = filename)
	
	prices <- data[data$energy_accepted < 0, c("id","price_cleared")]
	
	p1 <- ggplot2::ggplot(prices, ggplot2::aes(x=price_cleared, fill = id)) + ggplot2::geom_histogram(position="dodge") +
			ggplot2::theme(strip.text.x = ggplot2::element_text(size=8)) +
			(if (!is.null(title) && title != "") ggplot2::labs(title = title) else NULL) +
			{if (skiplegend) ggplot2::theme(legend.position="none") else list(ggplot2::theme(legend.position = "bottom"))} +
			{if (dexpa$fig$skiptitles)  ggplot2::theme(plot.title =  ggplot2::element_blank()) else NULL} +
			ggplotaddons
	print(p1)
	dexpa$fig$close()
	if (returnplot) return(p1)
}
