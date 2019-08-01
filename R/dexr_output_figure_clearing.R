#' Output figure: Number of considered requests per clearing by clearing time.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_clearing_numConsideredRequests_byCTbyProduct <- function(dexpa, data, 
		filename = "dex_clearing_numConsRequestsByCTbyProduct") {
	output_figure_bars(dexpa, data, y_column = "num_considered_requests", title = "Number of considered requests by clearing time and product",
			fill_column = NULL, fill_legendtitle = NULL, fill_legenditemnames = NULL,
			facet_column = "product_id", facet_ncol = 1, filename = filename,
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Clearing time"),
					ggplot2::ylab("Number of considered requests")
			), x_column = "clearing_time", position = "dodge", returnplot = FALSE)
}
#' Output figure: Number of considered requests per clearing and run ID by clearing time. Adds a legend for run ID.
#' @param dexpa 
#' @param data 
#' @return figure (file)
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_clearing_comp_numConsideredRequests_byCTbyProduct <- function(dexpa, data,
		filename = "dex_clearing_comp_numConsRequestsByCTbyProduct") {
	output_figure_bars(dexpa, data, y_column = "num_considered_requests", title = "Number of considered requests by clearing time and product",
			fill_column = "id", fill_legendtitle = "Run", fill_legenditemnames = NULL,
			facet_column = "product_id", facet_ncol = 1, filename = filename,
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Clearing time"),
					ggplot2::ylab("Number of considered requests"),
					ggplot2::theme(
							legend.position = "bottom"
					)
			), x_column = "clearing_time", position = "dodge", returnplot = FALSE)
}
#' Output figure: Cleared price per clearing by clearing time and product (colour).
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_clearing_prices_byCTbyProduct <- function(dexpa, data, filename = "dex_clearing_clearedPriceByCTbyProduct") {
	
	output_figure_bars(dexpa, data, y_column = "price_cleared", title = "Cleared price by clearing time and product",
			fill_column = "product_id", fill_legendtitle = "Market product", fill_legenditemnames = NULL,
			facet_column = NULL, facet_ncol = 1, filename = filename,
			alpha = 1.0, ggplotaddons = list(
					ggplot2::xlab("Clearing time"),
					ggplot2::ylab("Price cleared"),
					ggplot2::theme(
							legend.position = "bottom",
							plot.margin = grid::unit(c(0.5,0.4,0.5,0.3), "cm")
					)
			),  x_column = "clearing_time", position = "dodge", returnplot = FALSE)
}
#' Output figure: Cleared price per clearing by clearing time and product (colour) and run ID.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_clearing_comp_prices_byCTbyProduct <- function(dexpa, data,
		filename = "dex_clearing_clearedPriceByCTbyProduct") {
	data <- plyr::ddply(data, c("id", "clearing_time", "product_id"), function(df) {
				d <- data.frame(
						price_cleared = sum(df$price_cleared * df$energy_cleared, na.rm=T) / 
								sum(df$energy_cleared, na.rm=T))	
				d
			})
	
	output_figure_bars(dexpa, data, y_column = "price_cleared", title = "Cleared price by clearing time and product",
			fill_column = "id", fill_legendtitle = "Run ID", fill_legenditemnames = NULL,
			facet_column = "product_id", facet_ncol = 1, filename = filename,
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Clearing time"),
					ggplot2::ylab("Price cleared"),
					ggplot2::theme(
							legend.position = "bottom",
							plot.margin = grid::unit(c(0.5,0.4,0.5,0.3), "cm")
					)
			),  x_column = "clearing_time", position = "dodge", returnplot = FALSE)
}
#' Output figure: Cleared price per clearing by clearing time and product (colour) and run ID.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_clearing_comp_prices_bySTbyProduct <- function(dexpa, data) {
	data <- plyr::ddply(data, c("id", "delivery_period_start", "product_id"), function(df) {
				d <- data.frame(price_cleared = sum(df$price_cleared * df$energy_cleared, na.rm=T) / 
								sum(df$energy_cleared, na.rm=T))	
				d
			})
	
	output_figure_bars(dexpa, data, y_column = "price_cleared", title = "Cleared price by delivery start time and product",
			fill_column = "id", fill_legendtitle = "Run ID", fill_legenditemnames = NULL,
			facet_column = "product_id", facet_ncol = 1, filename = "dex_clearing_clearedPriceByCTbyProduct",
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Clearing time"),
					ggplot2::ylab("Price cleared"),
					ggplot2::theme(
							legend.position = "bottom",
							plot.margin = grid::unit(c(0.5,0.4,0.5,0.3), "cm")
					)
			),  x_column = "delivery_period_start", position = "dodge", returnplot = FALSE)
}
#' Output figure: Cleared price per clearing by delivery start time.
#' 
#' @param dexpa 
#' @param data  
#' @return figure
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_clearing_prices_energy_byST <- function(dexpa, data) {
	data <- plyr::ddply(data, c("delivery_period_start"), function(df) {
			# df = data[data$product_id == "Uniform, 15min" &  data$delivery_period_start == data$delivery_period_start[120],]
			d <- data.frame(price_cleared = sum(df$price_cleared * df$energy_cleared, na.rm=T) / 
							sum(df$energy_cleared, na.rm=T))	
			d
		})

	output_figure_lines(dexpa, data, y_column = "value", 
			#title = "Requested energy by generation/load and delivery start time",
			colour_column = "Type",
			linetype_column = "id", linetype_legendtitle = "Run ID",
			facet_ncol = 1, filename = "dex_energy_requested_comp_sumGenLoadByCTlines",
			alpha=1.0, ggplotaddons = list(
					ggplot2::xlab("Delivery Start time"),
					#ggplot2::ylab("Requested energy"),
					ggplot2::scale_x_datetime() +
							ggplot2::theme(
									legend.position = "bottom"
							)
			),  x_column = "delivery_period_start", returnplot = FALSE)
}