#' Output figure: Number of considered requests per clearing by clearing time.
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_clearing_numConsideredRequests_byCTbyProduct <- function(dexpa, data) {
	output_figure_bars(dexpa, data, y_column = "num_considered_requests", title = "Number of considered requests by clearing time and product",
			fill_column = NULL, fill_legendtitle = NULL, fill_legenditemnames = NULL,
			facet_column = "product_id", facet_ncol = 1, filename = "dex_clearing_numConsRequestsByCTbyProduct",
			alpha=1.0, ggplotaddons = NULL, x_column = "clearing_time", position = "dodge", returnplot = FALSE)
}
#' Output figure: Cleared price per clearing by clearing time and product (colour).
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_clearing_prices_byCTbyProduct <- function(dexpa, data) {
	output_figure_bars(dexpa, data, y_column = "Cleared price", title = "Cleared price by clearing time and product",
			fill_column = "product_id", fill_legendtitle = "Market product", fill_legenditemnames = NULL,
			facet_column = NULL, facet_ncol = 1, filename = "dex_clearing_clearedPriceByCTbyProduct",
			alpha=1.0, ggplotaddons = NULL, x_column = "clearing_time", position = "dodge", returnplot = FALSE)
}
#' Output figure: Mean cleared price per clearing by clearing time and product (colour).
#' @param dexpa 
#' @param data 
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_clearing_prices_byCTbyProduct <- function(dexpa, data) {
	output_figure_bars(dexpa, data, y_column = "Cleared price", title = "Cleared price by clearing time and product",
			fill_column = "product_id", fill_legendtitle = "Market product", fill_legenditemnames = NULL,
			facet_column = NULL, facet_ncol = 1, filename = "dex_clearing_clearedPriceByCTbyProduct",
			alpha=1.0, ggplotaddons = NULL, x_column = "clearing_time", position = "dodge", returnplot = FALSE)
}