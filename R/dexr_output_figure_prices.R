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
output_figure_prices_requested_comp_avgByTypeStartT <- function(dexpa, data, skiplegend=F) {
	
	data <- requests_prices_identify_type(dexpa, data)
	
	output_figure_lines(dexpa, data, y_column = "price", title = "Requested , average price per type and delivery start time",
			colour_column = "id", colour_legendtitle = "Run ID",
			facet_column = "Type",
			facet_ncol = 1, filename = paste("dex_prices_requested_comp_avgByTypeCTlines", 
					shbasic::shbasic_condenseRunids(data[, "id"]), sep="_"),
			alpha=1.0, ggplotaddons = list(
					if (skiplegend) ggplot2::theme(legend.position="none") else ggplot2::theme(
										legend.position = "bottom"
								),
					ggplot2::xlab("Start time"),
					ggplot2::ylab("Price (EUR)"),
					ggplot2::scale_x_datetime(),
					ggplot2::guides(colour = ggplot2::guide_legend(ncol=1))
			),  x_column = "start_time", returnplot = FALSE)
}