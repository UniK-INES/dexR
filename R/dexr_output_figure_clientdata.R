#' Prints a bar of data as ggplot2 with potentially different colour, linetype,
#' and potentially as facet plot.
#' 
#' @param dexpa parameter object
#' @param data data.frame or list that is rbinded to a data.frame
#' @param y_column
#' @param title figure title
#' @param fill_column column used to define colours
#' @param fill_legendtitle title for colour legend
#' @param fill_legenditemnames vector of names for colour legend items
#' @param facet_column column used to define facets
#' @param facet_ncol number of columns of facet wrap
#' @param filename without extension
#' @param alpha
#' @param ggplotaddons vector of ggplot objects to add
#' @param x_column
#' @param position passed to geom_bar
#' @param returnplot if true the ggplot object is returned
#' @return ggplot2 line visualisation
#' @example demo/example_visualise_bars.R
#'
#' @author Sascha Holzhauer
#' @export
output_figure_clientdata <- function(dexpa, d = input_csv_clientdata(dexpa), title = NULL,
		filename = paste(title, shbasic::shbasic_condenseRunids(data[, "Runid"]), sep="_"),
		ggplotaddons = NULL, returnplot = FALSE, facet_ncol = 2) {
	
	dexpa$fig$init(dexpa, outdir = paste(dexpa$dirs$output$figures, "bars", sep="/"), filename = filename)
	
	colnames(d)[colnames(d)=="name_emg"] <- "Client"
	data <- reshape2::melt(d, id.vars=c("Client"), variable.name = "Parameter", value.name = "Value")
	
	p1 <- ggplot2::ggplot() +
			ggplot2::geom_bar(data = data, mapping=ggplot2::aes_string(x = "Client",  y = "Value",
					fill = "Client"), stat="identity") +
			ggplot2::facet_wrap("Parameter", ncol = facet_ncol)  +
			{if (!is.null(title) && title != "") ggplot2::labs(title = title) else NULL} +
			ggplotaddons +
			ggplot2::theme(plot.margin = grid::unit(c(0.5,0.9,0.5,0.3), "cm")) 
	print(p1)
	dexpa$fig$close()
	if (returnplot) return(p1)
}