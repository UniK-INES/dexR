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
output_figure_bars <- function(dexpa, data, y_column, title = NULL,
		fill_column = NULL, fill_legendtitle = fill_column, fill_legenditemnames = NULL,
		facet_column = NULL, facet_ncol = 4, filename = paste(title, shbasic::shbasic_condenseRunids(data[, "Runid"]), sep="_"),
		alpha=1.0, ggplotaddons = NULL, x_column = "ID", 
		group_column = NULL, group_colors = NULL, position = "dodge", returnplot = FALSE) {
	
	if (!is.data.frame(data)) {
		data <- do.call(rbind, data)
	}
	if (!is.null(facet_column)) {
		data[,facet_column] <- as.factor(data[,facet_column])
	}
	
	dexpa$fig$init(dexpa, outdir = paste(dexpa$dirs$output$figures, "bars", sep="/"), filename = filename)
	
	scaleFillElem <- NULL
	if (!is.null(fill_column)) {
		
		if (!is.null(dexpa$fills[[fill_column]]) && 
				length(dexpa$fills[[fill_column]]) >=  length(unique(data[, fill_column]))) {
			scaleFillElem <- ggplot2::scale_fill_manual(name=fill_legendtitle, 
					values = dexpa$fills[[fill_column]],
					labels = if(!is.null(fill_legenditemnames)) fill_legenditemnames else ggplot2::waiver())
		} else {
			if (!is.null(dexpa$fills[[fill_column]])) {
				warning("Not enough colours in dexpa$fills[[", fill_column, "]] (", 
						length(dexpa$fills[[fill_column]]), " - needed: " , length(unique(data[, fill_column])), ")")
			}
			scaleFillElem <- ggplot2::scale_fill_manual(name=fill_legendtitle, guide = ggplot2::guide_legend(
							ncol=dexpa$fig$legend$ncols),
					values =  if(is.null(group_colors)) topo.colors(n = 
										length(unique(data[, fill_column]))) else group_colors,
					labels = if(!is.null(fill_legenditemnames)) fill_legenditemnames else ggplot2::waiver())
		}
	}
	
	facetElem = NULL
	if (!is.null(facet_column)) {
		facetElem <- ggplot2::facet_wrap(as.formula(paste("~", facet_column)), ncol = facet_ncol)
	}
	
	p1 <- ggplot2::ggplot() +
			ggplot2::geom_bar(data = data, alpha=alpha, mapping=ggplot2::aes_string(x = x_column,  y = y_column,
							fill = fill_column, group = group_column), stat="identity", position = position) +
			facetElem  +
			scaleFillElem +
			{if (!is.null(title) && title != "") ggplot2::labs(title = title) else NULL} +
			(if (!is.null(fill_column) && x_column == fill_column) ggplot2::scale_x_discrete(breaks=NULL) else NULL) +
			ggplotaddons +
			ggplot2::theme(plot.margin = grid::unit(c(0.5,0.9,0.5,0.3), "cm")) 
	print(p1)
	dexpa$fig$close()
	if (returnplot) return(p1)
}