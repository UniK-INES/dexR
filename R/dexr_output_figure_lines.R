#' Prints a lines of data as ggplot2 with potentially different colour, linetype,
#' and potentially as facet plot.
#' Uses ticks as X variable if present, runid otherwise.
#' 
#' @param dexpa parameter object
#' @param data data.frame or list that is rbinded to a data.frame
#' @param x_column column name for x data
#' @param y_column column name for y data
#' @param title figure title
#' @param colour_column column used to define colours
#' @param colour_legendtitle title for colour legend
#' @param colour_legenditemnames vector of names for colour legend items
#' @param linetype_column column used to define linetypes
#' @param linetype_legendtitle title for linetype legend
#' @param linetype_legenditemnames vector of names for linetype legend items
#' @param facet_column column used to define facets
#' @param facet_ncol number of columns of facet wrap
#' @param filename without extension
#' @param alpha transparency
#' @param ggplotaddons vector of ggplot objects to add
#' @param returnplot if true the ggplot object is returned
#' @param showsd if \code{TRUE} line shows mean and ribbons standard deviation (requires multiple data points per group)
#' @return ggplot2 line visualisation
#' @seealso \code{\link{output_figure_energy_requested_comp_sumByLoadGenByStartT}}
#'
#' @author Sascha Holzhauer
#' @export
output_figure_lines <- function(dexpa = dexpa, data = data, x_column= NULL, y_column, title = NULL,
		colour_column = NULL, colour_legendtitle = colour_column, colour_legenditemnames = NULL,
		linetype_column = NULL, linetype_legendtitle = linetype_column, linetype_legenditemnames = NULL,
		facet_column = NULL, facet_ncol = 2, filename = paste(dexpa$fig$filenameprefix, gsub(" ", "_", title), 
				shbasic::shbasic_condenseRunids(data[, "id"]), dexpa$fig$filenamepostfix, sep="_"),
		alpha = dexpa$fig$alpha, showsd = FALSE, ggplotaddons = NULL, returnplot = FALSE) {

	if (!is.data.frame(data)) {
		data <- do.call(rbind, data)
	}
	if (!is.null(linetype_column)) {
		data[,linetype_column] <- as.factor(data[,linetype_column])
	}
	if (!is.null(facet_column)) {
		if (!grepl("~",facet_column)) data[,facet_column] <- as.factor(data[,facet_column])
	}

	dexpa$fig$init(dexpa, outdir = paste(dexpa$dirs$output$figures, "lines", sep="/"), filename = filename)
	
	scaleColourElem <- NULL
	if (!is.null(colour_column)) {
		if (!is.null(dexpa$fills[[colour_column]]) && 
				length(dexpa$fills[[colour_column]]) >=  length(unique(data[, colour_column]))) {
			scaleColourElem <- ggplot2::scale_colour_manual(name=colour_legendtitle, 
					values = dexpa$fills[[colour_column]],
					labels = if(!is.null(colour_legenditemnames)) colour_legenditemnames else ggplot2::waiver())
		} else {
			if (!is.null(dexpa$fills[[colour_column]]) && 
					length(dexpa$fills[[colour_column]]) <  length(unique(data[, colour_column]))) {
				warning("Not enough colours in simp$fills[[", colour_column, "]] (", 
						length(dexpa$fills[[colour_column]]), " - needed: " , length(unique(data[, colour_column])), "[", 
						paste(unique(data[, colour_column]), collapse = "/"), "])")
			}
			scaleColourElem <- ggplot2::scale_colour_manual(name=colour_legendtitle, 
					values = topo.colors(n = length(unique(data[, colour_column]))),
					labels = if(!is.null(colour_legenditemnames)) colour_legenditemnames else ggplot2::waiver())
		}
	}
	
	scaleLinetypeElem <- NULL
	if (!is.null(linetype_column)) {
		scaleLinetypeElem <- ggplot2::scale_linetype_manual(name=linetype_legendtitle, 
				values = if (!is.null(dexpa$linetypes[[linetype_column]])) dexpa$linetypes[[linetype_column]] else 
							seq(from=1, by=1, to=length(unique(data[,linetype_column]))),
				labels = if(!is.null(linetype_legenditemnames)) linetype_legenditemnames else ggplot2::waiver())
	}
	
	facetElem = NULL
	if (!is.null(facet_column)) {
		facetElem <- ggplot2::facet_wrap(as.formula(paste("~", facet_column)), ncol = facet_ncol, scales=dexpa$fig$facet$scale) +
				ggplot2::theme(strip.text.x = element_text(size = dexpa$fig$facet$labelsize))
	}
	
	if (is.null(x_column)) {
		if ("Tick" %in% names(data)) {
			x_column = "Tick"
		} else {
			x_column = "Runid"
		}	
	}
	
	if (showsd) {
		lineelem <- list(ggplot2::stat_summary(data = data, fun.data=ggplot2::mean_se, geom = "ribbon", 
						alpha=dexpa$fig$ribbon$alpha,
						ggplot2::aes_string(x=x_column, y=y_column, fill = colour_column)),
				ggplot2::stat_summary(data = data, fun.y=mean, geom = "line", 
						ggplot2::aes_string(x = x_column, y = y_column,color=colour_column), 
							alpha=alpha, size = simp$fig$linewidth))
	} else {
		lineelem <- ggplot2::geom_line(data = data, mapping=ggplot2::aes_string(x = x_column, y = y_column,
						colour = colour_column, linetype = linetype_column), size = dexpa$fig$linewidth,
							alpha=alpha)
	}
	
	p1 <- ggplot2::ggplot() +
			lineelem +
			facetElem  +
		 	scaleColourElem +
			scaleLinetypeElem + 
			(if (!is.null(title) && title != "") ggplot2::labs(title = title) else NULL) +
			{if (dexpa$fig$skiptitles)  ggplot2::theme(plot.title =  ggplot2::element_blank()) else NULL} +
			ggplotaddons
	print(p1)
	dexpa$fig$close()
	if (returnplot) return(p1)
}