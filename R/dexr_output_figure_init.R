#' Init figure output
#' @param dexpa DEX parameter object
#' @param outdir (default: dexpa$dirs$output$figures) 
#' @param filename 
#' @param ensurePath switch
#' @return plot
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_init <- function(dexpa, outdir = dexpa$dirs$output$figures, filename, ensurePath = TRUE) {
	
	if (is.null(filename)) {
		R.oo::throw.default("Filename may not be NULL!")
	}
	if (is.null(dexpa$fig$resfactor)) {
		R.oo::throw.default("Set dexpa$fig$resFactor!")
	}
	if (is.null(dexpa$fig$width)) {
		R.oo::throw.default("Set dexpa$fig$width!")
	}
	if (is.null(dexpa$fig$height)) {
		R.oo::throw.default("Set dexpa$fig$height!")
	}
	if (is.null(dexpa$fig$numfigs) || length(dexpa$fig$numfigs) == 0) {
		R.oo::throw.default("Set dexpa$fig$numFigs (correctly)!")
	}
	
	if (is.null(dexpa$fig$outputformat)) {
		outputformat <- "png"
	} else {
		outputformat <- dexpa$fig$outputformat
	}
	
	futile.logger::flog.info("Output figure file: %s/%s.%s", 
			outdir,
			filename,
			outputformat,
			name = "dexr.output.figure.init")
	
	if (ensurePath) {
		shbasic::sh.ensurePath(outdir)
	}
	
	if (!is.null(dexpa$fig$splitfigs) && dexpa$fig$splitfigs > 0) {
		numFigs = 1
	} else {
		numFigs = dexpa$fig$numfigs
	}
	
	if(!is.null(dexpa$fig$numlines)) {
		numLines = dexpa$fig$numlines
	} else {
		if (!is.null(dexpa$fig$numcols)) {
			numLines = ceiling(numFigs / dexpa$fig$numcols)
		} else {
			numLines = numFigs
		}
	}
	
	if(!is.null(dexpa$fig$numcols)) {
		numCols = dexpa$fig$numcols
	} else {
		numCols = 1
	}
	
	futile.logger::flog.debug("Height: %f (num lines: %.1f)\nWidth:%f (number of column: %.1f)",
			dexpa$fig$height * dexpa$fig$resfactor * numLines,
			numLines,
			dexpa$fig$width * dexpa$fig$resfactor * numCols,
			numCols,
			name = "dexr.output.figure.init")
	
	if (outputformat == "png") {
		grDevices::png(file=paste(outdir, '/', filename,".png",sep=""),
				height = dexpa$fig$height * dexpa$fig$resfactor * numLines,
				width = dexpa$fig$width * dexpa$fig$resfactor * numCols,
				res=150 * dexpa$fig$resfactor,
				pointsize=12)
		
	} else if (outputformat == "jpeg") {
		grDevices::jpeg(file = paste(outdir, '/', filename, ".jpeg", sep=""),
				height = dexpa$fig$height * dexpa$fig$resfactor * numLines,
				width = dexpa$fig$width * dexpa$fig$resfactor * numCols,
				quality = 90,
				res = 150 * dexpa$fig$resfactor)
	}
}