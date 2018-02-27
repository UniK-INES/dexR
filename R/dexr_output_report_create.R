#' Create the full report
#' @param dexpa parameter object 
#' @param outputfile filename of resulting PDF file
#' @param output_dir destination folder
#' @return PDF file
#' 
#' @author Sascha Holzhauer
#' @export
createFullReport <- function(dexpa, outputfile, output_dir = dexpa$dirs$output$reports) {
	
	dexpa$fig$init <- function(simp, outdir, filename) {}
	dexpa$fig$close<- function() {}
	
	rmarkdown::render(	input 		= system.file("reports", "DEX_report_full.Rmd", package="dexR"),
						output_file = outputfile,
						output_dir  = output_dir)
}
