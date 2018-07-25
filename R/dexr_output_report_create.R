#' Create the full report
#' @param dexpa parameter object 
#' @param outputfile filename of resulting PDF file
#' @param output_dir destination folder
#' @return PDF file
#' 
#' @author Sascha Holzhauer
#' @export
createFullReport <- function(dexpa, outputfile, rmdfile="DEX_report_full.Rmd", output_dir = dexpa$dirs$output$reports) {
	
	dexpa$fig$init <- function(simp, outdir, filename) {}
	dexpa$fig$close<- function() {}
	
	report.env <- new.env()
	assign("dexpa", dexpa, envir=report.env)
	
	rmarkdown::render(	input 		= system.file("reports", rmdfile, package="dexR"),
						output_file = outputfile,
						output_dir  = output_dir,
						envir = report.env)

	futile.logger::flog.info("Full report generated",
			name="dexr.output.report.full")
}
#' Create the comparison report
#' @param dexpa parameter object 
#' @param outputfile filename of resulting PDF file
#' @param output_dir destination folder
#' @return PDF file
#' 
#' @author Sascha Holzhauer
#' @export
createCompareReport <- function(dexpas, outputfile, rmdfile="DEX_report_compare.Rmd", output_dir = dexpa$dirs$output$reports) {
	
	dexpa$fig$init <- function(simp, outdir, filename) {}
	dexpa$fig$close<- function() {}
	
	report.env <- new.env()
	assign("dexpas", dexpas, envir=report.env)	
	for (i in length(dexpas)) {
		assign(names(dexpas)[[i]], dexpas[[i]], envir=report.env)
	}
	
	# FIXME fails: using the wrong user in db connection
	tryCatch(rmarkdown::render(	input = system.file("reports", rmdfile, package="dexR"),
			output_file = outputfile,
			output_dir  = output_dir,
			envir = report.env), error=function(e) {
				print(paste("Error generating report ",e,sep=""))
			})

	futile.logger::flog.info("Comparison report generated",
			name="dexr.output.report.compare")
}
