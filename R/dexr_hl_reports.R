#' Create a report for the given <code>ids</code> based on the given <code>dexpa</code> (e.g. dumpdir).
#' 
#' @param dexpa parameter object required for e.g. dump dir configuration 
#' @param ids IDs of runs to compare
#' @param reporttmpl template for report
#' @param outfileprefix prefix for output filename
#' @return comparison report
#' 
#' @author Sascha Holzhauer
#' @export
hl_reports_comp <- function(dexpa, ids, reporttmpl="DEX_report_compare_slides.Rmd",
		outfileprefix = paste(dexpa$sim$project, "_ComparisonReport", sep="_")) {
	dexpas <- dexR::create_dexpas(ids)

	dexR::input_db_dumps2db(dexpas)

	dexR::createCompareReport(dexpas, 
		rmdfile = reporttmpl, 
		outputfile = paste(outfileprefix, "__", paste(lapply(dexpas, function(x) x$sim$id), collapse="__"), ".pdf", sep=""))

	## drop DBs:
	dexR::input_db_dropdbs(dexpas)
}
