#' Retrieves clearing and request information an outputs aggregated figures in a table
#' 
#' @param dexpas  
#' @return table containing aggregated figures
#' 
#' @author Sascha Holzhauer
#' @export
hl_statistics_comp_energy <- function(dexpas) {

	cinfos = data.frame()
	requestdata = data.frame()

	for (dp in dexpas) {
		cinfo <- dexR::input_db_clearings(dp)
		if (nrow(cinfos) == 0) {
			futile.logger::flog.warn("No clearing information retrieved from PostgreSQL database %s for ID %s!",
				dp$db$dbname,
				dp$id,
				name = "dexr.hl.statistics")
		} else {
			cinfo$id <- dexR::input_db_runID(dp)
				cinfos <- rbind(cinfos,cinfo)
		}
		
		requestdatum <- input_db_requests(dp)
		if (nrow(requestdatum) == 0) {
			futile.logger::flog.warn("No request information retrieved from PostgreSQL database %s for ID %s!",
				dp$db$dbname,
				dp$id,
				name = "dexr.hl.statistics")
		} else {
			requestdatum$id <- dexR::input_db_runID(dp)
			requestdatum <- rbind(requestdata, requestdatum)
		}

	}
	
	output_statistics_comp_energy(dexpa = dexpas[[1]], cinfos = cinfos, requestdata = requestdata)
}
