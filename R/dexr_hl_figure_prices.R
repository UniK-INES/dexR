#' Retrieves requests data from DB and creates figure of requested energy from generation by generation type 
#' and delivery start time.
#' 
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_prices_requested_comp_byTypeStartT <- function(dexpas, skiplegend=F) {
	data = data.frame()
	for (dp in dexpas) {
		d <- dexR::input_db_requests(dp)
		if (nrow(d) == 0) {
			# R.oo::throw.default("No requests in DB for ID ", dp$id, "!")
			futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s for ID %s!",
					dp$db$dbname,
					dp$id,
					name = "dexr.hl.requests")
		} else {
			d <- requests_filter_data(dp, d)
			data <- rbind(data, d)
		}
	}
	if (nrow(data) > 0) {
		data <- dexpa$sim$filter$requests(dexpa, data)
		dexR::output_figure_prices_requested_comp_avgByTypeStartT(dexpas[[1]], data, skiplegend=skiplegend)
	}
}