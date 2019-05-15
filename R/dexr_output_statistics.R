#' Calculate aggregate figures of traded energy.
#' @param dexpa 
#' @param cinfos 
#' @return markdown table
#' 
#' @author Sascha Holzhauer
#' @export
output_statistics_comp_energy <- function(dexpa, cinfos, requestdata) {

	requestdata <- dplyr::filter(requestdata, status %in% c(1,2,3))
	requestTable <- plyr::ddply(requestdata, c("id"), function(data) {
				# data <- requestdata[requestdata$id == "enavi_08-01",]
				result <- data.frame(
				id = unique(data$id),
				energy_accepted = sum(data[data$energy_accepted > 0, "energy_accepted"], na.rm=T),
				energy_requested_supply = sum(data[data$energy_requested < 0, "energy_requested"], na.rm=T),
				energy_requested_demand = sum(data[data$energy_requested > 0, "energy_requested"], na.rm=T))
				result
			})
	
	cinfosTable <- plyr::ddply(cinfos, c("id"), function(data) {
				result <- data.frame(
						id = unique(data$id),
						energy_cleared = sum(data$energy_cleared, na.rm=T))
				result
			})
	
	if (nrow(requestTable) == 0) {
		futile.logger::flog.warn("Request data has no rows!", 
				"dexr.output.stats.comp.energy")
	} else if (nrow(cinfosTable) == 0) {
		futile.logger::flog.warn("Clearing information has no rows!", 
				"dexr.output.stats.comp.energy")
	} else {
		aggTable <- merge(requestTable, cinfosTable, by="id", all=T)
		
		knitr::kable(aggTable, format="markdown", caption="Aggregated energy information",
				digits = 3,
				col.names = c(	"ID",
						"Energy accepted",
						"Requested energy (asks/supply)",
						"Requested energy (bids/demand)",
						"Energy cleared"))
	}
}
