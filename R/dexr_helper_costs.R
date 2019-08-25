#' Add a column with generation (Pv, Wind, StorageOut) and load type (StorageIn) per Id.
#' 
#' @param dexpa 
#' @param data  
#' @return data
#' 
#' @author Sascha Holzhauer
#' @export
requests_energy_identify_type <- function(dexpa, data) {
	data <- requests_identify_type(data, dataexp='df[r, if(df$status==2) "energy_accepted" else "energy_requested"]')
	data <- reshape2::melt(data, id.vars=c("id", "start_time"), variable.name = "Type",
			value.name = "energy")
	return(data)
}