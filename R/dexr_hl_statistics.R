#' Retrieves clearing and request information an outputs aggregated figures in a table
#' 
#' @param dexpas  
#' @return table containing aggregated figures
#' 
#' @author Sascha Holzhauer
#' @export
hl_statistics_energy <- function(dexpa) {
  hl_statistics_comp_energy(setNames(list(dexpa), dexpa$sim$id))
}
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
		if (nrow(cinfo) == 0) {
			futile.logger::flog.warn("No clearing information retrieved from PostgreSQL database %s for ID %s!",
				dp$db$dbname,
				dp$sim$id,
				name = "dexr.hl.statistics")
		} else {
			cinfo$id <- dp$sim$id
			cinfos <- rbind(cinfos,cinfo)
		}
		
		requestdatum <- input_db_requests(dp)
		if (nrow(requestdatum) == 0) {
			futile.logger::flog.warn("No request information retrieved from PostgreSQL database %s for ID %s!",
				dp$db$dbname,
				dp$sim$id,
				name = "dexr.hl.statistics")
		} else {
			requestdatum$id <- dp$sim$id
			requestdata <- rbind(requestdata, requestdatum)
		}

	}
	
	output_statistics_comp_energy(dexpa = dexpas[[1]], cinfos = cinfos, requestdata = requestdata)
}
#' Retrieves clearing and request information an outputs aggregated cost figures in a table
#' 
#' @param dexpas  
#' @return table containing aggregated figures
#' 
#' @author Sascha Holzhauer
#' @export
hl_statistics_costs <- function(dexpa) {
  hl_statistics_comp_costs(setNames(list(dexpa), dexpa$sim$id))
}
#' Retrieves clearing and request information an outputs aggregated costs figures in a table
#' 
#' @param dexpas  
#' @return table containing aggregated figures
#' 
#' @author Sascha Holzhauer
#' @export
hl_statistics_comp_costs <- function(dexpas) {
  
  cinfos = data.frame()
  requestdata = data.frame()
  
  for (dp in dexpas) {
    cinfo <- dexR::input_db_clearings(dp)
    if (nrow(cinfo) == 0) {
      futile.logger::flog.warn("No clearing information retrieved from PostgreSQL database %s for ID %s!",
                               dp$db$dbname,
                               dp$sim$id,
                               name = "dexr.hl.statistics")
    } else {
      cinfo$id <- dp$sim$id
      cinfos <- rbind(cinfos,cinfo)
    }
    
    requestdatum <- input_db_requests(dp)
    if (nrow(requestdatum) == 0) {
      futile.logger::flog.warn("No request information retrieved from PostgreSQL database %s for ID %s!",
                               dp$db$dbname,
                               dp$sim$id,
                               name = "dexr.hl.statistics")
    } else {
      requestdatum$id <- dp$sim$id
      requestdata <- rbind(requestdata, requestdatum)
    }
    
  }
  
  output_statistics_comp_costs(dexpa = dexpas[[1]], cinfos = cinfos, requestdata = requestdata)
}
