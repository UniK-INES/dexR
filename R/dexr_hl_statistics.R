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
#' Table with mean and sum of gini indices across time (load and generation separately)
#' 
#' @param dexpas 
#' @return 
#' 
#' @author Sascha Holzhauer
#' @export
hl_statistics_comp_gini <- function(dexpas) {
	data = data.frame()
	for (dp in dexpas) {
		# dp = dexpas[[1]]
		d <- dexR::input_db_requests(dp, additionalwhere = "e.status IN (1,2)")
		if (nrow(d) == 0) {
			# R.oo::throw.default("No requests in DB for ID ", dp$id, "!")
			futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s for ID %s!",
					dp$db$dbname,
					dp$id,
					name = "dexr.hl.requests")
		} else {
			d$id <- dexR::input_db_runID(dp)
			d$rawid <- dp$sim$id
			data <- rbind(data, d)
		}
	}
	if (nrow(data) > 0) {
		# prepare gini:
		data <- dexpas[[1]]$sim$filter$requests(dexpas[[1]], data)
		
		load = data[data$energy_accepted > 0,]
		load <- prepare_costdata_gini(dexpas, load, type = "load")
		load$Kind = "Load"
	
		gen = data[data$energy_accepted < 0,]
		gen$energy_accepted = gen$energy_accepted * (-1)
		gen <- prepare_costdata_gini(dexpas, gen, type = "gen")
		gen$Kind = "Gen"
		
		data <- rbind(load, gen)
		
		# filter types:
		data <- data[data$Type == shbasic::shbasic_substitute(dexpa, "Gini"),]
		output_statistics_comp_gini(dexpas, data)
	}
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
#' Retrieves request information an outputs aggregated figures of number of clients in a table
#' 
#' @param dexpas  
#' @return table containing aggregated figures
#' 
#' @author Sascha Holzhauer
#' @export
hl_statistics_comp_numclients <- function(dexpas) {
	
	requestdata = data.frame()
	
	for (dp in dexpas) {
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
	
	output_statistics_comp_numclients(dexpa = dexpas[[1]], requestdata = requestdata)
}