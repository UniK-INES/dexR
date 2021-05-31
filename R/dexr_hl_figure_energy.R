#' Retrieves requests data from DB and creates figure of the received ernergy per 
#' status by delivery start time.
#' 
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_energy_requested_byStatusByStartT <- function(dexpa, skiplegend=F, type="residual") {
	data <- input_db_requests(dexpa)
	if (nrow(data) > 0) {
		data <- dexpa$sim$filter$requests(dexpa, data)
		output_figure_energy_requested_byStatusByStartT(dexpa, data, type=type, skiplegend=skiplegend)
	} else {
		futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s!",
				dexpa$db$dbname,
				name = "dexr.hl.requests")
	}
}
#' Retrieves requests data from DB and creates figure of requested energy by delivery start time 
#' (generation, load, and residual as lines).
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_energy_requested_sumLoadGenByStartT <- function(dexpa) {
	data = data.frame()
	products <- input_db_param_products(dexpa)
		
	d <- input_db_requests(dexpa)
	if (nrow(d) == 0) {
		# R.oo::throw.default("No requests in DB for ID ", dp$id, "!")
		futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s for ID %s!",
				dexpa$db$dbname,
				dexpa$id,
				name = "dexr.hl.requests")
	} else {
		d$id <- input_db_runID(dexpa)	
		
		# filter requests (ACCEPTED, UNHANDLED, und DECLINED@last auction of each delivery interval)
			
		# DECLINED && submission_time > (start_time + closing_time - auction_interval)
		# d = d[1:5,]
		d[d$status %in% c(0,1,2) | (d$status==3 & d$submission_time > d$start_time - 
				# lubridate obviously ignores negative durations
				(lubridate::as.duration(paste(products[match(d$product_id,products$description), "closing_time"],"in",sep="")) + 
					lubridate::as.duration(paste(products[match(d$product_id,products$description), "auction_interval"],"in",sep="")))),]
		data <- dexpa$sim$filter$requests(dexpa, d)
		dexR::output_figure_energy_requested_sumByLoadGenByStartT(dexpa, data)
	}
}
#' Retrieves requests data from DB and creates figure of requested energy summed by delivery start time.
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_energy_requested_sumByStartT <- function(dexpa) {
	data <- input_db_requests(dexpa)
	if (nrow(data) > 0) {
		data <- dexpa$sim$filter$requests(dexpa, data)
		output_figure_energy_requested_sumByStartT(dexpa, data)
	} else {
		futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s!",
				dexpa$db$dbname,
				name = "dexr.hl.requests")
	}
}
#' Retrieves requests data from DB and creates figure of requested energy per 
#' status by delivery start time.
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_energy_requested_comp_byStatusByStartT <- function(dexpas, skiplegend=F, type="residual") {
	data = data.frame()
	for (dp in dexpas) {
		# dp = dexpas[[1]]
		# dp = dexpas[[2]]
		d <- input_db_requests(dp)
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
		output_figure_energy_requested_comp_byStatusByStartT(dexpas[[1]], data, type=type, skiplegend=skiplegend)
	}
}
#' Retrieves requests data from DB and creates figure of requested energy by delivery start time.
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_energy_requested_comp_sumByStartT <- function(dexpas) {
	data = data.frame()
	for (dp in dexpas) {
		# dp = dexpas[[1]]
		d <- input_db_requests(dp)
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
		output_figure_energy_requested_comp_sumByStartT(dexpas[[1]], data)
	}
}
#' Retrieves requests data from DB and creates figure of requested energy by delivery start time 
#' (generation, load, and residual as lines). 
#' 
#' Considered stati for various product configurations:
#' Single product, single auction: ACCEPTED, PARTYL_ACCEPTED, DECLINED
#' Single product, multiple auctions: ACCEPTED, PARTYL_ACCEPTED (energy_accepted, last auction: energy_requested, DECLINED (last auction)
#' Multiple products: ACCEPTED, PARTYL_ACCEPTED (actually, PARTLY_ACCEPTED (energy_requested) and DECLINED need to be considered for the last
#' auction across products, which is not yet implemented).
#' 
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_energy_requested_comp_sumLoadGenByStartT <- function(dexpas) {
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
			d <- dexR::requests_filter_data(dp, d)
			data <- rbind(data, d)
		}
	}
	#df <- data[data$id == data[1,"id"] & data$username == "n5_enavi02",]
	if (nrow(data) > 0) {
		data <- dexpa$sim$filter$requests(dexpas[[1]], data)
		data <- dexR::map_requests2intervals_energy(dexpa, data)
		dexR::output_figure_energy_requested_comp_sumByLoadGenByStartT(dexpas[[1]], data)
	}
}
#' Retrieves requests data from DB and creates figure of requested and accepted energy by delivery start time
#' 
#' Facets: Location
#' Colour: RunID
#' LineType: Load/Generation  
#' 
#' In case stati = ALL_REQUESTED, requests are filtered according to product configurations:
#' Single product, single auction: ACCEPTED, PARTYL_ACCEPTED, DECLINED
#' Single product, multiple auctions: ACCEPTED, PARTYL_ACCEPTED (energy_accepted, last auction: energy_requested, DECLINED (last auction)
#' Multiple products: ACCEPTED, PARTYL_ACCEPTED (actually, PARTLY_ACCEPTED (energy_requested) and DECLINED need to be considered for the last
#' auction across products, which is not yet implemented).
#' 
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_energy_status_comp_sumLoadGenByLocationAndStartT <- function(dexpas, 
		stati=c("ALL_REQUESTED", "ACCEPTED", "PARTLY_ACCEPTED", "DECLINED"),
		ggplotaddons = NULL) {
	data = data.frame()
	# stati=c("ACCEPTED", "PARTLY_ACCEPTED")
	for (dp in dexpas) {
		# dp = dexpas[[1]]
		d <- dexR::input_db_requests(dp)
		if (nrow(d) == 0) {
			# R.oo::throw.default("No requests in DB for ID ", dp$id, "!")
			futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s for ID %s!",
					dp$db$dbname,
					dp$id,
					name = "dexr.hl.requests")
		} else {
			if ("ALL_REQUESTED" %in% stati) {
				d <- dexR::requests_filter_data(dp, d)	
			} else {
				d$id <- dexR::input_db_runID(dp)	
				d <- d[d$status %in% as.numeric(names(dexpa$naming$statuses[dexpa$naming$statuses %in% stati])),]
				if ("DECLINED" %in% stati) {
					# In this case, the not-accepted share of partly accepted is counted as well!
					d[, "energy_accepted"] = d[, "energy_requested"]
				}
			}
			data <- rbind(data, d)
		}
	}
	#df <- data[data$id == data[1,"id"] & data$username == "n5_enavi02",]
	if (nrow(data) > 0) {
		data <- dexpa$sim$filter$requests(dexpas[[1]], data)
		data <- dexR::map_requests2intervals_energy(dexpa, data, location = T)
		data <- dplyr::filter(data, Type!="Residual")
		dexR::output_figure_energy_status_comp_sumByLocationLoadGenByStartT(dexpas[[1]], data, status = paste(stati, sep="-"),
				ggplotaddons = ggplotaddons)
	}
}
#' Retrieves requests data from DB and creates figure of requested and accepted power by delivery start time
#' 
#' Facets: Location
#' Colour: RunID
#' LineType: Load/Generation  
#' 
#' In case stati = ALL_REQUESTED, requests are filtered according to product configurations:
#' Single product, single auction: ACCEPTED, PARTYL_ACCEPTED, DECLINED
#' Single product, multiple auctions: ACCEPTED, PARTYL_ACCEPTED (energy_accepted, last auction: energy_requested, DECLINED (last auction)
#' Multiple products: ACCEPTED, PARTYL_ACCEPTED (actually, PARTLY_ACCEPTED (energy_requested) and DECLINED need to be considered for the last
#' auction across products, which is not yet implemented).
#' 
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_power_status_comp_sumLoadGenByLocationAndStartT <- function(dexpas, 
		stati=c("ALL_REQUESTED", "ACCEPTED", "PARTLY_ACCEPTED", "DECLINED"),
		ggplotaddons = NULL) {
	data = data.frame()
	# stati=c("ACCEPTED", "PARTLY_ACCEPTED")
	for (dp in dexpas) {
		# dp = dexpas[[1]]
		d <- dexR::input_db_requests(dp)
		if (nrow(d) == 0) {
			# R.oo::throw.default("No requests in DB for ID ", dp$id, "!")
			futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s for ID %s!",
					dp$db$dbname,
					dp$id,
					name = "dexr.hl.requests")
		} else {
			if ("ALL_REQUESTED" %in% stati) {
				d <- dexR::requests_filter_data(dp, d)	
			} else {
				d$id <- dexR::input_db_runID(dp)	
				d <- d[d$status %in% as.numeric(names(dexpa$naming$statuses[dexpa$naming$statuses %in% stati])),]
				if ("DECLINED" %in% stati) {
					# In this case, the not-accepted share of partly accepted is counted as well!
					d[, "energy_accepted"] = d[, "energy_requested"]
				}
			}
			data <- rbind(data, d)
		}
	}
	#df <- data[data$id == data[1,"id"] & data$username == "n5_enavi02",]
	if (nrow(data) > 0) {
		data <- dexpa$sim$filter$requests(dexpas[[1]], data)
		data <- dexR::map_requests2intervals_energy(dexpa, data, location = T)
		
		data <- dplyr::filter(data, Type!="Residual") %>% 
				dplyr::rename(Power = Energy) %>% # Rename Energy to Power
				dplyr::mutate(Power=Power*(60*60/dexpa$sim$deliveryinterval))
		dexR::output_figure_power_status_comp_sumByLocationLoadGenByStartT(dexpas[[1]], data, status = paste(stati, sep="-"),
				ggplotaddons = ggplotaddons)
	}
}
#' Retrieves requests data from DB and creates figure of power balance by area/location
#' by delivery start time
#' 
#' Facets: Location
#' Colour: RunID
#' LineType: Load/Generation  
#' 
#' In case stati = ALL_REQUESTED, requests are filtered according to product configurations:
#' Single product, single auction: ACCEPTED, PARTYL_ACCEPTED, DECLINED
#' Single product, multiple auctions: ACCEPTED, PARTYL_ACCEPTED (energy_accepted, last auction: energy_requested, DECLINED (last auction)
#' Multiple products: ACCEPTED, PARTYL_ACCEPTED (actually, PARTLY_ACCEPTED (energy_requested) and DECLINED need to be considered for the last
#' auction across products, which is not yet implemented).
#' 
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_power_status_comp_sumBalanceByLocationAndStartT <- function(dexpas, 
		stati=c("ALL_REQUESTED", "ACCEPTED", "PARTLY_ACCEPTED", "DECLINED"),
		ggplotaddons = NULL) {
	data = data.frame()
	# stati=c("ACCEPTED", "PARTLY_ACCEPTED")
	for (dp in dexpas) {
		# dp = dexpas[[1]]
		d <- dexR::input_db_requests(dp)
		if (nrow(d) == 0) {
			# R.oo::throw.default("No requests in DB for ID ", dp$id, "!")
			futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s for ID %s!",
					dp$db$dbname,
					dp$id,
					name = "dexr.hl.requests")
		} else {
			if ("ALL_REQUESTED" %in% stati) {
				d <- dexR::requests_filter_data(dp, d)	
			} else {
				d$id <- dexR::input_db_runID(dp)	
				d <- d[d$status %in% as.numeric(names(dexpa$naming$statuses[dexpa$naming$statuses %in% stati])),]
				if ("DECLINED" %in% stati) {
					# In this case, the not-accepted share of partly accepted is counted as well!
					d[, "energy_accepted"] = d[, "energy_requested"]
				}
			}
			data <- rbind(data, d)
		}
	}
	#df <- data[data$id == data[1,"id"] & data$username == "n5_enavi02",]
	if (nrow(data) > 0) {
		data <- dexpa$sim$filter$requests(dexpas[[1]], data)
		data <- dexR::map_requests2intervals_energy(dexpa, data, location = T)
		
		data <- dplyr::filter(data, !(Type %in% c("Gen", "Load"))) %>% 
				dplyr::rename(Power = Energy) %>% # Rename Energy to Power
				dplyr::mutate(Power=Power*(60*60/dexpa$sim$deliveryinterval))
		dexR::output_figure_power_status_comp_sumByLocationBalanceByStartT(dexpas[[1]], data, status = paste(stati, sep="-"),
				ggplotaddons = ggplotaddons)
	}
}
#' Retrieves requests data from DB and creates figure of requested energy from generation by generation type 
#' and delivery start time.
#' 
#' @param dexpa  
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_energy_requested_comp_sumGenByTypeStartT <- function(dexpas, skiplegend=F) {
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
		dexR::output_figure_energy_requested_comp_sumGenByGenTypeStartT(dexpas[[1]], data, skiplegend=skiplegend)
	}
}

