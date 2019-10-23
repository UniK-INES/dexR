#' Add a column with generation (Pv, Wind, StorageOut) and load type (StorageIn) per Id.
#' 
#' @param dexpa 
#' @param data  
#' @return data
#' 
#' @author Sascha Holzhauer
#' @export
requests_num_identify_type <- function(dexpa, data) {
	
	# check interval length:
	intervallengths <- plyr::ddply(data, c("id"), function(df) {
				shortestDelivery <- min(df$end_time - df$start_time)
				minStartTime 	 <- min(df$start_time)
				maxEndTime		 <- max(df$end_time)				
				intervallength <- length(seq(minStartTime, maxEndTime, by = shortestDelivery))
			}
	)
	
	if (max(intervallengths$V1) - min(intervallengths$V1) > dexpa$analyse$intervalsdifftoaccept) {
		futile.logger::flog.warn("Interval length differ by %f (accepted threshold is %f)! Consider to apply filters (dexpa$sim$starttime_min/max)!",
		    max(intervallengths$V1) - min(intervallengths$V1),
		    dexpa$analyse$intervalsdifftoaccept,
				name = "dexr.helper.types")
	}
	
	
	data <- plyr::ddply(data, c("id"), function(df) {
				# df <- data[data$id == unique(data$id)[2],]
				# df <- data[data$id == unique(data$id)[3],]
				
				# identify shortest delivery period:
				shortestDelivery <- min(df$end_time - df$start_time)
				minStartTime 	 <- min(df$start_time)
				maxEndTime		 <- max(df$end_time)
				
				# create interval vector of shortest delivery period:
				intervals <- seq(minStartTime, maxEndTime, by = shortestDelivery)
				intervals <- lubridate::interval(intervals[1:(length(intervals)-1)], intervals[(1+1):length(intervals)])
				result <- data.frame(start_time = intervals, 
						PV = rep(0, length(intervals)),
						Wind  = rep(0, length(intervals)),
						StorageOut  = rep(0, length(intervals)),
						StorageIn  = rep(0, length(intervals)))
				
				# aggregate energy:
				# TODO more efficient implementation!
				for (i in 1:length(intervals)) {
					result[i, "PV"] = nrow(df[lubridate::interval(df$start_time,df$end_time) %within% intervals[i] & grepl("Pv", df$cid),])
					result[i, "Wind"] = nrow(df[lubridate::interval(df$start_time,df$end_time) %within% intervals[i] & grepl("Wind", df$cid),])
					result[i, "StorageIn"] = nrow(df[lubridate::interval(df$start_time,df$end_time) %within% intervals[i] & grepl("Storage", df$cid) 
						& df$energy_requested > 0,])
					result[i, "StorageOut"] = nrow(df[lubridate::interval(df$start_time,df$end_time) %within% intervals[i] & grepl("Storage", df$cid) 
						& df$energy_requested < 0,])
					result[i, "Load"] = nrow(df[lubridate::interval(df$start_time,df$end_time) %within% intervals[i] & grepl("SimulatedDevices", df$cid),]) 
				}

				result$start_time <- lubridate::int_start(result$start_time)
				result
			})
	
	data <- reshape2::melt(data, id.vars=c("id", "start_time"), variable.name = "Type",
			value.name = "Number")
	return(data)
}
#' Add a column with generation (Pv, Wind, StorageOut) and load type (StorageIn) per Id.
#' 
#' @param dexpa 
#' @param data  
#' @return data
#' 
#' @author Sascha Holzhauer
#' @export
clients_num_identify_type <- function(dexpa, data) {
	
	# check interval length:
	intervallengths <- plyr::ddply(data, c("id"), function(df) {
				shortestDelivery <- min(df$end_time - df$start_time)
				minStartTime 	 <- min(df$start_time)
				maxEndTime		 <- max(df$end_time)				
				intervallength <- length(seq(minStartTime, maxEndTime, by = shortestDelivery))
			}
	)
	
	if (max(intervallengths$V1) - min(intervallengths$V1) > dexpa$analyse$intervalsdifftoaccept) {
		futile.logger::flog.warn("Interval length differ by %f (accepted threshold is %f)! Consider to apply filters (dexpa$sim$starttime_min/max)!",
				max(intervallengths$V1) - min(intervallengths$V1),
				dexpa$analyse$intervalsdifftoaccept,
				name = "dexr.helper.types")
	}
	
	
	data <- plyr::ddply(data, c("id"), function(df) {
				# df <- data[data$id == unique(data$id)[2],]
				# df <- data[data$id == unique(data$id)[3],]
				
				# identify shortest delivery period:
				shortestDelivery <- min(df$end_time - df$start_time)
				minStartTime 	 <- min(df$start_time)
				maxEndTime		 <- max(df$end_time)
				
				# create interval vector of shortest delivery period:
				intervals <- seq(minStartTime, maxEndTime, by = shortestDelivery)
				intervals <- lubridate::interval(intervals[1:(length(intervals)-1)], intervals[(1+1):length(intervals)])
				result <- data.frame(start_time = intervals, 
						PV = rep(0, length(intervals)),
						Wind  = rep(0, length(intervals)),
						StorageOut  = rep(0, length(intervals)),
						StorageIn  = rep(0, length(intervals)))
				
				# aggregate energy:
				# TODO more efficient implementation!
				for (i in 1:length(intervals)) {
					result[i, "PV"] = length(unique(df[lubridate::interval(df$start_time,df$end_time) %within% intervals[i] & grepl("Pv", df$cid),"username"]))
					result[i, "Wind"] = length(unique(df[lubridate::interval(df$start_time,df$end_time) %within% intervals[i] & grepl("Wind", df$cid),"username"]))
					result[i, "StorageIn"] = length(unique(df[lubridate::interval(df$start_time,df$end_time) %within% intervals[i] & grepl("Storage", df$cid) 
											& df$energy_requested > 0,"username"]))
					result[i, "StorageOut"] = length(unique(df[lubridate::interval(df$start_time,df$end_time) %within% intervals[i] & grepl("Storage", df$cid) 
											& df$energy_requested < 0,"username"]))
					result[i, "Load"] = length(unique(df[lubridate::interval(df$start_time,df$end_time) %within% intervals[i] & grepl("SimulatedDevices", df$cid),"username"])) 
				}
				
				result$start_time <- lubridate::int_start(result$start_time)
				result
			})
	
	data <- reshape2::melt(data, id.vars=c("id", "start_time"), variable.name = "Type",
			value.name = "Number")
	return(data)
}
#' Filter requests according to status and products
#' @param dexpa 
#' @param d 
#' @return 
#' 
#' @author Sascha Holzhauer
requests_filter_data <- function(dexpa, d) {
	d$id <- dexR::input_db_runID(dexpa)	
	products <- dexR::input_db_param_products(dexpa)
	if (nrow(products == 1)) {
		openings = lubridate::as.duration(paste(products[, "opening_time"],"in",sep=""))
		closings = lubridate::as.duration(paste(products[, "closing_time"],"in",sep=""))
		auction =  lubridate::as.duration(paste(products[, "auction_interval"],"in",sep=""))
		
		if(any(as.duration(openings - closings) / auction == 1)) {
			# Single product, single auction
			# filter requests (ACCEPTED, PARTLY_ACCEPTED, DECLINED
			d <- d[d$status %in% c(1,2,3),]
			d[, "energy_accepted"] = d[, "energy_requested"]
		} else {
			# Single product, multiple auctions
			# filter ACCEPTED, PARTYL_ACCEPTED (energy_accepted, last auction: energy_requested, DECLINED (last auction)
			d <- d[d$status %in% c(1,2,3),]
			consideredrows = (d$status %in% c(2,3) & d$submission_time > d$start_time - 
						# lubridate obviously ignores negative durations
						(lubridate::as.duration(paste(products[match(d$product_id,products$description), 
													"closing_time"],"in",sep="")) + 
							lubridate::as.duration(paste(products[match(d$product_id,products$description), 
													"auction_interval"],"in",sep=""))))
			d[consideredrows, "energy_accepted"] = d[consideredrows, "energy_requested"]
		}
	} else {
		# Multiple products
		futile.logger::flog.warn("Not yet fully implemented!")
		# filter requests (ACCEPTED, PARTLY_ACCEPTED, DECLINED
		d <- d[d$status %in% c(1,2,3),]
		d[, "energy_accepted"] = d[, "energy_requested"]
	}
	d
}