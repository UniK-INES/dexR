#' Add a column with generation (Pv, Wind, StorageOut) and load type (StorageIn) per Id.
#' 
#' @param dexpa 
#' @param data  
#' @return data
#' 
#' @author Sascha Holzhauer
#' @export
requests_prices_identify_type <- function(dexpa, data) {
	data <- plyr::ddply(data, c("id"), function(df) {
				# df <- data[data$id == data[1,"id"],]
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
						StorageIn  = rep(0, length(intervals)),
						Load  = rep(0, length(intervals)))

				counter <- data.frame(start_time = intervals, 
						PV = rep(0, length(intervals)),
						Wind  = rep(0, length(intervals)),
						StorageOut  = rep(0, length(intervals)),
						StorageIn  = rep(0, length(intervals)),
						Load  = rep(0, length(intervals)))
				
				# aggregate energy:
				# TODO more efficient implementation!
				for (r in 1:nrow(df)) {
					# r = 1
					# assign the according energy to all intervals that are within the delivery period:
					if (grepl("Pv", df[r, "cid"])) {
						result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
								"PV"] = result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
										"PV"] + df[r, "price_requested"]
						
						counter[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
								"PV"] = counter[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
										"PV"] + 1
						
					} else if (grepl("Wind", df[r, "cid"])) {
						result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
								"Wind"] = result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
										"Wind"] + df[r, "price_requested"]
						
						counter[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
								"Wind"] = counter[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
										"Wind"] + 1
								
					} else if (grepl("Storage", df[r, "cid"]) & (df[r, "energy_requested"] < 0)) {
						result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
								"StorageOut"] = result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
										"StorageOut"] + df[r, "price_requested"]
						
						counter[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
								"StorageOut"] = counter[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
										"StorageOut"] + 1
						
					} else if (grepl("Storage", df[r, "cid"]) & (df[r, "energy_requested"] > 0)) {
						result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
								"StorageIn"] = result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
										"StorageIn"] + df[r, "price_requested"]
						
						counter[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
								"StorageIn"] = counter[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
										"StorageIn"] + 1 
					} else if (grepl("_EnaviSimulatedDevices", df[r,"cid"])) {
						result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
								"Load"] = result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
										"Load"] + df[r, "price_requested"]
						
						counter[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
								"Load"] = counter[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
										"Load"] + 1
					}
				}
				result$start_time <- lubridate::int_start(result$start_time)
				result[,"PV"] = result[,"PV"] / counter[,"PV"] 
				result[,"Wind"] = result[,"Wind"] / counter[,"Wind"] 
				result[,"StorageOut"] = result[,"StorageOut"] / counter[,"StorageOut"] 
				result[,"StorageIn"] = result[,"StorageIn"] / counter[,"StorageIn"]
				result[,"Load"] = result[,"Load"] / counter[,"Load"] 
				result
			})
	
	data <- reshape2::melt(data, id.vars=c("id", "start_time"), variable.name = "Type",
			value.name = "price")
	return(data)
}