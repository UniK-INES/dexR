#' Add a column with generation (Pv, Wind, StorageOut) and load type (StorageIn) per Id.
#' 
#' @param dexpa 
#' @param data  
#' @return data
#' 
#' @author Sascha Holzhauer
#' @export
requests_energy_identify_type <- function(dexpa, data) {
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
						StorageIn  = rep(0, length(intervals)))
				
				# aggregate energy:
				# TODO more efficient implementation!
				for (r in 1:nrow(df)) {
					# r = 1
					# assign the according energy to all intervals that are within the delivery period:
					if (grepl("Pv", df[r, "cid"])) {
						result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
								"PV"] = result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
										"PV"] + df[r, if(df$status==2) "energy_accepted" else "energy_requested"]
					} else if (grepl("Wind", df[r, "cid"])) {
						result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
								"Wind"] = result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
										"Wind"] + df[r, if(df$status==2) "energy_accepted" else "energy_requested"]			
					} else if (grepl("Storage", df[r, "cid"]) & (df[r, "energy_requested"] < 0)) {
						result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
								"StorageOut"] = result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
										"StorageOut"] + df[r, if(df$status==2) "energy_accepted" else "energy_requested"]
					} else if (grepl("Storage", df[r, "cid"]) & (df[r, "energy_requested"] > 0)) {
						result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
								"StorageIn"] = result[intervals %within% lubridate::interval(df[r, "start_time"],df[r, "end_time"]),
										"StorageIn"] + df[r, if(df$status==2) "energy_accepted" else "energy_requested"]
					} 
				}
				result$start_time <- lubridate::int_start(result$start_time)
				result
			})
	
	data <- reshape2::melt(data, id.vars=c("id", "start_time"), variable.name = "Type",
			value.name = "energy")
	return(data)
}