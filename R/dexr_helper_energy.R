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
#' Map request data to intervals and calculate residuals
#' 
#' @param dexpa parameter object
#' @param data request data
#' @return data molten data.frame
#' 
#' @author Sascha Holzhauer
#' @export
map_requests2intervals_energy <- function(dexpa, data) {
	# identify shortest delivery in seconds:
	shortestDelivery <- numbers::mGCD(unique(as.numeric(data$end_time) - as.numeric(data$start_time)))
	
	data <- plyr::ddply(data, c("id"), function(df) {
		# df <- data[data$id == unique(data[,"id"])[3],]
		# df <- data[data$id == data[1,"id"] & data$username == "n5_enavi02",]

		minStartTime 	 <- min(df$start_time)
		maxEndTime		 <- max(df$end_time)
		
		# create interval vector of shortest delivery period:
		intervals <- seq(minStartTime, maxEndTime, by = lubridate::dseconds(shortestDelivery))
		intervals <- lubridate::interval(intervals[1:(length(intervals)-1)], intervals[(1+1):length(intervals)])
		
		types = c("Gen", "Load")
		
		df[df$energy_requested < 0, "type"] <- types[1]
		df[df$energy_requested >= 0, "type"] <- types[2]
		
		# convert according to delivery duration (i.e. divide by how often the figure is part of an interval):
		df$energy_accepted <-  df$energy_accepted * shortestDelivery/(as.numeric(df$end_time) - as.numeric(df$start_time))
		
		# intervals <- intervals[100:150]
		result = expand.grid(Type=types, start_time=1:length(intervals), stringsAsFactors = F)
		d <- data.table(result, key=c("Type", "start_time"))
		# Type = "Load"; start_time = 1; df <- df[5000:10000,]
				
		d <- d[, list("Value"=sum(df[df$type == Type & 
								intervals[start_time] %within% lubridate::interval(df$start_time, df$end_time), 
										"energy_accepted"])), by=c("Type", "start_time")]		
		# d <- d[d$Type == "Load", ]; plot(d$start_time, d$Value)
				
		d$Type <- as.factor(d$Type)
		d2 <- reshape2::dcast(setDF(d), start_time~Type, sum)
		# calculate residuals:
		d2$Residual <- d2$Load + d2$Gen
		d2$Gen <- -d2$Gen
		d2$start_time <- lubridate::int_start(intervals[d2$start_time])
		d2
	})
	
	data <- reshape2::melt(data, id.vars=c("id", "start_time"), variable.name = "Type", value.name = "Energy")
	data
}