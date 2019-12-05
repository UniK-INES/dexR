#' Calculate gini data
#' 
#' @param dexpas 
#' @param data 
#' @param type either "load" or "gen"
#' @param normalise
#' @import data.table 
#' @return data
#' 
#' @author Sascha Holzhauer
prepare_costdata_gini <- function(dexpas, data, type, normalise=F) {
	shortestDelivery <- numbers::mGCD(unique(as.numeric(data$end_time) - as.numeric(data$start_time)))
	
	userdata <- list()
	for (dexpa in dexpas) {
		users <- dexR::input_csv_clientdata(dexpa)
		
		if (type == "load") {
			users <- users[
					(!is.na(users[,"annualConsumption"]) &
								(users[,"annualConsumption"] > 0)	|
								(!is.na(users[,"ratedEnergy_upperLimit"]) & 
									users[,"ratedEnergy_upperLimit"] > 0)),]
		} else { # type = "gen
			users <- users[
					# check which EMG ought to generate:
					(!is.na(users[,"annualConsumption"]) &
								(users[,"annualConsumption"] < 0)	|
								(!is.na(users[,"ratedEnergy_upperLimit"]) & 
									users[,"ratedEnergy_upperLimit"] > 0) |
								(!is.na(users[,"rotorArea"]) & 
									users[,"rotorArea"] > 0) |
								(!is.na(users[,"panelArea"]) & 
									users[,"panelArea"] > 0)
								),]
		}
		
		users <- unlist(mapply(function(a, b) {paste("n", strsplit(a, ";", fixed = T)[[1]], "_", b, sep="")}, 
						users$nodes, users$name_org))
		
		userdata = c(userdata, setNames(list(users), dexpa$sim$id))
	}
	
	data <- plyr::ddply(data, c("start_time", "id"), function(d) {
				# d <- data[data$id == unique(data$id)[2] & data$start_tim == data$start_time[10],]
				# d <- data[data$id == unique(data$id)[3] & data$start_tim == data$start_time[1],]
				# fill users with no request during start_time:
				# identify user with load and/or storage
				
				r <- merge(d, data.frame("users" = userdata[[d$rawid[1]]]), by.x='username',by.y='users',all.x=T,all.y=T)
				r$rawid <- d$rawid[1]
				r$id <- d$id[1]
				r$start_time = d$start_time[1]
				r$end_time = d$end_time[1]
				r
			})
	
	data[is.na(data$energy_accepted),"energy_accepted"] <- 0
	data[is.na(data$price_cleared),"price_cleared"] <- 0
	
	# convert according to delivery duration:
	data$energy_accepted <- data$energy_accepted * shortestDelivery/(as.numeric(data$end_time) - as.numeric(data$start_time))
	
	data <- data.table(data, key = c("start_time", "id", "username"))
	
	data <- data[, list(energy = sum(energy_accepted),
					costs = sum(price_cleared * energy_accepted),
					price = if (any(price_cleared > 0)) 
								mean(price_cleared) else 0,
					costPerEnergy = if (sum(energy_accepted) == 0) 0 else sum(price_cleared) / sum(energy_accepted)), by = key(data)]
	
	result <- data.table(data, key = c("start_time", "id"))
	result <- result[, list(
					"Traded energy" = sum(energy),
					"Costs" = sum(costs),
					"Price" = mean(price[price > 0]),
					"Gini" = ineq::Gini(costPerEnergy, corr=T)), by = key(result)]
	
	names(result)[names(result) == "Traded energy"] <- shbasic::shbasic_substitute(dexpa, "Traded energy")
	names(result)[names(result) == "Costs"] <- shbasic::shbasic_substitute(dexpa, "Costs")
	names(result)[names(result) == "Price"] <- shbasic::shbasic_substitute(dexpa, "Price")
	names(result)[names(result) == "Gini"] <- shbasic::shbasic_substitute(dexpa, "Gini")
	
	# Normalise:
	if(normalise) {
		result$Energy = (result$Energy - min(result$Energy)) / (max(result$Energy) - min(result$Energy))
		result$Costs = (result$Costs - min(result$Costs)) / (max(result$Costs) - min(result$Costs))
	}
	result <- reshape2::melt(result, id.vars=c("start_time", "id"), variable.name = "Type",
			value.name = "values")
	
	setDF(result)
}
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