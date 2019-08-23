#' Extract and merge client data from CSV parameter files
#' @param dexpa 
#' @return data.frame
#' 
#' @author Sascha Holzhauer
#' @export
input_csv_clientdata <- function(dexpa) {

	paramConfigs <- dexR::input_csv_configparam(dexpa, columns=NULL)
		
	clients <- checkandloadcsvfile(dexpa, paramConfigs["clients"])

	loads <- checkandloadcsvfile(dexpa, paramConfigs["loads"])
	
	loadProfiles <- checkandloadcsvfile(dexpa, paramConfigs["loadProfiles"])
	
	generations <- checkandloadcsvfile(dexpa, paramConfigs["generations"])
	
	pvplants <- checkandloadcsvfile(dexpa, paramConfigs["pvplants"])
	
	windplants <- checkandloadcsvfile(dexpa, paramConfigs["windplants"])
	
	storages <- checkandloadcsvfile(dexpa, paramConfigs["devicesStorage"])
	
	# connect storages with clients via RequestConfig:
	requestConfigs <- checkandloadcsvfile(dexpa, paramConfigs["requestConfig"])
	
	data <- merge(clients, loads, by.x="name_emg", by.y="client", all=T)
	colnames(data)[colnames(data)=="name.y"] <- "nameLoad"
	data <- merge(data, loadProfiles, by.x="building", by.y="powerSensor", all=T)
	
	colnames(generations)[match( 
						c("name", "averagePrice", "priceFluctuation", "averagePriceOffer", "priceOfferFluctuation"), 
						colnames(generations))] <- c("nameGen", "averagePriceGen","priceFluctuationGen",
						"averagePriceOfferGen", "priceOfferFluctuationGen")
	# TODO storage
	
	data <- merge(data, generations, by.x="name_emg", by.y="client", all=T)
	data <- merge(data, pvplants, by.x="device", by.y="name", all=T)
	colnames(data)[colnames(data)=="name"] <- "namePV"
	colnames(data)[colnames(data)=="efficiency"] <- "efficiencyPV"
	colnames(data)[colnames(data)=="simulationUpdateFrequency"] <- "simulationUpdateFrequencyPV"
	colnames(data)[colnames(data)=="simulationProvider"] <- "simulationProviderPV"
	colnames(data)[colnames(data)=="reading"] <- "readingPV"
	colnames(data)[colnames(data)=="simulationForecastUpdateFrequency"] <- "simulationForecastUpdateFrequencyPV"
	
	data <- merge(data, windplants, by.x="device", by.y="name", all=T)
	colnames(data)[colnames(data)=="name"] <- "nameWind"
	colnames(data)[colnames(data)=="efficiency"] <- "efficiencyWind"
	colnames(data)[colnames(data)=="simulationUpdateFrequency"] <- "simulationUpdateFrequencyWind"
	colnames(data)[colnames(data)=="simulationProvider"] <- "simulationProviderWind"
	colnames(data)[colnames(data)=="reading"] <- "readingWind"
	colnames(data)[colnames(data)=="simulationForecastUpdateFrequency"] <- "simulationForecastUpdateFrequencyWind"
	
	data <- merge(data, requestConfigs, by.x="name_emg", by.y="client", all=T)
	colnames(data)[colnames(data)=="name"] <- "requestConfig"
	
	data <- merge(data, storages, by.x="storage", by.y="name", all=T)
	colnames(data)[colnames(data)=="name"] <- "nameStorage"
	colnames(data)[colnames(data)=="reading"] <- "readingStorage"
	
	data <- data[, c("name_emg", "price_fluctuation", "price_average", "annualConsumption", 
					"profileType", "rotorArea", "panelArea", "ratedEnergy_upperLimit")]
	data
}
checkandloadcsvfile <- function(dexpa, configfilename) {
	filename = combine_sourcedirfile(paste(dexpa$dirs$config, dexpa$sim$id, sep="/"), configfilename)
	data <- read.csv(file=filename)
	if (ncol(data) == 1)
		futile.logger::flog.warn("Make sure to sure to use commata in %s!", filename, name = "dexr.input.csv.client")
	return(data)
}
#' Reads params for the given dexpa from the parameter configuration CSV file
#' 
#' @param dexpa 
#' @param columns  
#' @return vector of columns
#' 
#' @author Sascha Holzhauer
#' @export
input_csv_configparam <- function(dexpa, columns=NULL) {
	if (tools::file_ext(dexpa$files$paramconfigs)=="ods") {
		futile.logger::flog.info("Reading config ODS file %s", dexpa$files$paramconfigs, name = "dexr.hl.experiment")
		paramConfigs <- readODS::read_ods(dexpa$files$paramconfigs, sheet = 1)
	} else {
		futile.logger::flog.info("Reading config CSV file %s", dexpa$files$paramconfigs, name = "dexr.hl.experiment")
		paramConfigs <- read.csv(dexpa$files$paramconfigs, header = TRUE, sep = ",", quote = "\"",
				dec = ".", fill = TRUE, comment.char = "")
	}
	
	# Check Runs.csv for requested ID:
	idMatches <- which(paramConfigs$ID %in% dexpa$sim$id)
	if(length(idMatches)==0) {
		futile.logger::flog.warn("ID %s not present in config table (%s)!", dexpa$sim$id, dexpa$files$paramconfigs, 
				name = "dexr.hl.experiment")
	}
	paramConfigs[idMatches,]
}
#' Extract the runinfos entry from CSV file for the dexpa's sim ID
#' 
#' In case there are more than one entries, return the latest. In case there is none found for the given dexpa ID, the last row
#' is returned.
#' @param dexpa 
#' @return data.frame with one row
#' 
#' @author Sascha Holzhauer
#' @export
input_csv_runinfos <- function(dexpa) {
	runinfos <- read.csv(file = dexpa$files$runinfos, stringsAsFactors = F)
	rinfos <- runinfos[runinfos$ID == dexpa$sim$id,]
	if (nrow(rinfos)==0) {
		runinfos <- runinfos[nrow(runinfos),]
		futile.logger::flog.warn("ID %s not present in runinfos table (%s)!", dexpa$sim$id, dexpa$files$runinfos, 
				name = "dexr.input.csv.runinfos")
	} else {
		runinfos <- rinfos
	}
	runinfos[nrow(runinfos),]
}