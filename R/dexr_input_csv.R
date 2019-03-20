#' Extract and merge client data from CSV parameter files
#' @param dexpa 
#' @return data.frame
#' 
#' @author Sascha Holzhauer
#' @export
input_csv_clientdata <- function(dexpa) {
	if (tools::file_ext(dexpa$files$paramconfigs)=="ods") {
		paramConfigs <- readODS::read_ods(dexpa$files$paramconfigs, sheet = 1, col_names = TRUE)
	} else {
		paramConfigs <- read.csv(dexpa$files$paramconfigs, header = TRUE, sep = ",", quote = "\"",
				dec = ".", fill = TRUE, comment.char = "")
	}
	idMatch <- match(dexpa$sim$id, paramConfigs$ID)
	if(is.na(idMatch)) {
		futile.logger::flog.warn("ID %s not present in config table (%s)!", dexpa$sim$id, dexpa$files$paramconfigs, 
				name = "dexr.input.csv.clientdata")
	}
	
	clients <- read.csv(file=paste(sourcedir=paste(dexpa$dirs$config, dexpa$sim$id, sep=""), 
					paramConfigs[idMatch, "clients"], sep="/"))
	loads <- read.csv(file=paste(sourcedir=paste(dexpa$dirs$config, dexpa$sim$id, sep=""), 
					paramConfigs[idMatch,"loads"], sep="/"))
	
	loadProfiles <- read.csv(file=paste(sourcedir=paste(dexpa$dirs$config, dexpa$sim$id, sep=""), 
					paramConfigs[idMatch,"loadProfiles"], sep="/"))
	
	generations <- read.csv(file=paste(sourcedir=paste(dexpa$dirs$config, dexpa$sim$id, sep=""), 
					paramConfigs[idMatch,"generations"], sep="/"))
	
	pvplants <- read.csv(file=paste(sourcedir=paste(dexpa$dirs$config, dexpa$sim$id, sep=""), 
					paramConfigs[idMatch,"pvplants"], sep="/"))
	
	windplants <- read.csv(file=paste(sourcedir=paste(dexpa$dirs$config, dexpa$sim$id, sep=""), 
					paramConfigs[idMatch,"windplants"], sep="/"))
	
	data <- merge(clients, loads, by.x="name_emg", by.y="client", all=T)
	data <- merge(data, loadProfiles, by.x="building", by.y="powerSensor", all=T)
	
	colnames(generations)[match( 
						c("name", "averagePrice", "priceFluctuation", "averagePriceOffer", "priceOfferFluctuation"), 
						colnames(generations))] <- c("nameGen", "averagePriceGen","priceFluctuationGen",
						"averagePriceOfferGen", "priceOfferFluctuationGen")
	
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
	
	data <- data[, c("name_emg", "price_fluctuation", "price_average", "annualConsumption", 
					"profileType", "rotorArea", "panelArea")]
	data
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