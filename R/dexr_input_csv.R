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
				name = "dexr.hl.experiment")
	}
	
	clients <- read.csv(file=paste(sourcedir=paste(dexpa$dirs$config, dexpa$sim$id, sep=""), 
					paramConfigs[idMatch, "clients"], sep="/"))
	loads <- read.csv(file=paste(sourcedir=paste(dexpa$dirs$config, dexpa$sim$id, sep=""), 
					paramConfigs[idMatch,"loads"], sep="/"))
	
	loadProfiles <- read.csv(file=paste(sourcedir=paste(dexpa$dirs$config, dexpa$sim$id, sep=""), 
					paramConfigs[idMatch,"loadProfiles"], sep="/"))
	
	data <- merge(clients, loads, by.x="name_emg", by.y="client", all.y=T)
	data <- merge(data, loadProfiles, by.x="building", by.y="powerSensor", all.y=T)
	
	data <- data[, c("name_emg", "price_fluctuation", "price_average", "annualConsumption")]
	data
}