#' Provides markdown formated table of product patterns.
#' @param dexp 
#' @return table of product patterns
#' 
#' @author Sascha Holzhauer
#' @export
output_table_param_products <- function(dexpas, format="markdown", caption="Product information", skipcolumns = c()) {
	# skipcolumns = c("max_price", "min_price")
	# format="csv"
	products <- data.frame()
	for (dexpa in dexpas) {
		products <- rbind(products, cbind("id" = dexpa$sim$id, input_db_param_products(dexpa)))
	}
	products$id = if(!is.null(dexpa$fig$labelsubs[products$id])) dexpa$fig$labelsubs[products$id] else products$id
	columns = c(
			"id"							= "Run ID",
			"description" 					= "Product",
			"first_delivery_period_start" 	= "Delivery start",
			"delivery_period_duration" 		= "Duration",
			"opening_time"					= "Opening",
			"auction_interval"				= "Auction interval",
			"closing_time"					= "Closing",
			"max_price"						= "Max price",
			"min_price"						= "Min price")
	columns = columns[!names(columns) %in% skipcolumns]
	if (format=="csv") {
		shbasic::sh.ensurePath(dexpa$dirs$output$tables)
		products <- products[, names(columns)]
		colnames(products) <- columns[colnames(products)]
		filename = paste(dexpa$dirs$output$tables, "/products_",  
		                 paste(dexpa$files$filenameprefix, lapply(dexpas, function(x) x$sim$id), collapse="__"),
		                 dexpa$files$filenamepostfix, ".csv", sep="")
		futile.logger::flog.info("Write product configuration to %s", filename, name="dexr.output.table.param.products")
		write.csv(products[, columns], file=filename, row.names=F)
	} else {
		futile.logger::flog.info("Output product configuration as markdown...", name="dexr.output.table.param.products")
		knitr::kable(products[, names(columns)], format=format, caption = caption, 
				col.names = c(columns))
	}
}
#' Provides markdown formated table of timing information.
#' @param dexp 
#' @return table of timing information
#' 
#' @author Sascha Holzhauer
#' @export
output_table_param_marketinfo <- function(dexpa, format="markdown", caption="Market information") {
	timing <- cbind(input_db_param_marketinfo(dexpa)[,c("uid")],
			input_csv_runinfos(dexpa)[,c("TF", "Basetime", "Offset", "Duration")],
			input_db_param_marketinfo(dexpa)[,-c(1)])

	#timing$Basetime <- paste(substring(timing$Basetime, 1,2), substring(timing$Basetime, 4), sep="")
	#timing$Basetime <- format(strptime(gsub("CEST ","",as.character(timing$Basetime)), tz="CEST", format = "%a %b %e %H:%M:%S %Y"), format = "%Y-%m-%d %H:%M:%S")
	timing$Basetime <- format(strptime(timing$Basetime, tz="CEST", format = "%F %T"), format = "%y-%m-%d %H:%M")
	timing$Offset <- format(as.POSIXct(as.numeric(timing$Offset)/1000, origin="1970-01-01"), format="%jd %H:%Mh")
	knitr::kable(timing, format=format,  row.names=F, caption=caption, 
			col.names = c(	"UID",
							"TF",
							"Basetime",
							"Offset",
							"Dur (h)",
							"F/untr kWh",
							"F/Miss read"))
}
#' Returns a kable markdown table with client information
#' @param dexpa 
#' @return kable markdown table
#' 
#' @author Sascha Holzhauer
#' @export
output_table_param_clients <- function(dexpa, format="markdown", caption="Client information", linespertable = 18,
		file=NULL, filextension="tex", prefix="") {
	# format="csv"
	data <- dexR::input_csv_clientdata(dexpa)
	
	data$annualConsumption = data$annualConsumption / 10^9
	data$ratedEnergy_upperLimit = data$ratedEnergy_upperLimit / 10^6
	data$profileType = substr(data$profileType,1,2)
	data$name_emg <- substr(data$name_emg, stringi::stri_length(data$name_emg[1])-2, stringi::stri_length(data$name_emg[1])-1)
	columns = c("name_emg"				= "Name",
				"nodes"					= "Nodes",
				"price_fluctuation"		= "PfL",
				"price_average"			= "PaL",
				"annualConsumption"		= "Cons (GJ)",
				"profileType"			= "Prof",
				"rotorArea"				= "RotorA",
				"panelArea"				= "PVA",
				"ratedEnergy_upperLimit"= "Storage",
				"priceOfferFluctuationGen" ="PfG", 
				"averagePriceOfferGen"  ="PaG"
		)
		
	if (all(is.na(data$nodes))) {
		columns["nodes"] <- NULL
	}
		
	if (format=="csv") {
		shbasic::sh.ensurePath(dexpa$dirs$output$tables)
		data <- data[, names(columns)]
		
		colnames(data) <- columns[colnames(data)]
		filename = paste(dexpa$dirs$output$tables, "/clients_",  
				paste(dexpa$files$filenameprefix, dexpa$sim$id, collapse="__"),
				dexpa$files$filenamepostfix, ".csv", sep="")
		
		futile.logger::flog.info("Write client configuration to %s", filename, name="dexr.output.table.param.clients")
		write.csv(data, file=paste(dexpa$dirs$output$tables, "/clients_",  
						dexpa$sim$id, ".csv", sep=""), row.names=F)
	} else {
		data$numnodes <- as.numeric(sapply(sapply(data$nodes, strsplit,";"), function(x) x[[1]]))
		data <- data[order(data$numnodes),]
		plyr::ddply(data, "numnodes", function(d) {
			cat(paste("\n####", paste("Nodes ", unique(d$nodes), collapse="/")))	
			for (i in seq(1,nrow(d), linespertable)) {
				# i=19
				optionKnitKableNa = getOption("knitr.kable.NA")
				options(knitr.kable.NA = '')
				tab <- knitr::kable(d[i:min(nrow(d),(i+linespertable-1)), names(columns)], format=format, 
						caption=caption, row.names=F, digits = 3,
						col.names = columns)
				options(knitr.kable.NA = optionKnitKableNa)
				
				if (!is.null(file)) {
					if (prefix != "") write(prefix, file = paste(file, "_", i, ".", filextension,sep=""))
					write(tab, file = paste(file, "_", i, ".", filextension,sep=""), append=prefix!="")	
				} else {
					print(tab)
				}
				if (i + linespertable <= nrow(d)) {
					cat("\n***")
				}
			}
		})
	}
}