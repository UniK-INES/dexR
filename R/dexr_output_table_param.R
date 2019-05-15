#' Provides markdown formated table of product patterns.
#' @param dexp 
#' @return table of product patterns
#' 
#' @author Sascha Holzhauer
#' @export
output_table_param_products <- function(dexpa, format="markdown", caption="Product information") {
	products <- input_db_param_products(dexpa)
	knitr::kable(products[, c(	"description",
								"first_delivery_period_start",
								"delivery_period_duration",
								"opening_time",
								"auction_interval",
								"closing_time",
								"max_price",
								"min_price"
							)], format=format, caption = caption, 
				col.names = c(	"Product",
								"Delivery start",
								"Duration",
								"Opening",
								"Auction interval",
								"Closing",
								"Max price",
								"Min price"))
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
	data <- input_csv_clientdata(dexpa)
	
	data$annualConsumption = data$annualConsumption / 10^9
	data$ratedEnergy_upperLimit = data$ratedEnergy_upperLimit / 10^6
	data$profileType = substr(data$profileType,1,2)
	
	for (i in seq(1,nrow(data), linespertable)) {
		# i=19
		optionKnitKableNa = getOption("knitr.kable.NA")
		options(knitr.kable.NA = '')
		tab <- knitr::kable(data[i:min(nrow(data),(i+linespertable-1)),], format=format, 
				caption=caption, row.names=F, digits = 3,
				col.names = c(	"Name", 
						"P fluc",
						"P avg",
						"Cons (GJ)",
						"Prof",
						"RotorA",
						"PanelA",
						"Storage"))
		options(knitr.kable.NA = optionKnitKableNa)
		
		if (!is.null(file)) {
			if (prefix != "") write(prefix, file = paste(file, "_", i, ".", filextension,sep=""))
			write(tab, file = paste(file, "_", i, ".", filextension,sep=""), append=prefix!="")	
		} else {
			print(tab)
		}
		if (i + linespertable <= nrow(data)) {
			cat("\n***")
		}
	}
}