#' Export market products from DB to CSV file.
#' @param dexpa the new configuration (relevant for config folder to store CSV file in)
#' @param dexpa_template the template configuration (for the database)
#' @param targetdir (optional)
#' @return CSV file
#' 
#' @author Sascha Holzhauer
#' @export
hl_config_exportMarketProducts <- function(dexpa, dexpa_template, targetdir=paste(dexpa$dirs$config, dexpa$sim$id,sep="/")) {
	products <- input_db_param_products(dexpa_template)
	shbasic::sh.ensurePath(targetdir)
	write.csv(products, row.names=FALSE, file=paste(targetdir, "/DEX_Param_MarketProducts_", dexpa$sim$id, ".csv", sep=""))
}
#' Copy CSV template to new configuration folder
#' @param dexpa parameter list
#' @param targetdir only used to check existence of target directory
#' @return CSV files
#' 
#' @author Sascha Holzhauer
#' @export
hl_config_copycsvtemplates <- function(dexpa, targetdir=paste(dexpa$dirs$config, dexpa$sim$id,sep="/")) {
	shbasic::sh.ensurePath(targetdir)
	
	futile.logger::flog.info("Copy CSV template to %s...", targetdir, 
			name = "dexr.hl.config.backend")
	
	for (f in list.files(path = dexpa$dirs$csvtemplates, full.names=T)) {
		file.copy(from = f, to = paste(dexpa$dirs$config, dexpa$sim$id, gsub("TMPL", dexpa$sim$id,
						basename(f)), sep="/"), overwrite = T)
	}
}
#' Stores market products from CSV file to PostGreSQL database.
#' The database tables are emptied before submission.
#' 
#' @param dexpa
#' \itemize{
#' 	\item \code{dexpa$dirs$config}
#' 	\item \code{dexpa$sim$id}
#'  \item \code{dexpa$sim$firstdeliverystart$delay}
#'  \item \code{dexpa$db$tablenames$marketproducts}
#'  \item \code{dexpa$db$tablenames$mmarketproducts}
#' }
#' @param sourcedir 
#' @param sourcefile 
#' @param firstDeliveryPeriodStart as POSIX object
#' @return table in DB
#' 
#' @author Sascha Holzhauer
#' @seealso \code{\link[dexr]{input_db_getconnection}}
#' @seealso \code{\link[RPostgreSQL]{dbWriteTable}}
#' @export
hl_config_marketProducts2db <- function(dexpa, sourcedir=paste(dexpa$dirs$config,dexpa$sim$id, sep="/"), 
		sourcefile=paste("DEX_Param_MarketProducts_", dexpa$sim$id, ".csv", sep=""),
		firstDeliveryPeriodStart = Sys.time()) {
	futile.logger::flog.info("Configure Market Backend products (%s/%s)...", sourcedir, sourcefile, 
			name = "dexr.hl.config.backend")
	
	futile.logger::flog.debug("First delivery start is %s", 
			format(as.POSIXct(firstDeliveryPeriodStart, tz="GTM", origin = "1970-01-01"), "%d/%m/%y %H:%M:%S"),
			name = "dexr.hl.config.backend")
	
	# derive dexpa$sim$firstdeliverystart$delay
	dexpa$sim$firstdeliverystart$delay <- max(dexpa$sim$firstdeliverystart$delay,
			dexpa$emg$restarttime * dexpa$sim$timefactor)
	
	futile.logger::flog.debug("First delivery start delay is %d sec. (restart time: %d)", 
			dexpa$sim$firstdeliverystart$delay,
			dexpa$emg$restarttime,
			name = "dexr.hl.config.backend")
	
	
	products <- read.csv(file=paste(sourcedir, sourcefile,sep="/"), stringsAsFactors=F)
	for (i in 1:nrow(products)) {
		# lubridate does not deal with secs > 60 as expected (https://github.com/tidyverse/lubridate/issues/661)
		products[i, "first_delivery_period_start"] <- as.numeric(lubridate::ceiling_date(firstDeliveryPeriodStart + 
								dexpa$sim$firstdeliverystart$delay, 
						paste(products[i,"delivery_period_duration"]/60000," mins", sep="")))*1000	
	}
	products[,"first_delivery_period_start"] <- as.numeric(products[,"first_delivery_period_start"])
	
	con <- input_db_getconnection(dexpa)
	
	colnames_market_product_pattern = DBI::dbGetQuery(con, paste(
			"select column_name from information_schema.columns where table_name= '", 
			dexpa$db$tablenames$marketproducts,"';", sep=""))
	DBI::dbGetQuery(con, paste("TRUNCATE", dexpa$db$tablenames$marketproducts, "CASCADE;"))
	RPostgreSQL::dbWriteTable(con, dexpa$db$tablenames$marketproducts, 
			value=products[,colnames_market_product_pattern$column_name], append=T, row.names=F)

	colnames_mmarket_product_pattern = DBI::dbGetQuery(con, paste(
			"select column_name from information_schema.columns where table_name= '", 
			dexpa$db$tablenames$mmarketproducts,"';", sep=""))
	DBI::dbGetQuery(con, paste("TRUNCATE", dexpa$db$tablenames$mmarketproducts, "CASCADE;"))
	RPostgreSQL::dbWriteTable(con, dexpa$db$tablenames$mmarketproducts, 
			value=products[,colnames_mmarket_product_pattern$column_name], append=T, row.names=F)
	DBI::dbDisconnect(con)
}
#' Stores client and role data from CSV file to PostGreSQL database.
#' The database tables are _not_ emptied before submission.
#' @param dexpa 
#' @param sourcedir 
#' @param sourcefile 
#' @return table in DB
#' 
#' @author Sascha Holzhauer
#' @export
hl_config_clients2db <- function(dexpa,sourcedir = paste(dexpa$dirs$config, dexpa$sim$id, sep="/"), 
		sourcefile=paste("DEX_Param_EnaviClient_", dexpa$sim$id, ".csv", sep="")) {
	
	
	futile.logger::flog.info("Configure Market Backend clients (%s/%s)...", sourcedir, sourcefile, 
			name = "dexr.hl.config.backend")
	clients <- read.csv(file=paste(sourcedir, sourcefile, sep="/"))
	clients$id <- clients$user_id
	
	con <- input_db_getconnection(dexpa)
	
	colnames_clients = DBI::dbGetQuery(con, paste(
					"select column_name from information_schema.columns where table_name= '", 
					dexpa$db$tablenames$clients,"';", sep=""))
	
	RPostgreSQL::dbWriteTable(con, dexpa$db$tablenames$clients, 
			value=clients[,colnames_clients$column_name], append=T, row.names=F)
	# encrypt passwords:
	for (i in 1:nrow(clients)) {
		DBI::dbGetQuery(con, paste(
			"UPDATE user_account SET password = crypt('", clients[i, "password"],"',gen_salt('bf', 10)) WHERE id = '", 
					clients[i, "id"],"';", sep=""))

		DBI::dbGetQuery(con, paste(
			"INSERT INTO oauth_client_details (client_id, client_secret, scope, authorized_grant_types,",
			" web_server_redirect_uri, authorities, access_token_validity, refresh_token_validity,",
			" additional_information, autoapprove) VALUES",
			"('", clients[i, "name"], "', crypt('", clients[i, "password"], "',gen_salt('bf', 10)), 'read,write',",
			"'password,authorization_code,refresh_token', null, null, 36000, 36000, null, true);", sep=""))
	}

	colnames_roles = DBI::dbGetQuery(con, paste(
					"select column_name from information_schema.columns where table_name= '", 
					dexpa$db$tablenames$roles,"';", sep=""))
	RPostgreSQL::dbWriteTable(con, dexpa$db$tablenames$roles, 
			value=clients[,colnames_roles$column_name], append=T, row.names=F)
	DBI::dbDisconnect(con)
	
	return(nrow(clients))
}
