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
combine_sourcedirfile <- function(sourcedir, sourcefile) {
	if (stringi::stri_startswith_fixed(sourcefile, "../")) {
		return(paste(dirname(sourcedir), substring(sourcefile,4),sep="/"))
	} else {
		return(paste(sourcedir, sourcefile, sep="/"))
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
	futile.logger::flog.info("Configure Market Backend products (%s)...", combine_sourcedirfile(sourcedir, sourcefile), 
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
	
	
	products <- read.csv(file=combine_sourcedirfile(sourcedir, sourcefile), stringsAsFactors=F)
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
hl_config_clients2db <- function(dexpa, sourcedir = paste(dexpa$dirs$config, dexpa$sim$id, sep="/"), paramConfigs) {
	
	allclients = data.frame()
	for (i in 1:nrow(paramConfigs)) {
		
		sourcefile = if(!is.na(paramConfigs[i, "clients"])) paramConfigs[i, "clients"] else 
					paste("DEX_Param_EnaviClient_", dexpa$sim$id, ".csv", sep="")
		
		futile.logger::flog.info("Configure Market Backend clients (%s)...", combine_sourcedirfile(sourcedir, sourcefile), 
				name = "dexr.hl.config.backend")
	
		rawclients <- read.csv(file=combine_sourcedirfile(sourcedir, sourcefile))
		
		futile.logger::flog.debug("Node-IDs in Param Config: %s", paramConfigs[i, "Nodes"], name="dexr.hl.config.clients")
		
		if (is.na(paramConfigs[i, "Nodes"])) paramConfigs[i, "Nodes"] <- 1
		for (nodeid in strsplit(paramConfigs[i, "Nodes"], ";")[[1]]) {
			clients = rawclients
			dexpa$sim$nodeid <- if(!is.na(as.numeric(nodeid))) as.numeric(nodeid) else 1
			clients$name <- adjust_client_id(dexpa, clients$name)
			clients$name_emg <- adjust_client_id(dexpa, clients$name_emg)
		
			clients$user_id = max(clients$user_id) * (dexpa$sim$nodeid - 1) + clients$user_id
			clients$id <- clients$user_id
		
			if(!("location" %in% colnames(clients))) {
				clients$location = "Tranformer01"
			}
			allclients = rbind(allclients, clients)
		}
	}
		
	con <- input_db_getconnection(dexpa)
	
	colnames_clients = DBI::dbGetQuery(con, paste(
					"select column_name from information_schema.columns where table_name= '", 
					dexpa$db$tablenames$clients,"';", sep=""))
	
	RPostgreSQL::dbWriteTable(con, dexpa$db$tablenames$clients, 
			value=allclients[,colnames_clients$column_name], append=T, row.names=F)
	# encrypt passwords:
	for (i in 1:nrow(allclients)) {
		DBI::dbGetQuery(con, paste(
			"UPDATE user_account SET password = crypt('", allclients[i, "password"],"',gen_salt('bf', 10)) WHERE id = '", 
			allclients[i, "id"],"';", sep=""))

		DBI::dbGetQuery(con, paste(
			"INSERT INTO oauth_client_details (client_id, client_secret, scope, authorized_grant_types,",
			" web_server_redirect_uri, authorities, access_token_validity, refresh_token_validity,",
			" additional_information, autoapprove) VALUES",
			"('", adjust_client_id(dexpa, allclients[i, "name"]), "', crypt('", allclients[i, "password"], "',gen_salt('bf', 10)), 'read,write',",
			"'password,authorization_code,refresh_token', null, null, 36000, 36000, null, true);", sep=""))
	}

	colnames_roles = DBI::dbGetQuery(con, paste(
					"select column_name from information_schema.columns where table_name= '", 
					dexpa$db$tablenames$roles,"';", sep=""))
	RPostgreSQL::dbWriteTable(con, dexpa$db$tablenames$roles, 
			value=allclients[,colnames_roles$column_name], append=T, row.names=F)
	DBI::dbDisconnect(con)
	
	return(nrow(allclients))
}
#' Deletes users from PostreDB (except admin, enavi, inspector)
#' 
#' @param dexpa 
#' @return 
#' 
#' @author Sascha Holzhauer
#' @export
hl_config_clearclients <- function(dexpa) {
	con <- input_db_getconnection(dexpa)
	DBI::dbGetQuery(con, "DELETE FROM users_roles WHERE user_id > 3; DELETE FROM user_account WHERE id > 3")
	DBI::dbDisconnect(con)
}
adjust_client_id <- function(dexpa, clientid) {
	if (dexpa$sim$multiplenodes) {
		clientids = paste("n", dexpa$sim$nodeid, "_", clientid, sep="")
	}
	else {
		clientids = clientid
	}
	return(clientids)
}
