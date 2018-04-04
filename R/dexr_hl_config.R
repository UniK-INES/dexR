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
#' @param dexpa
#' @targetdir only used to check existence of target directory
#' @return CSV files
#' 
#' @author Sascha Holzhauer
#' @export
hl_config_copycsvtemplates <- function(dexpa, targetdir=paste(dexpa$dirs$config, dexpa$sim$id,sep="/")) {
	shbasic::sh.ensurePath(targetdir)
	for (f in list.files(path = dexpa$dirs$csvtemplates, full.names=T)) {
		file.copy(from = f, to = gsub("TMPL", dexpa$sim$id,
						gsub("template/csv", paste("parameters",dexpa$sim$id, sep="/"), f)), overwrite = T)
	}
}
#' Stores market products from CSV file to PostGreSQL database.
#' The database tables are emptied before submission.
#' @param dexpa 
#' @param sourcedir 
#' @param sourcefile 
#' @return table in DB
#' 
#' @author Sascha Holzhauer
#' @export
hl_config_marketProducts2db <- function(dexpa, sourcedir=dexpa$dirs$config, 
		sourcefile=paste("DEX_Param_MarketProducts_", dexpa$sim$id, ".csv", sep="")) {
	futile.logger::flog.info("Configure Market Backend products...", name = "dexr.hl.config.backend")
	products <- read.csv(file=paste(sourcedir, dexpa$sim$id, sourcefile,sep="/"), stringsAsFactors=F)
	for (i in 1:nrow(products)) {
		products[i, "first_delivery_period_start"] <- as.numeric(lubridate::round_date(Sys.time(), 
						paste(products[i,"delivery_period_duration"]/1000," sec", sep="")))*1000	
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
hl_config_clients2db <- function(dexpa, sourcedir=dexpa$dirs$config, 
		sourcefile=paste("DEX_Param_EnaviClient_", dexpa$sim$id, ".csv", sep="")) {
	futile.logger::flog.info("Configure Market Backend clients...", name = "dexr.hl.config.backend")
	clients <- read.csv(file=paste(sourcedir, dexpa$sim$id, sourcefile, sep="/"))
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
}
