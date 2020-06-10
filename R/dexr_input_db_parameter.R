#' Retrieve market product information from PostgreSQL database.
#' 
#' @param dexp parameter object
#' @return data.frame of requests 
#' 
#' @author Sascha Holzhauer
#' @export
input_db_param_products <- function(dexpa) {
	
	futile.logger::flog.info("Retrieve market product pattern information from PostgreSQL database %s",
			dexpa$db$dbname,
			name = "dexr.input.db.param")
	
	con <- input_db_getconnection(dexpa)
	
	df_products <- DBI::dbGetQuery(con, "
					SELECT
						*
					FROM 
						market_product_pattern p,
						mmarket_product_pattern mp
					WHERE 
					p.product_id = mp.product_pattern_product_id;")
	
	futile.logger::flog.info("Obtained product information from database %s",
			dexpa$db$dbname,
			name = "dexr.input.db.param")
	df_products$first_delivery_period_start <- ms_to_date(df_products$first_delivery_period_start, timezone="Europe/Berlin")
	
	DBI::dbDisconnect(con)
	futile.logger::flog.info("Disconnected from database %s",
			dexpa$db$dbname,
			name = "dexr.input.db.param")
	return(df_products)
}
#' Retrieve timing information from PostgreSQL database.
#' 
#' @param dexp parameter object
#' @return data.frame of requests 
#' 
#' @author Sascha Holzhauer
#' @export
input_db_param_marketinfo <- function(dexp) {
	
	futile.logger::flog.info("Retrieve market information from PostgreSQL database %s",
			dexp$db$dbname,
			name = "dexr.input.db.param")
	
	con <- input_db_getconnection(dexp)
	
	df_products <- DBI::dbGetQuery(con, "
					SELECT
					*
					FROM 
						market_information");
	
	
	DBI::dbDisconnect(con)
	return(df_products)
}
#' Retrieve user parameters from PostgreSQL database.
#' 
#' @param dexp parameter object
#' @return data.frame of requests 
#' 
#' @author Sascha Holzhauer
#' @export
input_db_param_users <- function(dexp) {
	
	futile.logger::flog.info("Retrieve user information from PostgreSQL database %s",
			dexp$db$dbname,
			name = "dexr.input.db.param")
	
	con <- input_db_getconnection(dexp)
	
	df_users <- DBI::dbGetQuery(con, "
					SELECT
					*
					
					FROM 
					user_Account");
	
	DBI::dbDisconnect(con)
	return(df_users)
}
