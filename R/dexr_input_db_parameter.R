#' Retrieve market product information from PostgreSQL database.
#' 
#' @param dexp parameter object
#' @return data.frame of requests 
#' 
#' @author Sascha Holzhauer
#' @export
input_db_param_products <- function(dexp) {
	
	futile.logger::flog.info("Retrieve market product pattern information from PostgreSQL database %s",
			dexp$db$dbname,
			name = "dexp.input.db.param")
	
	con <- input_db_getconnection(dexp)
	
	df_products <- DBI::dbGetQuery(con, "
					SELECT
						*
						
					FROM 
						market_product_pattern p,
						mmarket_product_pattern mp
					WHERE 
					p.product_id = mp.product_pattern_product_id;")
	
	df_products$first_delivery_period_start <- ms_to_date(df_products$first_delivery_period_start, timezone="Europe/Berlin")
	
	DBI::dbDisconnect(con)
	return(df_products)
}
#' Retrieve timing information from PostgreSQL database.
#' 
#' @param dexp parameter object
#' @return data.frame of requests 
#' 
#' @author Sascha Holzhauer
#' @export
input_db_param_timing <- function(dexp) {
	
	futile.logger::flog.info("Retrieve timing information from PostgreSQL database %s",
			dexp$db$dbname,
			name = "dexp.input.db.param")
	
	con <- input_db_getconnection(dexp)
	
	df_products <- DBI::dbGetQuery(con, "
					SELECT
					*
					
					FROM 
						time_information");
	
	
	DBI::dbDisconnect(con)
	return(df_products)
}
#' Retrieve timing information from PostgreSQL database.
#' 
#' @param dexp parameter object
#' @return data.frame of requests 
#' 
#' @author Sascha Holzhauer
#' @export
input_db_param_users <- function(dexp) {
	
	futile.logger::flog.info("Retrieve user information from PostgreSQL database %s",
			dexp$db$dbname,
			name = "dexp.input.db.param")
	
	con <- input_db_getconnection(dexp)
	
	df_users <- DBI::dbGetQuery(con, "
					SELECT
					*
					
					FROM 
					user_Account");
	
	DBI::dbDisconnect(con)
	return(df_users)
}
