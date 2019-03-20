#' Retrieve clearing data from PostgreSQL database.
#' 
#' @param dexpa parameter list
#' @return data.frame of clearing infos 
#' 
#' @author Sascha Holzhauer
#' @export
input_db_clearings <- function(dexpa) {
	
	futile.logger::flog.info("Retrieve clearing data from PostgreSQL database %s",
			dexpa$db$dbname,
			name = "dexr.input.db.clearing")
	
	con <- input_db_getconnection(dexpa)
	
	df_cinfos <- DBI::dbGetQuery(con, paste("
					SELECT
						id,
						clearing_time,
						delivery_period_start,
						energy_cleared,
						num_considered_requests,
						price_cleared,
						mmarket_product_pattern.description AS product_id
					FROM 
						clearing_info,
						mmarket_product_pattern
					WHERE
						clearing_info.product_pattern_product_id = mmarket_product_pattern.product_pattern_product_id AND
						clearing_info.delivery_period_start >= ", dexpa$sim$starttime_min, " AND
						clearing_info.delivery_period_start <= ", dexpa$sim$starttime_max, ";", sep=""))
	DBI::dbDisconnect(con)
	
	df_cinfos$delivery_period_start <- ms_to_date(df_cinfos$delivery_period_start, timezone="Europe/Berlin")
	df_cinfos$clearing_time <- ms_to_date(df_cinfos$clearing_time, timezone="Europe/Berlin")
	
	return(df_cinfos)
}
