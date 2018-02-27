#' Retrieve request data from PostgreSQL database.
#' 
#' @param dexp parameter object
#' @return data.frame of requests 
#' 
#' @author Sascha Holzhauer
#' @export
input_db_requests <- function(dexp) {
	
	futile.logger::flog.info("Retrieve request data from PostgreSQL database %s",
			dexp$db$dbname,
			name = "dexp.input.db.requests")
	
	con <- input_db_getconnection(dexp)
	
	df_requests <- DBI::dbGetQuery(con, "
					SELECT
						m.submission_time,
						m.id,
						u.name 				AS username,
						e.cid,
						e.start_time,
						e.end_time,
						e.energy_requested,
						e.energy_accepted,
						e.price_requested,
						e.price_cleared,
						rn.name 			AS status,
						mp.description 		AS product_id,
						mp.clearing_id
					FROM 
						market_energy_request m, 
						energy_request e,
						user_account u,
						market_product_pattern p,
						mmarket_product_pattern mp,
						requests_status_names rn 
					WHERE 
						m.request_uid = e.uid AND
						e.product_id = p.product_id AND
						p.product_id = mp.product_pattern_product_id AND
						m.user_account_id = u.id AND
						rn.id = e.status;")
		
	df_requests$submission_time <- ms_to_date(df_requests$submission_time, timezone="Europe/Berlin")
	df_requests$start_time <- ms_to_date(df_requests$start_time, timezone="Europe/Berlin")
	df_requests$end_time <- ms_to_date(df_requests$end_time, timezone="Europe/Berlin")
		
	DBI::dbDisconnect(con)
	return(df_requests)
}
