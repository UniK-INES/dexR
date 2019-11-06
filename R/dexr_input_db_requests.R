#' Retrieve request data from PostgreSQL database.
#' 
#' @param dexp parameter object
#' @return data.frame of requests 
#' 
#' @author Sascha Holzhauer
#' @export
input_db_requests <- function(dexpa, additionalwhere="TRUE") {
	
	futile.logger::flog.info("Retrieve request data from PostgreSQL database %s",
			dexpa$db$dbname,
			name = "dexr.input.db.requests")
	
	con <- input_db_getconnection(dexpa)
	
	df_requests <- DBI::dbGetQuery(con, paste("
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
						e.status 			AS status,
						mp.description 		AS product_id,
						mp.clearing_id
					FROM 
						market_energy_request m, 
						energy_request e,
						user_account u,
						market_product_pattern p,
						mmarket_product_pattern mp 
					WHERE 
						m.request_uid = e.uid AND
						e.product_id = p.product_id AND
						p.product_id = mp.product_pattern_product_id AND
						m.user_account_id = u.id AND
						e.start_time >= ", dexpa$sim$starttime_min, " AND
						e.start_time <= ", dexpa$sim$starttime_max, " AND ",
						additionalwhere,";"), sep="")
		
	df_requests$submission_time <- ms_to_date(df_requests$submission_time, timezone="Europe/Berlin")
	df_requests$start_time <- ms_to_date(df_requests$start_time, timezone="Europe/Berlin")
	df_requests$end_time <- ms_to_date(df_requests$end_time, timezone="Europe/Berlin")
		
	DBI::dbDisconnect(con)
	return(df_requests)
}
#' Retrieve energy requests to illustrate a single clearing process
#' @param dexpa 
#' @param starttime time of clearing to be illustrated
#' @return data.frame of energy requests
#' 
#' @author Sascha Holzhauer
#' @export
input_db_requests_clearing <- function(dexpa, starttime=NULL){
	
	con <- input_db_getconnection(dexpa)
	
	futile.logger::flog.info("Retrieve clearing-specific request data from PostgreSQL database %s",
			dexpa$db$dbname,
			name = "dexr.input.db.requests")
	
	stmt = paste(
			"select
					er.uid,
					mer.user_account_id,
					er.cid,
					er.product_id,
					er.energy_requested,
					er.price_requested,
					to_timestamp(mer.submission_time/1000) submission_time,
					to_timestamp(er.start_time/1000) start_time,
					to_timestamp(er.end_time/1000) end_time
					from market_energy_request mer, energy_request er
					where mer.request_uid=er.uid AND",
			if (!is.null(starttime)) paste("and er.start_time=", starttime, " ", sep="") else "",
					"e.start_time >= ", dexpa$sim$starttime_min, " AND
					e.start_time <= ", dexpa$sim$starttime_max,
					"order by start_time;"
					,sep="");
	
	rs<-dbSendQuery(con,stmt)
	readTab<-fetch(rs,n=-1)
	
	market_df<-readTab
	market_df$start_time<-lubridate::floor_date(market_df$start_time, "5 minutes")
	market_df$end_time<-lubridate::floor_date(market_df$end_time, "5 minutes")
	return(market_df)
}
