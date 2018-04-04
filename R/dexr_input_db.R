#' Retrieve a connection to the PostgreSQL DB specified in passed dexpa
#' @param dexpa 
#' @return database connection
#' 
#' @author Sascha Holzhauer
#' @export
input_db_getconnection <- function(dexpa) {
	
	futile.logger::flog.debug("Retrieve connection from PostgreSQL database %s",
			dexpa$db$dbname,
			name = "dexr.input.db")
	
	drv <- DBI::dbDriver("PostgreSQL")
	con <- DBI::dbConnect(drv, dbname = dexpa$db$dbname,
			host = dexpa$db$host, port = dexpa$db$port,
			user = dexpa$db$username, password = dexpa$db$password)
	return(con)
}
#' Imports the given dumpfile into the database configured in the given parameter object
#' @param dexpa parameter object
#' @param dumpfile dumpfile to import
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
input_db_dump2db <- function(dexpa, dumpfile) {
	# Superuser required as long as other user does not have rights for new database:
	Sys.setenv("PGPASSWORD"=dexpa$db$supassword)
	
	system(paste("createdb -T", dexpa$db$dbname_template, "-h", dexpa$db$host, "-p", dexpa$db$port, "--username", dexpa$db$suname, "--no-password", dexpa$db$dbname, sep=" "))
	
	system(paste("pg_restore",  "-h", dexpa$db$host, "-p", dexpa$db$port, "--username", dexpa$db$suname, 
					"--no-password --clean --if-exists -d", dexpa$db$dbname, paste(dexpa$dirs$output$dbdumps,dumpfile,sep="/")))
	
	# set privileges:
	dp2 <- dexpa
	dp2$db$username = dexpa$db$suname
	dp2$db$password = dexpa$db$supassword
	con <- dexR::input_db_getconnection(dp2)
	DBI::dbGetQuery(con, paste("GRANT ALL ON SCHEMA public TO ", dexpa$db$username, ";", sep=""))
	DBI::dbDisconnect(con)
}

#' Exports the database configured in the given parameter object to the given dumpfile
#' @param dexpa parameter object
#' @param dumpdir directory to export to (\code{dexpa$dirs$output$dbdumps} is prefixed)
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
input_db_db2dump <- function(dexpa, dumpdir) {
	# Superuser required as long as other user does not have rights for new database:
	Sys.setenv("PGPASSWORD"=dexpa$db$supassword)
	
	system(paste("pg_dump",  "-h", dexpa$db$host, "-p", dexpa$db$port, "--username", dexpa$db$suname, 
					"--no-password --format directory --blobs --file",  paste(dexpa$dirs$output$dbdumps,dumpdir,sep="/"), dexpa$db$dbname))	
}
#' Drop database that is defined in dexpa
#' @param dexpa 
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
input_db_dropdb <- function(dexpa) {
	con <- input_db_getconnection(dexpa)
	DBI::dbGetQuery(con, paste("DROP DATABASE", dexpa$db$dbname))
	DBI::dbDisconnect(con)	
}
#' Genrate run ID string of the given database configuration.
#' @param dexpa 
#' @return run ID string
#' 
#' @author Sascha Holzhauer
#' @export
input_db_runID <- function(dexpa) {
	futile.logger::flog.info("Retrieve run ID from PostgreSQL database %s",
			dexpa$db$dbname,
			name = "dexr.input.db")
	
	con <- input_db_getconnection(dexpa)
	
	num_clients <- DBI::dbGetQuery(con, "
					SELECT
					COUNT(id) - 3 as num_clients
					FROM 
					user_account")
	products <- input_db_param_products(dexpa)
		
	minfo <- DBI::dbGetQuery(con, "
					SELECT
						fine_per_untraded_kwh
					FROM 
						market_information")
	
	DBI::dbDisconnect(con)
	
	return(paste(dexpa$sim$id, "_", num_clients$num_clients, "C", nrow(products), "P", minfo$fine_per_untraded_kwh, "F", "_",
			paste(products$delivery_period_duration/(1000*60), "d", products$auction_interval, "a", products$clearingID, sep="", collapse="_"), sep=""))
}
