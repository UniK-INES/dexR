#' Retrieve a connection to the PostgreSQL DB specified in passed dexpa
#' @param dexpa 
#' @return database connection
#' 
#' @author Sascha Holzhauer
#' @export
input_db_getconnection <- function(dexpa) {
	
	futile.logger::flog.debug("Retrieve connection from PostgreSQL database %s",
			dexpa$db$dbname,
			name = "dexpa.input.db")
	
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
input_db_dump3db <- function(dexpa, dumpfile) {
	# Superuser required as long as other user does not have rights for new database:
	Sys.setenv("PGPASSWORD"=dp2$db$supassword)
	
	# Done by pg_restore:
	# system(paste("createdb -T template0 -h ", dp2$db$host, " -p ", dp2$db$port, " --username enavi --no-password ", dp2$db$dbname, sep=" "))
	
	system(paste("pg_restore",  "-h", dp2$db$host, "-p", dp2$db$port, "--username ", dp2$db$suname, 
					"--no-password --clean -d", dp2$db$dbname, dumpfile))
}