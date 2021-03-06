#' Retrieve a connection to the PostgreSQL DB specified in passed dexpa
#' @param dexpa 
#' @return database connection
#' 
#' @author Sascha Holzhauer
#' @export
input_db_getconnection <- function(dexpa) {
	
	futile.logger::flog.debug("Retrieve connection from PostgreSQL database %s for user %s at host %s",
			dexpa$db$dbname,
			dexpa$db$username,
			dexpa$db$host,
			name = "dexr.input.db")
	
	drv <- DBI::dbDriver("PostgreSQL")
	con <- DBI::dbConnect(drv, dbname = dexpa$db$dbname,
			host = dexpa$db$host, port = dexpa$db$port,
			user = dexpa$db$username, password = dexpa$db$password)
	return(con)
}
#' Creates the database configured in the given parameter object
#' @param dexpa parameter object
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
input_db_createdb <- function(dexpa) {
	futile.logger::flog.info("Create database %s..." ,
			dexpa$db$dbname,
			name = "dexr.input.db.create")
	# Superuser required as long as other user does not have rights for new database:
	Sys.setenv("PGPASSWORD"=dexpa$db$supassword)
	
	system(paste("createdb -T", dexpa$db$dbname_template, "-h", dexpa$db$host, "-p", dexpa$db$port, "--username", dexpa$db$suname, "--no-password", dexpa$db$dbname, sep=" "))
	
	# set privileges:
	dp2 <- dexpa
	dp2$db$username = dexpa$db$suname
	dp2$db$password = dexpa$db$supassword
	con <- dexR::input_db_getconnection(dp2)
	DBI::dbGetQuery(con, paste("GRANT ALL ON SCHEMA public TO ", dexpa$db$username, ";", sep=""))
	DBI::dbGetQuery(con, "CREATE extension pgcrypto")
	DBI::dbDisconnect(con)
	
	futile.logger::flog.info("Creation of DB completed." ,
			name = "dexr.input.db.create")
}
#' Imports the given dumpfile into the database configured in the given parameter object
#' @param dexpa parameter object
#' <ul><li>dexpa$db$dbname</li>
#'     <li>dexpa$db$suname</li>
#' 	   <li>dexpa$db$supassword</li>
#' 	   <li>dexpa$db$dbname_template</li>
#'     <li>dexpa$db$host</li>
#'     <li>dexpa$db$port</li>	  
#'     <li>dexpa$dirs$output$dbdumps</li>
#' 	   <li>dexpa$db$username</li>
#' </ul>
#' 
#' @param dumpfile dumpfile to import
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
input_db_dump2db <- function(dexpa, dumpfile =  paste("dump_", dexpa$sim$id, sep="")) {
	futile.logger::flog.info("Import dump %s to database %s, using template %s and user %s (password: %s)..." ,
			dumpfile,
			dexpa$db$dbname,
			dexpa$db$dbname_template,
			dexpa$db$suname,
			dexpa$db$supassword,
			name = "dexr.input.db.dump")
	# Superuser required as long as other user does not have rights for new database:
	Sys.setenv("PGPASSWORD"=dexpa$db$supassword)
	
	command = paste("createdb -T", dexpa$db$dbname_template, "-h", dexpa$db$host, "-p", dexpa$db$port, 
			"--username", dexpa$db$suname, "--no-password", dexpa$db$dbname, sep=" ")
	futile.logger::flog.debug("Command: %s", command, name = "dexr.input.db.dump")
	system(command)
	
	command = paste("pg_restore",  "-h", dexpa$db$host, "-p", dexpa$db$port, "--username", dexpa$db$suname, 
			"--no-password --clean -d", dexpa$db$dbname, paste(dexpa$dirs$output$dbdumps,dumpfile,sep="/"))
	futile.logger::flog.debug("Command: %s", command, name = "dexr.input.db.dump")
	system(command)
	
	# set privileges:
	dp2 <- dexpa
	dp2$db$username = dexpa$db$suname
	dp2$db$password = dexpa$db$supassword
	con <- dexR::input_db_getconnection(dp2)
	DBI::dbGetQuery(con, paste("GRANT ALL ON SCHEMA public TO ", dexpa$db$username, ";", sep=""))
	DBI::dbDisconnect(con)
	
	futile.logger::flog.info("Importing dump completed" ,
			name = "dexr.input.db.dump")
}
#' Import dumpfiles which are defined in a list of dexpas
#' @param dexpas list of parameter lists
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
input_db_dumps2db <- function(dexpas) {
	for (dp in dexpas) {
		dexR::input_db_dump2db(dp, dumpfile = paste("dump_", dp$sim$id, sep=""))
	}	
}
#' Exports the database configured in the given parameter object to the given dumpfile
#' @param dexpa parameter object
#' @param dumpdir directory to export to (\code{dexpa$dirs$output$dbdumps} is prefixed)
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
input_db_db2dump <- function(dexpa, dumpdir, remoteServer=FALSE, outputfile="") {
		
	if (remoteServer) {
		futile.logger::flog.info("Dump database %s at localhost:%s with username %s to dumpdir %s (remote with ssh-config %s)..." ,
				dexpa$db$dbname,
				dexpa$db$port,
				dexpa$db$suname,
				paste(dexpa$dirs$output$dbdumpsremote, dumpdir, sep="/"),
				dexpa$db$sshname,
				name = "dexr.input.db.dump")
		
		system2("ssh", args=paste(dexpa$db$sshname, if (dexpa$db$sshverbose) "-v", " 'pg_dump",  "-h localhost", "-p", dexpa$db$port, "--username", dexpa$db$suname, 
					"--no-password --format directory --blobs --file", paste(dexpa$dirs$output$dbdumpsremote, dumpdir, sep="/"), dexpa$db$dbname, "'"),
				stdout=outputfile, stderr=outputfile)
	} else {
		futile.logger::flog.info("Dump database %s to dumpdir %s..." ,
				dexpa$db$dbname,
				paste(dexpa$dirs$output$dbdumps,dumpdir,sep="/"),
				name = "dexr.input.db.dump")
		
		shbasic::sh.ensurePath(dexpa$dirs$output$dbdumps)

		if (file.exists(paste(dexpa$dirs$output$dbdumps,dumpdir,sep="/"))) {
			unlink(paste(dexpa$dirs$output$dbdumps,dumpdir,sep="/"), recursive=TRUE)
			futile.logger::flog.warn("Removed existing directory %s/%s." ,
					dexpa$dirs$output$dbdumps,
					dumpdir,
					name = "dexr.input.db.dump")
		}
		
		# Superuser required as long as other user does not have rights for new database:
		Sys.setenv("PGPASSWORD"=dexpa$db$supassword)
		
		system2("pg_dump", args= paste("-h", dexpa$db$host, "-p", dexpa$db$port, "--username", dexpa$db$suname, 
					"--no-password --format directory --blobs --file",  paste(dexpa$dirs$output$dbdumps,dumpdir,sep="/"), dexpa$db$dbname),
			stdout=outputfile, stderr=outputfile)
	}
	
	futile.logger::flog.info("Dumping database %s finished." ,
			dexpa$db$dbname,
			dumpdir,
			name = "dexr.input.db.dump")
}
#' Drop database that is defined in dexpa
#' @param dexpa 
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
input_db_dropdb <- function(dexpa) {
	futile.logger::flog.info("Drop database %s..." ,
			dexpa$db$dbname,
			name = "dexr.input.db.drop")
	
	Sys.setenv("PGPASSWORD"=dexpa$db$supassword)
	system(paste("dropdb", "-h", dexpa$db$host, "-p", dexpa$db$port, "--username", dexpa$db$suname, "--no-password", dexpa$db$dbname, sep=" "))
	
	futile.logger::flog.info("Database %s dropped." ,
			dexpa$db$dbname,
			name = "dexr.input.db.drop")
}
#' Drop databases which are defined in a list of dexpas
#' @param dexpas list of parameter lists
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
input_db_dropdbs <- function(dexpas) {
	for (dp in dexpas) {
		dexR::input_db_dropdb(dp)
	}	
}
#' Generate run ID string of the given database configuration.
#' @param dexpa 
#' @return run ID string
#' 
#' @author Sascha Holzhauer
#' @export
input_db_runID <- function(dexpa) {
	if (!is.null(dexpa$fig$labelsubs)) {
		return(dexpa$fig$labelsubs[dexpa$sim$id])	
	} else {
		
		num_clients <- if (dexpa$cache$usecache) R.cache::loadCache(key=list("num_clients", dexpa$sim$id)) else NULL
		minfo <- if (dexpa$cache$usecache) R.cache::loadCache(key=list("minfo", dexpa$sim$id)) else NULL
		
		if (is.null(num_clients) || is.null(minfo)) {
			
			futile.logger::flog.info("Retrieve run ID from PostgreSQL database %s",
					dexpa$db$dbname,
					name = "dexr.input.db")
			
			
			con <- input_db_getconnection(dexpa)
			
			num_clients <- DBI::dbGetQuery(con, "
							SELECT
							COUNT(id) - 3 as num_clients
							FROM 
							user_account")
			R.cache::saveCache(num_clients, key=list("num_clients", dexpa$sim$id), comment="input_db_runID()", dirs = dexpa$cache$subdir)
				
			minfo <- DBI::dbGetQuery(con, "
							SELECT
								fine_per_untraded_kwh
							FROM 
								market_information")
				
			R.cache::saveCache(minfo, key=list("minfo", dexpa$sim$id), comment="input_db_runID()", dirs = dexpa$cache$subdir)
				
			DBI::dbDisconnect(con)
	
		}
		products <- input_db_param_products(dexpa)
		
		return(paste(dexpa$sim$id, "_", num_clients$num_clients, "C", nrow(products), "P", minfo$fine_per_untraded_kwh, "F", "_",
			paste(products$delivery_period_duration/(1000*60), "d", products$auction_interval, "a", products$clearingID, sep="", collapse="_"), sep=""))
	}
}
