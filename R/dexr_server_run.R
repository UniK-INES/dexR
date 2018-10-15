#' Start backend server
#' @param dexpa 
#' @return status code
#' 
#' @author Sascha Holzhauer
#' @export
server_start <- function(dexpa) {
	futile.logger::flog.info("Starting Market Backend server (%s:%s)...", dexpa$server$url, dexpa$server$port,
			name = "dexr.server.run.start")
	r <- httr::GET(paste(dexpa$server$url, ":", dexpa$server$port, "/", dexpa$server$api$start,sep=""),
			httr::authenticate(dexpa$server$username, dexpa$server$password, type = "basic"))
	return(if(httr::status_code(r)==200)"Starting Market Backend server successful" else "Starting Market Backend server NOT successful")
}
#' Stop backend server
#' @param dexpa 
#' @return shutdown message
#' 
#' @author Sascha Holzhauer
#' @export
server_shutdown <- function(dexpa) {
	futile.logger::flog.info("Stopping Market Backend server...", name = "dexr.server.run.shutdown")
	try(httr::POST(paste(dexpa$server$url,":", dexpa$server$port, "/", dexpa$server$api$shutdown,sep=""),
			httr::authenticate(dexpa$server$username, dexpa$server$password, type = "basic")), silent=T)
	futile.logger::flog.info("Market Backend server stopped.", name = "dexr.server.run.shutdown")
}
#' Retrieve backend server status information
#' @param dexpa 
#' @return JSON info map
#' 
#' @author Sascha Holzhauer
#' @export
server_status <- function(dexpa) {
	futile.logger::flog.debug("Request server (%s:%s) status...", dexpa$server$url, dexpa$server$port,
			name = "dexr.server.run.status")
	req <- httr::GET(paste(dexpa$server$url,":", dexpa$server$port, "/", dexpa$server$api$status,sep=""),
			httr::authenticate(dexpa$server$username, dexpa$server$password, type = "basic"))
	httr::content(req, as = "parsed")
}
#' Checks whether the server is ready for requests.
#' @param dexpa 
#' @return TRUE if the server is up
#' 
#' @author Sascha Holzhauer
#' @export
server_isrunning <- function(dexpa) {
	futile.logger::flog.debug("Check server (%s:%s) running...", dexpa$server$url, dexpa$server$port,
			name = "dexr.server.run.status")
	RCurl::url.exists(paste(dexpa$server$url, ":", dexpa$server$port, sep=""))
}
