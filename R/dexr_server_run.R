#' Start backend server
#' @param dexpa 
#' @return status code
#' 
#' @author Sascha Holzhauer
#' @export
server_start <- function(dexpa) {
	futile.logger::flog.info("Configure Market Backend clients...", name = "dexr.hl.config.backend")
	r <- httr::GET(paste(dexpa$server$url, ":", dexpa$server$port, dexpa$server$api$start,sep="/"),
			httr::authenticate(dexpa$server$username, dexpa$server$password, type = "basic"))
	return(if(httr::status_code(r)==200)"Starting Market Backend server succesful" else "Starting Market Backend server NOT successful")
}
#' Stop backend server
#' @param dexpa 
#' @return shutdown message
#' 
#' @author Sascha Holzhauer
#' @export
server_shutdown <- function(dexpa) {
	futile.logger::flog.info("Stopping Market Backend server...", name = "dexr.hl.experiment")
	try(httr::POST(paste(dexpa$server$url,":", dexpa$server$port, dexpa$server$api$shutdown,sep="/"),
			httr::authenticate(dexpa$server$username, dexpa$server$password, type = "basic")), silent=T)
	futile.logger::flog.info("Market Backend server stopped.", name = "dexr.hl.experiment")
}
#' Retrieve backend server status information
#' @param dexpa 
#' @return JSON info map
#' 
#' @author Sascha Holzhauer
#' @export
server_status <- function(dexpa) {
	req <- httr::GET(paste(dexpa$server$url,":", dexpa$server$port, dexpa$server$api$status,sep="/"),
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
	RCurl::url.exists(dexpa$server$url, ":", dexpa$server$port)
}
