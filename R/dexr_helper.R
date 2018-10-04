#' Convert milliseconds to POSIXct
#' @param ms a numeric vector of milliseconds (big integers of 13 digits)
#' @param t0 a string of the format "yyyy-mm-dd", specifying the date that corresponds to 0 millisecond
#' @param timezone a string specifying a timezone that can be recognized by R
#' @return POSIXct vector representing calendar dates and times   
#' 
#' @author Sascha Holzhauer
#' @export
ms_to_date = function(ms, t0="1970-01-01", timezone="Europe/Berlin") {      
	sec = ms / 1000
	as.POSIXct(sec, origin=t0, tz=timezone)
}
#' Convert time string to milliseconds
#' 
#' @param timestring 
#' @param format 
#' @param timezone  
#' @return time in ms
#' 
#' @author Sascha Holzhauer
#' @export
str_to_ms = function(timestring, format = "%Y-%m-%d %H:%M", timezone="Europe/Berlin") {
	as.numeric(strptime(timestring,format = format, tz=timezone))*1000
}