#' Convert milliseconds to POSIXct
#' @param ms a numeric vector of milliseconds (big integers of 13 digits)
#' @param t0 a string of the format "yyyy-mm-dd", specifying the date that corresponds to 0 millisecond
#' @param timezone a string specifying a timezone that can be recognized by R
#' @return POSIXct vector representing calendar dates and times   
#' 
#' @author Sascha Holzhauer
#' @export
ms_to_date = function(ms, t0="1970-01-01", timezone) {      
	sec = ms / 1000
	as.POSIXct(sec, origin=t0, tz=timezone)
}