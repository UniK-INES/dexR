#' Set reporting of figures to medium level
#' @param dexpa 
#' @return dexpa
#' 
#' @author Sascha Holzhauer
#' @export
reports_level_medium <- function(dexpa) {
	dexpa$fig$show$energy$summed_delivery <- F
	dexpa$fig$show$energy$residual_status_delivery <- F
	
	dexpa$fig$show$requests$product_submission <- F
	dexpa$fig$show$requests$product_delivery <- F
	dexpa$fig$show$requests$status_submission <- F
	dexpa$fig$show$requests$clients_delivery <- F
	return(dexpa)
}