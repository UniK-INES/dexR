#' Provides markdown formated table of product patterns.
#' @param dexp 
#' @return table of product patterns
#' 
#' @author Sascha Holzhauer
#' @export
output_table_param_products <- function(dexpa) {
	products <- input_db_param_products(dexpa)
	knitr::kable(products[, c(	"description",
								"first_delivery_period_start",
								"delivery_period_duration",
								"opening_time",
								"auction_interval",
								"closing_time",
								"max_price",
								"min_price"
							)], format="markdown", caption="Product information", 
				col.names = c(	"Product",
								"Delivery start",
								"Duration",
								"Opening",
								"Auction interval",
								"Closing",
								"Max price",
								"Min price"))
}
#' Provides markdown formated table of timing information.
#' @param dexp 
#' @return table of timing information
#' 
#' @author Sascha Holzhauer
#' @export
output_table_param_marketinfo <- function(dexpa) {
	timing <- input_db_param_marketinfo(dexpa)
	knitr::kable(timing, format="markdown", caption="Market information", 
			col.names = c(	"UID", 
							"Fine/untraded kWh",
							"Fine/Missing reading"))
}