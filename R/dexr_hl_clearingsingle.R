#' Gnerates a figure of clearing at starttime
#' @param dexpa 
#' @param starttime
#' @return figure in file
#' 
#' @author Sascha Holzhauer
#' @export
hl_clearingsingle <- function(dexpa, starttime, title = NULL, filename = "GetIntersectionPrice", dexpa_comp = NULL) {
	
	if (is.null(starttime)) {
		R.oo::throw.default("Parameter starttime may not be NULL!")
	}
	
	market_df=input_db_requests_clearing(dexpa, starttime=starttime)
	start_times<-unique(market_df$start_time)
	
	the_start_time=start_times[1]
	
	#Bid (price) is the highest price that a buyer (i.e., bidder) is willing to pay for a good
	#Ask or offer (price) represents the minimum price that a seller is willing to receive for the good
	
	bids_df 	= dexR:::clearing_calc_demand(market_df=market_df, start_time=the_start_time)
	offers_df 	= dexR:::clearing_calc_supply(market_df=market_df, start_time=the_start_time)
	
	tt <- dexR:::clearing_calc(bids_df=bids_df, offers_df=offers_df)

	
	if (!is.null(dexpa_comp)) {
		market_comp = input_db_requests_clearing(dexpa_comp, starttime=starttime)
		start_times<-unique(market_comp$start_time)
		
		the_start_time=start_times[1]
		
		#Bid (price) is the highest price that a buyer (i.e., bidder) is willing to pay for a good
		#Ask or offer (price) represents the minimum price that a seller is willing to receive for the good
		
		bids_comp 	= dexR:::clearing_calc_demand(market_df=market_comp, start_time=the_start_time)
		offers_comp	= dexR:::clearing_calc_supply(market_df=market_comp, start_time=the_start_time)
		
		tt_comp <- dexR:::clearing_calc(bids_df=bids_comp, offers_df=offers_comp)
		
		quantity_traded_comp = tt_comp$quantity_traded
		clearing_price_comp	= tt_comp$clearing_price
	} else {
		offers_comp=NULL
		bids_comp=NULL
	}
	
	quantity_traded = tt$quantity_traded
	clearing_price	= tt$clearing_price
	
	dexpa$fig$width  = 800
	dexpa$fig$height = 600
	
	dexpa$fig$init(dexpa, outdir=paste(dexpa$dirs$output$figures, "lines", sep="/"), 
			filename = filename)
	
	dexR::output_figure_singleclearing_twisted(dexpa, title=title, offers_df=offers_df, bids_df=bids_df,
			offers_comp=offers_comp, bids_comp=bids_comp)
	
	lines(  c(0,quantity_traded, quantity_traded),
			c(clearing_price, clearing_price, min(offers_df$price_requested, bids_df$price_requested)),
			lty="dashed")
	text(	c(0,quantity_traded),
			c(clearing_price,min(offers_df$price_requested,bids_df$price_requested)),
			labels=c(clearing_price,quantity_traded),pos=c(3,4),srt=45)

	dexpa$fig$close()
}