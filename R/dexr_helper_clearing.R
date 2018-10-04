#' Calculate supply curve based on market data frame and a start time of the dellivery period
#' 
#' @param market_df 
#' @param start_time 
#' @return data frame of cumulated offers ordered by increasing price
#' 
#' @author Friedrich Krebs
clearing_calc_supply <- function(market_df, start_time){
	offers_df=market_df[market_df$start_time==start_time & market_df$energy_requested<0,c("uid","energy_requested","price_requested")]
	#offers to supply electricity are ranked in order of increasing price
	offers_df=offers_df[order(offers_df$price_requested),]
	#then quantities are cumulated
	offers_df[1,"cum_energy_requested"]=-offers_df[1,"energy_requested"]
	if (nrow(offers_df) > 1) {
		for (i in 2:nrow(offers_df)){
			offers_df[i,"cum_energy_requested"]=offers_df[i-1,"cum_energy_requested"]-offers_df[i,"energy_requested"]
		}
	}
	return(offers_df)
}
#' Calculate demand curve based on market data frame and a start time of the delivery period
#' 
#' @param market_df 
#' @param start_time 
#' @return data frame of cumulated bids ordered by decreasing price
#' 
#' @author Friedrich Krebs
clearing_calc_demand <- function(market_df, start_time){
	bids_df=market_df[market_df$start_time==start_time & market_df$energy_requested>0,c("uid","energy_requested","price_requested")]
	#bids to buy electricity are ranked in order of decreasing price
	bids_df=bids_df[order(-bids_df$price_requested),]
	#then quantities are cummulated
	bids_df[1,"cum_energy_requested"]=bids_df[1,"energy_requested"]
	if (nrow(bids_df) > 1) {
		for (i in 2:nrow(bids_df)){
			bids_df[i,"cum_energy_requested"]=bids_df[i-1,"cum_energy_requested"]+bids_df[i,"energy_requested"]
		}
	}
	return(bids_df)
}
#' Find the price for an amount of energy in a data frame of stacked bids or offers
#' 
#' @param energy_amount 
#' @param df holds demand or supply curve
#' @return NaN if there is no price found for energy_amount 
#' 
#' @author Friedrich Krebs
clearing_find_price_requested <- function(energy_amount,df){
	price_requested=NaN  # TODO not used
	last_energy_entry=0
	i=1
	repeat {
		energy_entry=df[i,]$cum_energy_requested
		if (energy_amount>last_energy_entry & energy_amount<=energy_entry){
			return(df[i,]$price_requested)
		} else {
			if (i==nrow(df)){
				return(NaN)
			} else {
				last_energy_entry=energy_entry
				i=i+1
			}
		}
	}
}
#' Calculate the system marginal price (SMP) and the amount of energy to be traded
#' 
#' @param bids_df holds the demand curve
#' @param offers_df holds the supply curve
#' @return data.frame with clearing_price and quantity_traded
#' 
#' @author Friedrich Krebs
clearing_calc <- function(bids_df, offers_df){
	
	##put all bids and offers in on data frame
	clearing_df=rbind(offers_df,bids_df)
	##order data frame
	clearing_df=clearing_df[order(clearing_df$cum_energy_requested),]
	
	##loop over all quantities in clearing_df
	i=1
	clearing_price=NaN
	quantity_traded=0
	repeat {
		quantity=clearing_df[i,]$cum_energy_requested
		bid_price=dexR:::clearing_find_price_requested(quantity,bids_df)
		offer_price=dexR:::clearing_find_price_requested(quantity,offers_df)
		if (is.nan(bid_price) | is.nan(offer_price) | offer_price>bid_price){
			##break if
			## for the given quantity there is no bid price or no offer price
			## or offer price is higher bid price
			if (i>1){
				if (clearing_df[i-1,]$energy_requested<0){
					##clearing price determined by bid
					clearing_price=dexR:::clearing_find_price_requested(quantity_traded,bids_df)
				} else {
					##clearing price determined by offer
					clearing_price=dexR:::clearing_find_price_requested(quantity_traded,offers_df)
				}
			}
			return(data.frame(clearing_price=clearing_price,quantity_traded=quantity_traded))
		} else {
			quantity_traded=quantity
			if (i<nrow(clearing_df)){
				i=i+1
			} else {
				return(data.frame(clearing_price=offer_price,quantity_traded=quantity_traded))
			}
		}
	}
}