#' Extracts segment coordination from data.frame  for a twisted diagramm
#' @param df 
#' @return combined data point
#' 
#' @author Friedrich Krebs
#' @author Sascha Holzhauer
calc_segments_twisted <- function(df){
	x0=c(0)
	y0=c(df[1,"price_requested"])
	x1=c(df[1,"cum_energy_requested"])
	y1=c(df[1,"price_requested"])
	for (i in 2:(nrow(df))){
		x0=c(x0,df[i-1,"cum_energy_requested"])
		y0=c(y0,df[i,"price_requested"])
		x1=c(x1,df[i,"cum_energy_requested"])
		y1=c(y1,df[i,"price_requested"])
		x0=c(x0,df[i-1,"cum_energy_requested"])
		y0=c(y0,df[i-1,"price_requested"])
		x1=c(x1,df[i-1,"cum_energy_requested"])
		y1=c(y1,df[i,"price_requested"])
	}
	return (cbind(y0,x0,y1,x1))
}
#' Extracts segment coordination from data.frame
#' 
#' @param df 
#' @return combined data point
#' 
#' @author Friedrich Krebs
calc_segments <- function(df){
	x0=c(0)
	y0=c(df[1,"price_requested"])
	x1=c(df[1,"cum_energy_requested"])
	y1=c(df[1,"price_requested"])
	for (i in 2:(nrow(df))){
		x0=c(x0,df[i-1,"cum_energy_requested"])
		y0=c(y0,df[i,"price_requested"])
		x1=c(x1,df[i,"cum_energy_requested"])
		y1=c(y1,df[i,"price_requested"])
		x0=c(x0,df[i-1,"cum_energy_requested"])
		y0=c(y0,df[i-1,"price_requested"])
		x1=c(x1,df[i-1,"cum_energy_requested"])
		y1=c(y1,df[i,"price_requested"])
	}
	return (cbind(x0,y0,x1,y1))
}
#' Plots demand and supply curve for a single uniform price clearing with price on x-axis
#' 
#' @param title Title to be plotted in figure 
#' @param offers_df 
#' @param bids_df  
#' @return plot
#' 
#' @author Friedrich Krebs
#' @author Sascha Holzhauer
#' @export
output_figure_singleclearing_twisted <- function(dexpa, title="", offers_df, bids_df, offers_comp = NULL, bids_comp = NULL){
	price_requested_lim=c(min(bids_df$price_requested, offers_df$price_requested, bids_comp$price_requested, 
					offers_comp$price_requested),
			max(bids_df$price_requested,offers_df$price_requested, bids_comp$price_requested, 
					offers_comp$price_requested))
	quantity_lim=c(0,max(sum(bids_df$energy_requested), sum(bids_comp$energy_requested), -sum(offers_df$energy_requested),
					-sum(offers_comp$energy_requested)))
	
	plot(price_requested_lim[1],xlim=price_requested_lim,xlab="price_requested",
			quantity_lim[1],ylim=quantity_lim,ylab="energy_requested"
			,main=title
			,type="n")
	
	segs=calc_segments_twisted(bids_df)
	segments(segs[,1],segs[,2],segs[,3],segs[,4],lwd=dexpa$fig$linewidth,col="red",pch=0)
	#text(bids_df$cum_energy_requested,bids_df$price_requested,labels=bids_df$submitter_id,col="red",pos=3,offset=1)
	
	if(!is.null(bids_comp)) {
		segs=calc_segments_twisted(bids_comp)
		segments(segs[,1],segs[,2],segs[,3],segs[,4],lwd=dexpa$fig$linewidth,col="red",pch=1)
	}
	
	segs=calc_segments_twisted(offers_df) 
	segments(segs[,1],segs[,2],segs[,3],segs[,4],lwd=dexpa$fig$linewidth,col="blue",pch=0)
	
	if(!is.null(offers_comp)) {
		segs=calc_segments_twisted(offers_comp)
		segments(segs[,1],segs[,2],segs[,3],segs[,4],lwd=dexpa$fig$linewidth,col="blue",pch=1)
	}	
	
	legend(
			"topright" 
			,c("Demand","Supply")
			,col = c("red","blue")
			,pch=c(0,1)
	)
}
#' Plots demand and supply curve for a single uniform price clearing with energy on x-axis (German version)
#' 
#' @param title Title to be plotted in figure 
#' @param offers_df 
#' @param bids_df 
#' @return 
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_singleclearing_de <- function(dexpa, title="", offers_df, bids_df){
	price_requested_lim=c(min(bids_df$price_requested,offers_df$price_requested),max(bids_df$price_requested,offers_df$price_requested))
	quantity_lim=c(0,max(sum(bids_df$energy_requested),-sum(offers_df$energy_requested)))
	
	plot(quantity_lim[1],xlim=quantity_lim, xlab="Energiemenge in Wh",
			price_requested_lim[1], ylim=price_requested_lim, ylab="Preis in EUR"
			,main=title
			,type="n")
	
	segs=calc_segments(bids_df)
	segments(segs[,1],segs[,2],segs[,3],segs[,4],lwd=2,col=rgb(240,50,85, max = 255))
	#text(bids_df$cum_energy_requested,bids_df$price_requested,labels=bids_df$submitter_id,col="red",pos=3,offset=1)
	
	segs=calc_segments(offers_df) 
	segments(segs[,1],segs[,2],segs[,3],segs[,4],lwd=2,col=rgb(50,240,205,max = 255))
	legend(
			"topright" 
			,c("Nachfrage","Angebot")
			,col = c(rgb(240,50,85, max = 255),rgb(50,240,205,max = 255))
			,pch=0
	)
}
#' Plots demand and supply curve for a single uniform price clearing with energy on x-axis (English version)
#' 
#' @param title Title to be plotted in figure 
#' @param offers_df 
#' @param bids_df 
#' @return 
#' 
#' @author Sascha Holzhauer
#' @export
output_figure_singleclearing_en <- function(dexpa, title="",offers_df,bids_df){
	price_requested_lim=c(min(bids_df$price_requested,offers_df$price_requested),max(bids_df$price_requested,offers_df$price_requested))
	quantity_lim=c(0,max(sum(bids_df$energy_requested), -sum(offers_df$energy_requested)))
	
	par(mar=c(4, 4, 0, 0) + 0.1)
	plot(quantity_lim[1],xlim=quantity_lim,xlab="Requested energy in Wh",
			price_requested_lim[1],ylim=price_requested_lim,ylab="Price in EUR"
			,main=title
			,type="n")
	
	segs=calc_segments(bids_df)
	segments(segs[,1],segs[,2],segs[,3],segs[,4],lwd=2,col="red")
	#text(bids_df$cum_energy_requested,bids_df$price_requested,labels=bids_df$submitter_id,col="red",pos=3,offset=1)
	
	segs=calc_segments(offers_df) 
	segments(segs[,1],segs[,2],segs[,3],segs[,4],lwd=2,col="blue")
	legend(
			"topright" 
			,c("Demand","Supply")
			,col = c("red","blue")
			,pch=0
	)
}