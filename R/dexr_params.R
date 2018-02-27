#' Merge the given list into the default dexpa parameter list.
#' Applies \code{\link[utils]{mergeList}}.
#' @param dexpa 
#' @return given list modified by adding undefined parameter of dexpa default paramter list.
#' 
#' @author Sascha Holzhauer
#' @export
param_mergeDefaultDexpa <- function(dexpa = list()) {
	################################################################
	# General SIMulation Properties:
	################################################################
	
	if (!exists("defdexpa")) defdexpa <- list()
	defdexpa$sim <- list()

	defdexpa$sim$version					<- "version"
	defdexpa$sim$scenario				<- "scenario"
	defdexpa$sim$runids					<- c("0-0")
	
	### Directories ################################################################
	defdexpa$dirs <- list()
	defdexpa$dirs$project			<- "./"
	
	defdexpa$dirs$output <- list()
	defdexpa$dirs$outputdir			<- paste(defdexpa$dirs$project, "output/version/", sep="")	
	defdexpa$dirs$output$rdata		<- paste(defdexpa$dirs$outputdir, "rData/", sep="") 
	defdexpa$dirs$output$figures		<- paste(defdexpa$dirs$outputdir, "figures/", sep="")
	defdexpa$dirs$output$reports		<- paste(defdexpa$dirs$outputdir, "reports/", sep="")
	
	### DB Settings ##################################################################
	defdexpa$db <- list()	
	defdexpa$db$host			<- "localhost"

	defdexpa$db$suname			<- "postgres"
	defdexpa$db$supassword			<- "NotSet"
	
	### XML attributes Names ###########################################################
	defdexpa$xml <- list()	
	
	### Figure Settings ###########################################################
	defdexpa$fig <- list()
	defdexpa$fig$resfactor		<- 3
	defdexpa$fig$outputformat 	<- "png" #"jpeg"
	defdexpa$fig$init			<- dexR::output_figure_init
	defdexpa$fig$close			<- dev.off
	defdexpa$fig$numfigs			<- 1
	defdexpa$fig$numcols			<- 1
	defdexpa$fig$height			<- 700
	defdexpa$fig$width			<- 1000
	defdexpa$fig$splitfigs		<- FALSE
	defdexpa$fig$plottitle		<- TRUE
	defdexpa$fig$alpha			<- 0.7
	defdexpa$fig$linewidth		<- 1
	defdexpa$fig$facetlabelsize	<- 12
	
	### Colour Settings ###########################################################
	defdexpa$colours <- list()
	defdexpa$colours$products 		<- c(	"1" = "blue",
							"2" = "red")
	defdexpa$colours$statuses 		<- c(	"0" = "blue",
							"1" = "green",
							"2" = "blue",
							"3" = "orange",
							"6" = "red")
	
	### Debug Settings ############################################################
	defdexpa$debug <- list()
	# the higher, the more verbose
	defdexpa$debug$global 	<- 0
	defdexpa$debug$db		<- NA
	defdexpa$debug$input		<- NA
	defdexpa$debug$output	<- NA
	defdexpa$debug$fig		<- NA
	
	result <- modifyList(defdexpa, dexpa)
	result
}
#' Get the default dexpa parameter list
#' @return list of default dexpa parameters
#' 
#' @author Sascha Holzhauer
#' @export
param_getDefaultDexpa <- function() {
	param_mergeDefaultDexpa()
}

#' Get the dexpa parameter list for working with example data
#' @return list of dexpa parameters to run examples
#' 
#' @author Sascha Holzhauer
#' @export
param_getExamplesDexpa <- function() {
	dexpa <- param_mergeDefaultDexpa()

	dexpa$dirs <- list()
	dexpa$dirs$outputdir		<- system.file("extdata", "output/version", package = "craftyr")
	
	dexpa$dirs$output <- list()
	dexpa$dirs$output$simulation	<- paste(dexpa$dirs$outputdir, "/simulation/", sep="")
	dexpa$dirs$output$rdata		<- paste(dexpa$dirs$outputdir, "/rData/", sep="") 
	dexpa$dirs$output$raster		<- paste(dexpa$dirs$outputdir, "/raster/", sep="") 
	dexpa$dirs$output$figures	<- paste(dexpa$dirs$outputdir, "/figures/", sep="")
	dexpa$dirs$output$reports	<- paste(dexpa$dirs$outputdir, "/reports/", sep="")
	
	dexpa$dirs$data				<- system.file("extdata", "data/version", package = "craftyr")
	
	dexpa$fig$init <- function(dexpa, outdir, filename) {}
	dexpa$fig$close<- function() {}
	dexpa$sim$id <- "Example"
	dexpa
}
