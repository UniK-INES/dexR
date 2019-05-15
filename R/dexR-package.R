#' DEX R
#'
#' \tabular{ll}{
#' Package \tab		dexR \cr
#' Model:  \tab		<Basic> \cr
#' Licence:\tab		GPL v3 \cr
#' Be aware of:\tab	-\cr
#' Version: \tab	0.6.7 \cr
#' Date	   	\tab 	2019-05-15 \cr
#' Changes	\tab	refactoring, comment \cr
#' }
#' 
#' @description The package offers deployment and anlyses methods for DEX.
#' @name  	dexR
#' @aliases dexR
#' @import R.oo
#' @docType package
#' @title 	DexR
#' @author 	Sascha Holzhauer, UniKassel \email{Sascha.Holzhauer@@uni-kassel.de"}
#' @keywords energy simulation market analyses
#' 
NULL
.onAttach <- function(...) {
	packages = installed.packages()
	packageStartupMessage(paste("Welcome to dexR ", packages[packages[,"Package"] == "dexR", "Version"], sep=""))
}