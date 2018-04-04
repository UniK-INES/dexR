library(roxygen2)
library("devtools")
full <- TRUE #required to update NAMESPACE by roxygen2

# has_devel() 	# does not work because of 'C:\Program' is not recognized as an internal or 
# external command, operable program or batch file.

pkgsName <- "dexR"
setwd("/daten/INES/workspace/")

#create(pkgsName)

setwd(paste("./", pkgsName, sep=""))
#devtools::use_vignette("craftyr-intro")
#devtools::use_vignette("craftyr-raster")

if (full) {
	document()
}

setwd("..")

devtools::build(pkgsName)

if (full) {
	devtools::build_vignettes(pkgsName)
}


install(pkgsName)
# devtools::install_bitbucket("geoslurg/craftyr@default")
# for eddie (use qlogin session: qlogin -l h_vmem=2G):
# module load R/3.2.2
# R
# .libPaths(.libPaths()[2])
# devtools::install_bitbucket("craftyr", username="geoslurg", ref="default")
if (full) {
	browseVignettes("dexR")
}


## prepare data objects
#cellData <- read.csv("./inst/extdata/NEEDS ADAPTATION/Scenario-0-0-Region-Cell-2010.csv")
#save(cellData, file="./data/cellData.rda")