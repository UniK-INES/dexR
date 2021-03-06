library(roxygen2)
library("devtools")
full <- TRUE #required to update NAMESPACE by roxygen2

# has_devel() 	# does not work because of 'C:\Program' is not recognized as an internal or 
# external command, operable program or batch file.

pkgsName <- "dexR"
setwd("~/git")

#create(pkgsName)

setwd(paste("./", pkgsName, sep=""))

if (full) {
	document()
}

setwd("..")

devtools::build(pkgsName)

if (full) {
	devtools::build_vignettes(pkgsName)
}


install(pkgsName, upgrade="always", build_vignettes=full)

if (full) {
	browseVignettes("dexR")
}


## prepare data objects
#cellData <- read.csv("./inst/extdata/NEEDS ADAPTATION/Scenario-0-0-Region-Cell-2010.csv")
#save(cellData, file="./data/cellData.rda")