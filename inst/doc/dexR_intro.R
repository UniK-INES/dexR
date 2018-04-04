## ---- eval=FALSE---------------------------------------------------------
#  ## DexpaProject.R
#  [...]

## ---- eval=FALSE---------------------------------------------------------
#  # Only contained when the particular script is only executed on a specific machine!
#  # Otherwise. the machine-specific file needs to be executed before.
#  source("/PATH-TO/dexpa-machine_XY.R")
#  
#  # dexpa$dirs$scripts is set by machine-specific file:
#  setwd(paste(dexpa$dirs$scripts, sep="/"))
#  # usually, the setting/scenario specific dexpa.R is a level above:
#  source("../dexpa.R")

## ---- eval=FALSE---------------------------------------------------------
#  ## DexpaProject.R
#  [...]

## ---- eval=FALSE---------------------------------------------------------
#  csv_LandUseIndex_rbinded <- data
#  input_tools_save(simp, "csv_LandUseIndex_rbinded")
#  rm (csv_LandUseIndex_rbinded)

## ---- eval=FALSE---------------------------------------------------------
#  input_tools_load(simp, "csv_LandUseIndex_rbinded")

## ---- eval=FALSE---------------------------------------------------------
#  data(package="craftyr")
#  source("../demo/simp-machine.R")
#  simp$fig$init(simp, filename = "example/hist_aft.png")
#  hist(cellData$LandUseIndex, breaks=3, col="red")
#  dev.off()
#  

## ---- eval=FALSE---------------------------------------------------------
#  futile.logger::flog.threshold(futile.logger::DEBUG, name='dexr')
#  futile.logger::flog.threshold(futile.logger::TRACE, name='dexr.input')

## ---- eval=FALSE---------------------------------------------------------
#  futile.logger::flog.appender(appender.file(filename), name='dexr.output')

