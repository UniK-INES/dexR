## ---- eval=FALSE---------------------------------------------------------
#  ## DexpProject.R
#  [...]

## ---- eval=FALSE---------------------------------------------------------
#  # Only contained when the particular script is only executed on a specific maschine!
#  # Otherwise. the maschine=specific file needs to be executed before.
#  source("/PATH-TO/dexp-machine_XY.R")
#  
#  # simp$dirs$simp is set by maschine-specific file:
#  setwd(paste(dexp$dirs$dexp, dexp$sim$folder, "project/", dexp$sim$task, sep="/"))
#  # usually, the setting/scenario specific dexp.R is a level above:
#  source("../dexp.R")

## ---- eval=FALSE---------------------------------------------------------
#  futile.logger::flog.threshold(futile.logger::DEBUG, name='dexp')
#  futile.logger::flog.threshold(futile.logger::TRACE, name='dexp.input')

## ---- eval=FALSE---------------------------------------------------------
#  futile.logger::flog.appender(appender.file(filename), name='test.logger')

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

