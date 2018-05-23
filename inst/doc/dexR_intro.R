## ---- eval=FALSE---------------------------------------------------------
#  ## DexpaProject.R
#  #### COMMON PACKAGES ###########################################################
#  library(shbasic)
#  
#  #### FUNCTIONS #################################################################
#  #eg. for simp$dirs$param$getparamdir
#  
#  ### Simulation Data ############################################################
#  if (!exists("dexpa")) dexpa <-  dexR::param_getDefaultDexpa()
#  
#  ### DB Settings ##################################################################
#  if(!is.list(dexpa$db)) dexpa$db <- list()
#  dexpa$db$host			<- "localhost"
#  dexpa$db$port			<- "5432"
#  dexpa$db$dbname			<- "enavi"
#  dexpa$db$username		<- "enavi"
#  dexpa$db$password		<- "<password>"
#  
#  dexpa$db$suname			<- "postgres"
#  dexpa$db$supassword		<- "<supassword>"

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
#  dexR::input_db_db2dump(dexpa, dumpdir = paste("dump_", dexpa$sim$id, sep=""))
#  dp1 			<- dexpa
#  dp1$db$dbname 	<- "enavi_01-01"
#  dexR::input_db_dump2db(dp1, dumpfile=paste("dump_", dp1$sim$id, sep=""))

## ---- eval=FALSE---------------------------------------------------------
#  futile.logger::flog.threshold(futile.logger::DEBUG, name='dexr')
#  futile.logger::flog.threshold(futile.logger::TRACE, name='dexr.input')

## ---- eval=FALSE---------------------------------------------------------
#  futile.logger::flog.appender(appender.file(filename), name='dexr.output')

