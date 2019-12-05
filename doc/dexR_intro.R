## ---- eval=FALSE--------------------------------------------------------------
#  ## DexpaProject.R
#  #### Load common packages ######################################################
#  library(kfigr)
#  library(shbasic)
#  library(RPostgreSQL)
#  
#  ### Load default parameters ####################################################
#  if (!exists("dexpa")) dexpa <-  dexR::param_getDefaultDexpa()
#  
#  ### Simulation Data ############################################################
#  dexpa$sim$project		<- "Projectname"
#  
#  ### Logger settings ############################################################
#  futile.logger::flog.threshold(futile.logger::INFO, name='dexr')
#  
#  ### DB Settings ################################################################
#  if(!is.list(dexpa$db)) dexpa$db <- list()
#  dexpa$db$host			<- "localhost"
#  dexpa$db$port			<- "5432"
#  dexpa$db$dbname			<- "dexr"
#  dexpa$db$username		<- "user"
#  dexpa$db$password		<- "password"
#  
#  dexpa$db$suname			<- "postgres"
#  dexpa$db$supassword		<- "supassword"

## ---- eval=FALSE--------------------------------------------------------------
#  # Only contained when the particular script is only executed on a specific machine!
#  # Otherwise. the machine-specific file needs to be executed before.
#  source("/PATH-TO/dexpa-machine_XY.R")
#  
#  # dexpa$dirs$scripts is set by machine-specific file:
#  setwd(paste(dexpa$dirs$scripts, sep="/"))
#  # usually, the project version specific `dexpa` file is a level above:
#  source("../dexpa-pversion_pversion.R")

## ---- eval=FALSE--------------------------------------------------------------
#  dexR::input_db_db2dump(dexpa, dumpdir = paste("dump_", dexpa$sim$id, sep=""))
#  dp1 			<- dexpa
#  dp1$db$dbname 	<- "enavi_01-01"
#  dexR::input_db_dump2db(dp1, dumpfile=paste("dump_", dp1$sim$id, sep=""))

## ---- eval=FALSE--------------------------------------------------------------
#  futile.logger::flog.threshold(futile.logger::DEBUG, name='dexr')
#  futile.logger::flog.threshold(futile.logger::TRACE, name='dexr.input')

## ---- eval=FALSE--------------------------------------------------------------
#  futile.logger::flog.appender(appender.file(filename), name='dexr.output')

