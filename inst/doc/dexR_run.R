## ---- eval=FALSE---------------------------------------------------------
#  source("/home/USER/dexr/scripts/dexpa-machine_machine.R")
#  dexpa$sim$version <- "testversion"	
#  dexpa$sim$id 	  <- "Testrun01"
#  source("/home/USER/dexr/scripts/dexpa-pversion_pversion.R")
#  
#  dexR::hl_experiment(dexpa, basetime = as.numeric(round(strptime("30/09/19 12:00", "%d/%m/%y %H:%M"),"mins"))*1000)

## ---- eval=FALSE---------------------------------------------------------
#  source("/home/USER/dexr/scripts/dexpa-machine_machine.R")
#  dexpa$sim$version <- "testversion"	
#  dexpa$sim$id 	  <- "Testrun01"
#  source("/home/USER/dexr/scripts/dexpa-pversion_pversion.R")
#  
#  dexR::hl_experiment_cluste(dexpa, basetime = as.numeric(round(strptime("30/09/19 12:00", "%d/%m/%y %H:%M"),"mins"))*1000),
#  		offset = round(basetime - as.numeric(Sys.time())*1000))

## ---- eval=FALSE---------------------------------------------------------
#  source("/home/sascha/git/enavi/market/enavi-r/config/dexpa-machine_T460s.R")
#  dexpa$sim$version 	<- "testversion"	
#  dexpa$sim$id 		<- "Testrun01"
#  
#  setwd(paste(dexpa$dirs$scripts, sep="/"))
#  source(paste(dexpa$dirs$scripts, dexpa$sim$version, "dexpa.R", sep="/"))
#  
#  ####### Configuration
#  
#  dexpa$sim$timefactor	<- 60
#  dexpa$sim$duration	<- 10*60*60 # seconds
#  
#  outputfile=paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, ".log", sep="")
#  outfilemarket = paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, "_market.log", sep="")
#  outfileemg = paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, "_emg.log", sep="")
#  outfileemg = paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, "_emg.log", sep="")
#  
#  basetime = as.numeric(round(strptime("30/09/30 12:00", "%d/%m/%y %H:%M"),"mins"))*1000
#  offset = basetime - round(as.numeric(Sys.time())*1000)
#  
#  shbasic::sh.ensurePath(outputfile, stripFilename = T)
#  con <- file(outputfile)
#  sink(con, append=TRUE)
#  sink(con, append=TRUE, type="message")
#  
#  infoData <- data.frame()
#  
#  
#  ####### Run experiment
#  
#  futile.logger::flog.info("Starting backend with basetime = %s...",
#  		format(as.POSIXct(basetime, tz="GTM", origin = "1970-01-01"), "%d/%m/%y %H:%M:%S"),,
#  		name = "dexr.hl.experiment.emg")
#  infoData <- dexR::hl_experiment_runbackend(dexpa, outfilesys = outfilemarket,
#  		basetime = basetime,
#  		offset = offset,
#  		startServer=F)
#  
#  
#  dexR::hl_experiment_configemg(dexpa, outfilesys= if (is.null(dexpa$emg$emgconfigoutput)) "" else
#  	paste(dexpa$dirs$output$logs, "/", dexpa$sim$id, "/", dexpa$sim$id, "_", dexpa$emg$emgconfigoutput, ".log", sep=""))
#  
#  
#  dexR::hl_experiment_awaitemgstartup(dexpa)
#  
#  
#  message = dexR::server_start(dexpa)
#  futile.logger::flog.info(message, name = "dexr.hl.experiment")	
#  
#  
#  futile.logger::flog.info("Wait for simulation to complete (Duration: %d / factor: %d = %d", dexpa$sim$duration,
#  		dexpa$sim$timefactor, dexpa$sim$duration/dexpa$sim$timefactor, name = "dexr.hl.experiment")
#  Sys.sleep(dexpa$sim$duration/dexpa$sim$timefactor)
#  
#  
#  ######## Stop services:
#  
#  dexR::hl_experiment_stopemg(dexpa)
#  
#  dexR::input_db_db2dump(dexpa, dumpdir = paste("dump_", dexpa$sim$id, sep=""))
#  
#  try(dexR::createFullReport(dexpa, outputfile = paste("StageA_FullReport_", dexpa$sim$id, ".pdf", sep="")))
#  
#  dexR::server_shutdown(dexpa)
#  
#  if (outputfile != "") {
#  	sink()
#  	sink()
#  	sink(type="message")
#  }
#  
#  lapply(DBI::dbListConnections(DBI::dbDriver("PostgreSQL")), DBI::dbDisconnect)

