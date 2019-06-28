source(paste(dirname(tempfile()), "Testproject/scripts/dexpa-machine_machine.R", sep="/"))
dexpa$sim$version <- "testversion"
source(paste(dexpa$dirs$scripts, "/", dexpa$sim$version, "/dexpa-pversion_", dexpa$sim$version, ".R", sep=""))

dexpa$sim$id 	  <- "Testrun01"


# requires a PostgreSQL-DB with following configuration and extension pgcrypto (see documentation "Market Server DEX" > "Installation and Configuration" > "DB")

dexpa$sim$duration		<- 2*60*60             # in sec
dexpa$sim$timefactor		<- 60.0

dexR::hl_experiment(dexpa, basetime = as.numeric(round(strptime("30/09/19 12:00", "%d/%m/%y %H:%M"),"mins"))*1000)
