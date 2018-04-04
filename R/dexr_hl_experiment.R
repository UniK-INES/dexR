#' Run backend server to experiment
#' @param dexpa 
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
hl_experiment_runbackend <- function(dexpa) {
	futile.logger::flog.info("Starting Market Backend server...", name = "dexr.hl.experiment")
	# Instatiate server:
	system(wait=FALSE, paste("mvn -f ", dexpa$dirs$backend, " spring-boot:run ",
					"-Dde.unik.enavi.market.testing.load=FALSE ",
					"-Dde.unik.enavi.market.time.factor=", dexpa$sim$timefactor, " ",
					"-Dde.unik.enavi.market.time.basetime=", as.numeric(round(Sys.time(),"mins"))*1000, sep=""))
	
	control = 0
	while (!server_isrunning(dexpa) && control < 20) {
		# wait...
		futile.logger::flog.info("Again, check availability of market backend server...",
				name = "dexr.hl.experiment")
		Sys.sleep(2)
		control = control + 1
	}
	
	if (control >= 20) {
		R.oo::throw("Starting market backend server NOT successful!")
	}
	
	# Import market products;
	hl_config_marketProducts2db(dexpa)
	
	# Insert client data:
	hl_config_clients2db(dexpa)
	
	# Start backend server:
	server_start(dexpa)
}
#' Run EMGs to experiment
#' @param dexpa 
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
hl_experiment_runemg <- function(dexpa) {
	## create XML configuration:
	futile.logger::flog.info("Creating EMG XML configuration...", name = "dexr.hl.experiment")
	system(wait=TRUE, paste("java",
		" -Dfile.encoding=UTF-8",
		" -classpath /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/resources.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/rt.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/jsse.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/jce.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/charsets.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/sunjce_provider.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/nashorn.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/sunec.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/sunpkcs11.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/zipfs.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/localedata.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/cldrdata.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/java-atk-wrapper.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/jaccess.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/icedtea-sound.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/dnsns.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/jfxrt.jar:/home/sascha/git/enavi/emg/src/emg-config-tool/emg-config-tool/target/classes:/home/sascha/.m2/repository/javax/activation/activation/1.1/activation-1.1.jar:/home/sascha/.m2/repository/org/freemarker/freemarker/2.3.27-incubating/freemarker-2.3.27-incubating.jar:/home/sascha/.m2/repository/org/apache/commons/commons-csv/1.5/commons-csv-1.5.jar:/home/sascha/.m2/repository/commons-cli/commons-cli/1.4/commons-cli-1.4.jar:/home/sascha/.m2/repository/ch/qos/logback/logback-classic/1.3.0-alpha4/logback-classic-1.3.0-alpha4.jar:/home/sascha/.m2/repository/ch/qos/logback/logback-core/1.3.0-alpha4/logback-core-1.3.0-alpha4.jar:/home/sascha/.m2/repository/org/slf4j/slf4j-api/1.8.0-beta1/slf4j-api-1.8.0-beta1.jar de.unik.ines.enavi.ctool.EmgConfigManager",
		" -i ", dexpa$sim$id,
		" -o ", dexpa$dirs$config,
		" -c ", paste(dexpa$dirs$config, dexpa$sim$id, "/DEX_Param_EnaviClient_", dexpa$sim$id, ".csv", sep=""),
		" -l ", paste(dexpa$dirs$config, dexpa$sim$id, "/DEX_Param_Loads_", dexpa$sim$id, ".csv", sep=""),
		" -g ", paste(dexpa$dirs$config, dexpa$sim$id, "/DEX_Param_Generations_", dexpa$sim$id, ".csv", sep=""),
		" -b ", paste(dexpa$dirs$config, dexpa$sim$id, "/DEX_Param_DevicesBuilding_", dexpa$sim$id, ".csv", sep=""),
		" -p ", paste(dexpa$dirs$config, dexpa$sim$id, "/DEX_Param_DevicesPVplant_", dexpa$sim$id, ".csv", sep=""),
		" -s ", paste(dexpa$dirs$config, dexpa$sim$id, "/DEX_Param_LoadProfiles_", dexpa$sim$id, ".csv", sep=""), sep=""))

	# copy static XML files:
	for (f in dexpa$xml$staticfiles) {
		file.copy(from = paste(dexpa$dirs$xmltemplatesstatic, f, sep="/"), to = paste(dexpa$dirs$config, "/", dexpa$sim$id, sep=""),
				overwrite = TRUE)
	}

	futile.logger::flog.info("Starting EMG...", name = "dexr.hl.experiment")
	path = getwd();
	setwd(dexpa$dirs$emgrundir)
	Sys.setenv(VMOPTS = paste("-Dorg.ogema.app.resadmin.replay_oncleanstart_path=", dexpa$dirs$config, dexpa$sim$id, sep=""))
	system2(wait=FALSE, "./start.sh", args = paste(
		"-clean --properties config/sh_ogema.properties", sep=""))
	setwd(path)
}
#' Retrieve backend server status information
#' @param dexpa 
#' @return JSON info map
#' 
#' @author Sascha Holzhauer
#' @export
hl_experiment_stopemg <- function(dexpa) {
	futile.logger::flog.info("Stopping EMG...", name = "dexr.hl.experiment")
	httr::GET(paste(dexpa$emg$url,dexpa$emg$api$shutdown,sep="/"))
}
#' Concuct experiment from starting backend server to creation of full report
#' @param dexpa 
#' @return report, database
#' 
#' @author Sascha Holzhauer
#' @export
hl_experiment <- function(dexpa) {
	hl_experiment_runbackend(dexpa)
	
	hl_experiment_runemg(dexpa)
	
	futile.logger::flog.info("Wait for simulation to complete (Duration:", dexpa$sim$duration, " / factor: ", 
			dexpa$sim$timefactor, name = "dexr.hl.experiment")
	Sys.sleep(dexpa$sim$duration/dexpa$sim$timefactor)
	
	dexR::input_db_db2dump(dexpa, dumpdir = paste("dump_", dexpa$sim$id, sep=""))
	dexR::input_db_dump2db(dp2, dumpfile=paste("dump_", dexpa$sim$id, sep=""))
	
	hl_experiment_stopemg(dexpa)
	server_shutdown(dexpa)
	
	dexR::createFullReport(dp2, outputfile = paste("StageA_FullReport_", dexpa$sim$id, ".pdf", sep=""))
}