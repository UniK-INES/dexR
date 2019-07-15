source(system.file("demo/dexpa-machine_testmachine.R", package="dexR"))
dexR::setup_environment(dexpa)

# for this demo a special machine-specfic dexpa file is required:
file.copy(from= system.file("demo/dexpa-machine_testmachine.R", package="dexR"), 
			to=paste(dexpa$dirs$scripts, "dexpa-machine_machine.R",sep="/"), 
			overwrite = TRUE, copy.mode = TRUE)

dexpa$sim$version <- "testversion"
dexpa <- dexR::setup_project_version(dexpa)

# overwrite dexpa-project with demo-specific parameters:
file.copy(from= system.file("demo/dexpa-project.R", package="dexR"), 
		to=dexpa$dirs$scripts, 
		overwrite = TRUE, copy.mode = TRUE)

dexpa$sim$id 	  <- "Testrun01"
dexR::hl_config_copycsvtemplates(dexpa)

futile.logger::flog.info("NOTE: Copy or link EMG config tool to release dir %s! or change dexpa$dirs$emgconfigtool before running!",
	dexpa$dirs$emgconfigtool,
	"dexr.demo.config")

futile.logger::flog.info("NOTE: Copy or link EMG rundir to release dir %s! or change dexpa$dirs$emgrundir before running!",
	dexpa$dirs$emgrundir,
	"dexr.demo.config")

futile.logger::flog.info("NOTE: Link DEX server with POM file %s! or change dexpa$files$backendPOM before running!",
	dexpa$files$backendPOM,
	"dexr.demo.config")


