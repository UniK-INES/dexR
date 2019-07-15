#' Runs OpSim control manager
#' 
#' @param dexpa 
#' @param outfileopsim 
#' @return run OpSim control manager
#' 
#' @author Sascha Holzhauer
#' @export
hl_opsim_runmanager <- function(dexpa, outfileopsim = paste(dexpa$dirs$output$logs, dexpa$opsim$control$logfile, sep="/")) {
	
	futile.logger::flog.info("Run opsim (%s) in %s with JRE %s. Log files in %s...",
			dexpa$opsim$sontrol$jar,
			dexpa$opsim$control$rundir,
			dexpa$opsim$control$jre,
			name = "dexr.hl.opsim")
	
	futile.logger::flog.debug("Set working directory to %s...", 
			dexpa$opsim$control$rundir,
			name = "dexr.hl.opsim")
	
	setwd(dexpa$opsim$control$rundir)
	
	system2(wait=FALSE, paste(dexpa$opsim$control$jre, "java", sep="/"), 
			args = paste(' -jar ',
					dexpa$opsim$sontrol$jar),
			stdout=outfileopsim, stderr=outfileopsim)
}