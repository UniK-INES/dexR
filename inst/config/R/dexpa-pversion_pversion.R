 ################################################################################
# Project version specific DEX Properties
#
# Project:		dexR
# Version:		A
# Last update: 		08/03/2019
# Author: 		Sascha Holzhauer
################################################################################

### General ####################################################################

# Out-commented because it needs to be defined before setup - remove comment-#
# after setup!

#dexpa$sim$version			<- "testversion"

### Directories ################################################################
# Adjust project version-specific folders:
dexpa$dirs <-  rapply(dexpa$dirs, gsub, how="replace", pattern = "%PVERSION%", 
			replacement=dexpa$sim$version, fixed=TRUE)
dexpa$files <- rapply(dexpa$files, gsub, how="replace", pattern = "%PVERSION%",
			replacement=dexpa$sim$version, fixed=TRUE)

### Files ######################################################################


### Simulation #################################################################
dexpa$sim$firstdeliverystart$delay 	<- 0

### Logger settings ############################################################
futile.logger::flog.threshold(futile.logger::DEBUG, name='dexr')

### Server #####################################################################
dexpa$server$controls           	<- 100
dexpa$server$matchbasetime      	<- "true"

