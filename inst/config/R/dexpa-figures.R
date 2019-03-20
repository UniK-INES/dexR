################################################################################
# Project-specific dexR Properties:
#
# Project:		Test
# Last update: 		08/03/2019
# Author: 		Sascha Holzhauer
################################################################################

#### Load common packages ######################################################
library(kfigr)
library(shbasic)
library(RPostgreSQL)

### Load default parameters ####################################################
if (!exists("dexpa")) dexpa <-  dexR::param_getDefaultDexpa()

### Simulation Data ############################################################
dexpa$sim$project		<- "Projectname"

### Logger settings ############################################################
futile.logger::flog.threshold(futile.logger::INFO, name='dexr')

### DB Settings ################################################################
if(!is.list(dexpa$db)) dexpa$db <- list()
dexpa$db$host			<- "localhost"
dexpa$db$port			<- "5432"
dexpa$db$dbname			<- "enavi"
dexpa$db$username		<- "enavi"
dexpa$db$password		<- "enavi!"

dexpa$db$suname			<- "postgres"
dexpa$db$supassword		<- "WiScAn07"
