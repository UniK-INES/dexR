hl_statistics_comp_energy <- function(dexpa, dp2) {
	cinfo1 <- input_db_clearings(dexpa)
	cinfo1$id <- input_db_runID(dexpa)
	cinfo2 <- input_db_clearings(dp2)
	cinfo2$id <- input_db_runID(dp2)
	
	requestdata1  <- input_db_requests(dexpa)
	requestdata1$id <- input_db_runID(dexpa)
	requestdata2 <- input_db_requests(dp2)
	requestdata2$id <- input_db_runID(dp2)
	
	output_statistics_comp_energy(dexpa, cinfos = rbind(cinfo1,cinfo2), requestdata = rbind(requestdata1, requestdata2))
}