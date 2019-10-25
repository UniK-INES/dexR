#' Retrieves requests data from DB and creates figure of gini coefficient of cost per KWh by delivery start time.
#' @param dexpa parameter
#' @param type either 'gen' or 'load'
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_energycosts_requests_giniByStartT <- function(dexpa, type = "load") {
	if (type == "gen") {
		data <- dexR::input_db_requests(dexpa, additionalwhere = "e.status IN (1,2) AND e.energy_accepted < 0")
		data$energy_accepted <- data$energy_accepted * (-1)
	} else if (type == "load") {
		data <- dexR::input_db_requests(dexpa, additionalwhere = "e.status IN (1,2) AND e.energy_accepted > 0")
	} else {
		futile.logger::flog.warn("Type needs to be either 'gen' or 'load'",
				dexpa$db$dbname,
				name = "dexr.hl.costs")
	}
	if (nrow(data) > 0) {
		data <- dexpa$sim$filter$requests(dexpa, data)
		output_figure_energycosts_requested_giniByStartT(dexpa, data)
	} else {
		futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s!",
				dexpa$db$dbname,
				name = "dexr.hl.costs")
	}
}
#' Retrieves requests data from DB and creates figure of gini coefficient of requested energy cost per KWh by delivery start time.
#' 
#' TODO consider meter readings
#' 
#' @param dexpa  
#' @param type either 'gen' or 'load'
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_energycosts_requests_comp_giniByStartT <- function(dexpas, type = "load", skiplegend=F) {
	data = data.frame()
	for (dp in dexpas) {
		# dp = dexpas[[1]]
		if (type == "gen") {
			d <- dexR::input_db_requests(dp, additionalwhere = "e.status IN (1,2) AND e.energy_accepted < 0")
			d$energy_accepted <- d$energy_accepted * (-1)
		} else if (type == "load") {
			d <- dexR::input_db_requests(dp, additionalwhere = "e.status IN (1,2) AND e.energy_accepted > 0")
		} else {
			futile.logger::flog.warn("Type needs to be either 'gen' or 'load'",
					dp$db$dbname,
					name = "dexr.hl.costs")
		}
		
		if (nrow(d) == 0) {
			# R.oo::throw.default("No requests in DB for ID ", dp$id, "!")
			futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s for ID %s!",
					dp$db$dbname,
					dp$id,
					name = "dexr.hl.requests")
		} else {
			d$id <- input_db_runID(dp)
			data <- rbind(data, d)
		}
	}
	if (nrow(data) > 0) {
		data <- dexpas[[1]]$sim$filter$requests(dexpas[[1]], data)
		output_figure_energycosts_requested_comp_giniByStartT(dexpas[[1]], data, skiplegend=skiplegend)
	}
}
#' Retrieves requests data from DB and creates figure of requested energy and associated costs
#' summed by delivery start time.
#' 
#' TODO consider meter readings
#' 
#' @param dexpa parameters
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_energycosts_requested_sumByStartT <- function(dexpa) {
	data <- input_db_requests(dexpa)
	if (nrow(data) > 0) {
		data <- dexpa$sim$filter$requests(dexpa, data)
		output_figure_energycosts_requested_sumByStartT(dexpa, data)
	} else {
		futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s!",
				dexpa$db$dbname,
				name = "dexr.hl.requests")
	}
}
#' Retrieves requests data from DB and creates figure of requested energy by delivery start time.
#' @param dexpa parameter
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_energycosts_requested_comp_sumByStartT <- function(dexpas, skiplegend=F) {
	futile.logger::flog.debug("Figure: Energycosts Summed by delviery start time: retrieve data...", name="dexr.hl.costs")
	data = data.frame()
	for (dp in dexpas) {
		# dp = dexpas[[1]]
		d <- dexR::input_db_requests(dp)
		if (nrow(d) == 0) {
			# R.oo::throw.default("No requests in DB for ID ", dp$id, "!")
			futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s for ID %s!",
					dp$db$dbname,
					dp$id,
					name = "dexr.hl.costs")
		} else {
			d$id <- dexR::input_db_runID(dp)
			data <- rbind(data, d)
		}
	}
	if (nrow(data) > 0) {
		futile.logger::flog.debug("Figure: Energycosts Summed by delviery start time: filter data...", name="dexr.hl.costs")
		data <- dexpa$sim$filter$requests(dexpa, data)
		output_figure_energycosts_requested_comp_sumByStartT(dexpas[[1]], data, skiplegend=skiplegend)
	}
}
#' Retrieves requests data from DB and creates figure of requested energy by delivery start time.
#' @param dexpa parameter
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_energycosts_requested_comp_avgByTypeStartT <- function(dexpas) {
	data = data.frame()
	for (dp in dexpas) {
		# dp = dexpas[[1]]
		d <- input_db_requests(dp)
		if (nrow(d) == 0) {
			# R.oo::throw.default("No requests in DB for ID ", dp$id, "!")
			futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s for ID %s!",
					dp$db$dbname,
					dp$id,
					name = "dexr.hl.costs")
		} else {
			d$id <- input_db_runID(dp)
			data <- rbind(data, d)
		}
	}
	if (nrow(data) > 0) {
		data <- dexpa$sim$filter$requests(dexpa, data)
		output_figure_energycosts_requested_comp_avgByTypeStartT(dexpas[[1]], data)
	}
}
#' Retrieves requests data from DB and creates histogram of prices.
#' @param dexpa parameter
#' @return figure file
#' 
#' @author Sascha Holzhauer
#' @export
hl_figure_prices_comp_histogram <- function(dexpas, skiplegend=F) {
	futile.logger::flog.debug("Figure: prices histogram: request data...", name="dexr.hl.costs.histogram")
	data = data.frame()
	for (dp in dexpas) {
		# dp = dexpas[[1]]
		d <- dexR::input_db_requests(dp)
		if (nrow(d) == 0) {
			futile.logger::flog.warn("No requests retrieved from PostgreSQL database %s for ID %s!",
					dp$db$dbname,
					dp$id,
					name = "dexr.hl.costs")
		} else {
			d$id <- dexR::input_db_runID(dp)
			data <- rbind(data, d)
		}
	}
	if (nrow(data) > 0) {
		futile.logger::flog.debug("Figure: prices histogram: filter data...", name="dexr.hl.costs")
		data <- dexpa$sim$filter$requests(dexpa, data)
		output_figure_prices_comp_histogram(dexpas[[1]], data, skiplegend=skiplegend)
	}
}