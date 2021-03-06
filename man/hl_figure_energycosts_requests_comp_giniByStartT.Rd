% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dexr_hl_figure_costs.R
\name{hl_figure_energycosts_requests_comp_giniByStartT}
\alias{hl_figure_energycosts_requests_comp_giniByStartT}
\title{Retrieves requests data from DB and creates figure of gini coefficient of requested energy cost
per KWh by delivery start time.}
\usage{
hl_figure_energycosts_requests_comp_giniByStartT(
  dexpas,
  type = "load",
  skiplegend = F
)
}
\arguments{
\item{type}{either 'gen' or 'load'}

\item{dexpa}{}
}
\value{
figure file
}
\description{
TODO Considers different delivery intervals (converts resolution to longer intervals) for total costs and total energy
TODO consider meter readings
}
\author{
Sascha Holzhauer
}
