% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dexr_helper.R
\name{ms_to_date}
\alias{ms_to_date}
\title{Convert milliseconds to POSIXct}
\usage{
ms_to_date(ms, t0 = "1970-01-01", timezone = "Europe/Berlin")
}
\arguments{
\item{ms}{a numeric vector of milliseconds (big integers of 13 digits)}

\item{t0}{a string of the format "yyyy-mm-dd", specifying the date that corresponds to 0 millisecond}

\item{timezone}{a string specifying a timezone that can be recognized by R}
}
\value{
POSIXct vector representing calendar dates and times
}
\description{
Convert milliseconds to POSIXct
}
\author{
Sascha Holzhauer
}
