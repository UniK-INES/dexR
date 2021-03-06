% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dexr_output_figure.R
\name{output_figure_bars}
\alias{output_figure_bars}
\title{Prints a bar of data as ggplot2 with potentially different colour, linetype,
and potentially as facet plot.}
\usage{
output_figure_bars(
  dexpa,
  data,
  y_column,
  title = NULL,
  fill_column = NULL,
  fill_legendtitle = fill_column,
  fill_legenditemnames = NULL,
  facet_column = NULL,
  facet_ncol = 4,
  filename = paste(dexpa$fig$filenameprefix, gsub(" ", "_", title),
    shbasic::shbasic_condenseRunids(data[, "id"]), dexpa$fig$filenamepostfix, sep = "_"),
  alpha = 1,
  ggplotaddons = NULL,
  x_column = "ID",
  group_column = NULL,
  group_colors = NULL,
  position = "dodge",
  returnplot = FALSE
)
}
\arguments{
\item{dexpa}{parameter object}

\item{data}{data.frame or list that is rbinded to a data.frame}

\item{y_column}{column name for y data}

\item{title}{figure title}

\item{fill_column}{column used to define colours}

\item{fill_legendtitle}{title for colour legend}

\item{fill_legenditemnames}{vector of names for colour legend items}

\item{facet_column}{column used to define facets}

\item{facet_ncol}{number of columns of facet wrap}

\item{filename}{without extension}

\item{alpha}{transparency}

\item{ggplotaddons}{vector of ggplot objects to add}

\item{x_column}{column name for x data}

\item{position}{passed to geom_bar}

\item{returnplot}{if true the ggplot object is returned}
}
\value{
ggplot2 line visualisation
}
\description{
Prints a bar of data as ggplot2 with potentially different colour, linetype,
and potentially as facet plot.
}
\examples{
print("todo")
}
\author{
Sascha Holzhauer
}
