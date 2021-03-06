% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OvaryVisFromDims.R
\name{OvaryVisFromDims}
\alias{OvaryVisFromDims}
\title{a simple heuristic to determine ovary visualisation}
\usage{
OvaryVisFromDims(d1, d2, d3)
}
\arguments{
\item{d1}{Numeric dimension D1 in mm}

\item{d2}{Numeric dimension D3 in mm}

\item{d3}{Numeric dimension D3 in mm}
}
\value{
string not visualised or visualised depending upon inputs
}
\description{
a simple heuristic to determine ovary visualisation
}
\examples{
OvaryVisFromDims(20, 12,11)
}
