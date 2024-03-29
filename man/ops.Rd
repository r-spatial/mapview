% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ops.R
\docType{methods}
\name{ops}
\alias{ops}
\alias{+,mapview,mapview-method}
\alias{+,mapview,ANY-method}
\alias{+,mapview,NULL-method}
\alias{+,mapview,character-method}
\alias{|,mapview,mapview-method}
\alias{|,mapview,NULL-method}
\alias{|,NULL,mapview-method}
\title{mapview + mapview adds data from the second map to the first}
\usage{
\S4method{+}{mapview,mapview}(e1, e2)

\S4method{+}{mapview,ANY}(e1, e2)

\S4method{+}{mapview,NULL}(e1, e2)

\S4method{+}{mapview,character}(e1, e2)

\S4method{|}{mapview,NULL}(e1, e2)

\S4method{|}{NULL,mapview}(e1, e2)
}
\arguments{
\item{e1}{a leaflet or mapview map, or NULL.}

\item{e2}{a leaflet or mapview map, or NULL.}
}
\description{
mapview + mapview adds data from the second map to the first

mapview + data adds spatial data (raster*, sf*, sp*) to a mapview map

mapview + NULL returns the LHS map

[...]

mapview | mapview provides a slider in the middle to compare two maps.

mapview | NULL returns the LHS map

NULL | mapview returns the RHS map
}
\examples{
  m1 <- mapView(franconia, col.regions = "red")
  m2 <- mapView(breweries)

  ### add two mapview objects
  m1 + m2

  ### add layers to a mapview object
  if (interactive()) {
    library(plainview)
    m1 + breweries + plainview::poppendorf[[4]]
  }

  m1 <- mapView(franconia, col.regions = "red")
  m2 <- mapView(breweries)

  ### add two mapview objects
  m1 | m2

}
