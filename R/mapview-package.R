#' Interactive viewing of spatial objects in R
#'
#' The package provides functionality to view spatial objects interactively.
#' The intention is to provide interactivity for easy and quick visualization
#' during spatial data analysis. It is not intended for fine-tuned presentation
#' quality map production.
#'
#' @name mapview-package
#' @docType package
#' @title Interactive viewing of spatial objects in R
#' @author Tim Appelhans, Florian Detsch, Chris Reudenbach,
#' Stephan Woellauer, Spaska Forteva, Thomas Nauss,
#' Environmental Informatics Marburg \cr
#' \cr
#' \emph{Maintainer:} Tim Appelhans \email{admin@@environmentalinformatics-marburg.de}
#'
#' @import leaflet sp raster satellite scales Rcpp methods png lattice rgdal gdalUtils latticeExtra viridisLite
#' @importFrom grDevices rgb
#' @importFrom utils write.table
#' @importFrom webshot webshot
#'
#' @useDynLib mapview
#' @keywords package
#'
NULL
#'
#' @docType data
#' @name atlStorms2005
#' @title Atlantic Ocean storms 2005
#' @description Atlantic Ocean storms 2005
#' @details This dataset contains storm tracks for selected storms
#' in the Atlantic Ocean basin for the year 2005
#' @format \code{sp::SpatialLinesDataFrame}
NULL
#'
#' @docType data
#' @name gadmCHE
#' @title Administrative borders of Switzerland (level 2)
#' @description Administrative borders of Switzerland (level 2)
#' @details This dataset comes from \url{http://gadm.org}.
#' It was downloaded using \code{\link{getData}}.
#' @format \code{sp::SpatialPolygonsDataFrame}
#' @source
#' \url{http://gadm.org}
NULL
#'
#' @docType data
#' @name kiliNDVI
#' @title 16-day NDVI of Kilimanjaro
#' @description 16-day NDVI of Kilimanjaro from January to December 2013
#' @details 16-day Aqua-MODIS NDVI (MYD13Q1, Collection 6) from 2013,
#' quality-controlled and Whittaker-smoothed as described in Detsch et al.
#' (2016; see References).
#' @references Detsch F, Otte I, Appelhans T, Hemp A, Nauss T (2016) Seasonal
#' and long-term vegetation dynamics from 1-km GIMMS-based NDVI time series at
#' Mt. Kilimanjaro, Tanzania. Remote Sensing of Environment 178: 70--83,
#' doi:10.1016/j.rse.2016.03.007
#' @format \code{"RasterStack-class"}
NULL
#'
#' @docType data
#' @name poppendorf
#' @title Landsat 8 detail of Franconian Switzerland centered on Poppendorf
#' @description Landsat 8 detail of Franconian Switzerland centered on Poppendorf
#' @details Use of this data requires your agreement to the USGS regulations on
#' using Landsat data.
#' @format \code{"RasterBrick-class"} with 10 bands (layers 8 and 12 have been dropped).
#' @source
#' \url{http://earthexplorer.usgs.gov/}
NULL
#'
#' @docType data
#' @name breweries91
#' @title Selected breweries in Franconia
#' @description Selected breweries in Franconia (zip code starting with 91...)
#' @details This dataset contains selected breweries in Franconia. It is a
#' subset of a larger database that was compiled by students at the
#' University of Marburg for a seminar called
#' "The Geography of Beer, sustainability in the food industry"
#' @format \code{sp::SpatialPointsDataFrame}
NULL
#'
#' @docType data
#' @name campsQ2
#' @title Antarctic Facilities
#' @description Antarctic facilities. Type of facility attribute is ""feature_ty"
#' @details The data show the Antarctic facilities (2012) as provided by \href{https://www.comnap.aq}{comnap} via the \href{www.quantarctica.org}{Quantarctica2} data compilation that is compiled by the \href{http://www.npolar.no/en}{Norwegian Polar Institute}
#' @format \code{sp::SpatialPointsDataFrame}
NULL
#'
#' @docType data
#' @name roadsGRL
#' @title roads data from Greenland as provided by geofabrik.de
#' @description Roads snapshot March(2016) from the OSM data extract as provided by geofabrik.de
#' @details The data show the roads (snapshot 03/2013) as downloaded by \href{http://download.geofabrik.de/north-america/greenland-latest.shp.zip}{geofabrik}. The geofabrik extracts are based on the OpenStreetMap \href{http://www.openstreetmap.org/}{OSM} data. For further information see also \href{http://download.geofabrik.de/north-america/greenland.html}{Geofabrik Downloads}
#' @format \code{sp::SpatialPointsDataFrame}
NULL
