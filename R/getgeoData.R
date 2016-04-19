if (!isGeneric('getGeoData')) {
  setGeneric('getGeoData', function(x, ...)
    standardGeneric('getGeoData'))
}
#'@name getGeoData
#'
#'@title Retrieves online geodata and converts it to raster/sp objects
#'
#'@description Robert J. Hijmans getData() from the raster package is well known and highly used. The only disadvantage is that it currently doesn't support a bunch of great additional and/or improved/newer data sets.  getGeoData provides some more actual or better choices for climate and DEM data as well as some easy to use interfaces to OSM and other crowd sourced data compilations.
#' The main issue of the functionis to offer an easy to use access to a wider range of free to access data sets that may improve significantly the quality of typical ecological and other spatial analysis approaches by an straightforward utilization of data.
#' You may download the data individually but by default all data will be downloaded georeferenced and converted in \link{raster} or \link{sp} objects.
#'
#'@usage getGeoData(name, download=TRUE, path='', ...)
#' ccodes()
#'
#'@param name Data set name, currently supported are:
#'\code{GADM}, \cr
#'\code{countries}, \cr
#'\code{SRTM}, \cr
#'\code{alt}, \cr
#'\code{worldclim},\cr
#'\code{schmatzPangea},\cr
#'\code{harrylist}, \cr
#'\code{OSM}, \cr
#'\code{tiroldem}.\cr
#'See Details for more info
#'@param download Logical \code{TRUE} data will be downloaded if not locally available
#'@param path Character Path name indicating where to store the data. Default is the current working directory
#'@param ... Additional required (!) parameters. These are data set specific. See Details
#'@author Robert J. Hijmans, Chris Reudenbach \email{giswerk@@gis-ma.org}
#'
#'@return A spatial object (Raster* or Spatial*)
#'
#'@details
#'
#' \code{GADM} is a database of global administrative boundaries. \cr
#' \code{alt} stands for altitude (elevation); the data were aggregated from SRTM 90 m resolution data between -60 and 60 latitude. \cr
#' \code{countries} has polygons for all countries at a higher resolution than the 'wrld_simpl' data\cr in the maptools pacakge . \cr
#'If  \code{name}='alt' or \code{name}='GADM' you must provide a 'country=' argument. Countries are specified by their 3 letter ISO codes. Use getData('ISO3') to see these codes. In the case of GADM you must also provide the level of administrative subdivision (0=country, 1=first level subdivision). In the case of alt you can set 'mask' to FALSE. If it is TRUE values for neighbouring countries are set to NA. For example:\cr \cr
#'     \code{getGeoData('GADM', country='FRA', level=1)}\cr
#'     \code{getGeoData('alt', country='FRA', mask=TRUE)}\cr
#' \cr
#' \code{SRTM} refers to the 4.1 version of the CGIAR-SRTM (90 m resolution). \cr
#'If  \code{name}='SRTM' you must provide at least the extent of an area as argument (minlong,minlat,maxlong,maxlat).
#'#' \cr
#'
#'If \code{name}=CMIP5 for (projected) future climate data you must provide arguments var and res as above. Only resolutions 2.5, 5, and 10 are currently available. In addition, you need to provide model, rcp and year.
#'For example:\cr
#'   \code{getGeoData('CMIP5', var='tmin', res=10, rcp=85, model='AC', year=70)}\cr
#'   function (var, model, rcp, year, res, lon, lat, path, download = TRUE)\cr
#'   'model' should be one of "AC", "BC", "CC", "CE", "CN", "GF", "GD", "GS", "HD", "HG", "HE", "IN", "IP", "MI", "MR", "MC", "MP", "MG", or "NO".\cr
#'   'rcp' should be one of 26, 45, 60, or 85.\cr
#'   'year' should be 50 or 70\cr
#' Not all combinations are available. See www.worldclim.org for details.\cr
#' \cr
#'
#' \code{worldclim} is a database of global interpolated climate data. \cr
#'If  \code{name}='worldclim' you must also provide a variable name 'var=', and a resolution 'res='. Valid variables names are 'tmin', 'tmax', 'prec' and 'bio'. Valid resolutions are 0.5, 2.5, 5, and 10 (minutes of a degree). In the case of res=0.5, you must also provide a lon and lat argument for a tile; for the lower resolutions global data will be downloaded. In all cases there are 12 (monthly) files for each variable except for 'bio' which contains 19 files.\cr
#'    \code{getGeoData('worldclim', var='tmin', res=0.5, lon=5, lat=45)} \cr
#'    \code{getGeoData('worldclim', var='bio', res=10)}\cr\cr
#'
#'  +++ additional datasets +++ \cr\cr
#'
#' \code{schmatzPangea} provides the Gridded climate data from 5 Global Climate Models (GCM) of the Last Glacial Maximum (LGM) downscaled to 30 arc seconds for Europe \url{http://doi.pangaea.de/10.1594/PANGAEA.845883}\cr
#'If  \code{name}='schmatzPangea' you have to specify the item of interest. Please note: The data download may take a long time!\cr
#'The list of allowd items is: \cr
#'   \itemize{
#'\item \code{prec_eu_wc_30s} baseline climate	precipitation, Worldclim LGM coastline, current, 30x30sec	,	http://hs.pangaea.de/model/schmatz/prec_eu_wc_30s
#'\item \code{tave_eu_wcpi_30s} baseline climate	average surface air temperature, Worldclim LGM coastline, preindustrial, 30x30sec	,	http://hs.pangaea.de/model/schmatz/tave_eu_wcpi_30s
#'\item \code{tmax_eu_wcpi_30s} baseline climate	maximum surface air temperature, Worldclim LGM coastline, preindustrial, 30x30sec	,	http://hs.pangaea.de/model/schmatz/tmax_eu_wcpi_30s
#'\item \code{tmin_eu_wcpi_30s} baseline climate	minimum surface air temperature, Worldclim LGM coastline, preindustrial, 30x30sec	,	http://hs.pangaea.de/model/schmatz/tmin_eu_wcpi_30s
#'\itemize{
#'\item \code{prec_*,tave_*,tmax_*,tmin_*}: \code{startTime = 1}  is equivalent to LGM average Januar  \code{endTime = 12}  is equivalent to LGM average december
#'}
#'\item \code{bioclim_A_MO_pmip2_21k_oa_CCSM_eu_30s} bioclimatic variables	19 bioclimatic variables, CCSM, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/bioclim_A_MO_pmip2_21k_oa_CCSM_eu_30s
#'\item \code{bioclim_A_MO_pmip2_21k_oa_CNRM_eu_30s} bioclimatic variables	19 bioclimatic variables, CNRM, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/bioclim_A_MO_pmip2_21k_oa_CNRM_eu_30s
#'\item \code{bioclim_A_MO_pmip2_21k_oa_FGOALS_eu_30s} bioclimatic variables	19 bioclimatic variables, FGOALS, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/bioclim_A_MO_pmip2_21k_oa_FGOALS_eu_30s
#'\item \code{bioclim_A_MO_pmip2_21k_oa_IPSL_eu_30s} bioclimatic variables	19 bioclimatic variables, IPSL, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/bioclim_A_MO_pmip2_21k_oa_IPSL_eu_30s
#'\item \code{bioclim_A_MO_pmip2_21k_oa_MIROC3.2_eu_30s} bioclimatic variables	19 bioclimatic variables, MIROC3.2, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/bioclim_A_MO_pmip2_21k_oa_MIROC3.2_eu_30s
#' \itemize{
#' \item \code{bioclim_*}: \code{startTime = 1}  is equivalent to LGM average year \code{endTime = 1}  is equvalent to LGM average year
#' #' \itemize{
#' \item \code{bio_1} Annual Mean Temperature ; units = "C*10" ; FillValue = -9999 ; missing_value = -9999
#' \item \code{bio_2} Mean Diurnal Range (Mean(period max-min))" ; units = "C*10" ; FillValue = -9999 ; missing_value = -9999
#' \item \code{bio_3} Isothermality (P2 / P7)" ; units = "C*10" ; FillValue = -9999 ; missing_value = -9999
#' \item \code{bio_4} Temperature Seasonality (standard deviation)" ; units = "C*10" ; FillValue = -9999 ; missing_value = -9999
#' \item \code{bio_5} Max Temperature of Warmest Period" ; units = "C*10" ; FillValue = -9999 ; missing_value = -9999
#' \item \code{bio_6} Min Temperature of Coldest Period" ; units = "C*10" ; FillValue = -9999 ; missing_value = -9999
#' \item \code{bio_7} Temperature Annual Range (P5-P6)" ; units = "C*10" ; FillValue = -9999 ; missing_value = -9999
#' \item \code{bio_8} Mean Temperature of Wettest Quarter" ; units = "C*10" ; FillValue = -9999 ; missing_value = -9999
#' \item \code{bio_9} Mean Temperature of Driest Quarter" ; units = "C*10" ; FillValue = -9999 ; missing_value = -9999
#' \item \code{bio_10} Mean Temperature of Warmest Quarter" ; units = "C*10" ; FillValue = -9999 ; missing_value = -9999
#' \item \code{bio_11} Mean Temperature of Coldest Quarter" ; units = "C*10" ; FillValue = -9999 ; missing_value = -9999
#' \item \code{bio_12} Annual Precipitation" ; units = "mm" ; FillValue = -9999 ; missing_value = -9999
#' \item \code{bio_13} Precipitation of Wettest Period" ; units = "mm" ; FillValue = -9999 ; missing_value = -9999
#' \item \code{bio_14} Precipitation of Driest Period" ; units = "mm" ; FillValue = -9999 ; missing_value = -9999
#' \item \code{bio_15} Precipitation Seasonality (Coefficient of Variation)" ; units = "mm" ; FillValue = -9999 ; missing_value = -9999
#' \item \code{bio_16} Precipitation of Wettest Quarter" ; units = "mm" ; FillValue = -9999 ; missing_value = -9999
#' \item \code{bio_17} Precipitation of Driest Quarter" ; units = "mm" ; FillValue = -9999 ; missing_value = -9999
#' \item \code{bio_18} Precipitation of Warmest Quarter" ; units = "mm" ; FillValue = -9999 ; missing_value = -9999
#' \item \code{bio_19} Precipitation of Coldest Quarter" ; units = "mm" ; FillValue = -9999 ; missing_value = -9999		#' }

#' }
#'\item \code{pr_A_MO_pmip2_21k_oa_CCSM_eu_30s} downscaled GCM	precipitation, CCSM, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/pr_A_MO_pmip2_21k_oa_CCSM_eu_30s
#'\item \code{pr_A_MO_pmip2_21k_oa_CNRM_eu_30s} downscaled GCM	precipitation, CNRM, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/pr_A_MO_pmip2_21k_oa_CNRM_eu_30s
#'\item \code{pr_A_MO_pmip2_21k_oa_FGOALS_eu_30s} downscaled GCM	precipitation, FGOALS, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/pr_A_MO_pmip2_21k_oa_FGOALS_eu_30s
#'\item \code{pr_A_MO_pmip2_21k_oa_IPSL_eu_30s} downscaled GCM	precipitation, IPSL, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/pr_A_MO_pmip2_21k_oa_IPSL_eu_30s
#'\item \code{pr_A_MO_pmip2_21k_oa_MIROC3.2_eu_30s} downscaled GCM	precipitation, MIROC3.2, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/pr_A_MO_pmip2_21k_oa_MIROC3.2_eu_30s
#'\item \code{tas_A_MO_pmip2_21k_oa_CCSM_eu_30s} downscaled GCM	average surface air temperature, CCSM, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/tas_A_MO_pmip2_21k_oa_CCSM_eu_30s
#'\item \code{tas_A_MO_pmip2_21k_oa_CNRM_eu_30s} downscaled GCM	average surface air temperature, CNRM, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/tas_A_MO_pmip2_21k_oa_CNRM_eu_30s
#'\item \code{tas_A_MO_pmip2_21k_oa_FGOALS_eu_30s} downscaled GCM	average surface air temperature, FGOALS, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/tas_A_MO_pmip2_21k_oa_FGOALS_eu_30s
#'\item \code{tas_A_MO_pmip2_21k_oa_IPSL_eu_30s} downscaled GCM	average surface air temperature, IPSL, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/tas_A_MO_pmip2_21k_oa_IPSL_eu_30s
#'\item \code{tas_A_MO_pmip2_21k_oa_MIROC3.2_eu_30s} downscaled GCM	average surface air temperature, MIROC3.2, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/tas_A_MO_pmip2_21k_oa_MIROC3.2_eu_30s
#'\item \code{tasmax_A_MO_pmip2_21k_oa_CCSM_eu_30s} downscaled GCM	maximum surface air temperature, CCSM, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/tasmax_A_MO_pmip2_21k_oa_CCSM_eu_30s
#'\item \code{tasmax_A_MO_pmip2_21k_oa_CNRM_eu_30s} downscaled GCM	maximum surface air temperature, CNRM, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/tasmax_A_MO_pmip2_21k_oa_CNRM_eu_30s
#'\item \code{tasmax_A_MO_pmip2_21k_oa_FGOALS_eu_30s} downscaled GCM	maximum surface air temperature, FGOALS, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/tasmax_A_MO_pmip2_21k_oa_FGOALS_eu_30s
#'\item \code{tasmax_A_MO_pmip2_21k_oa_IPSL_eu_30s} downscaled GCM	maximum surface air temperature, IPSL, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/tasmax_A_MO_pmip2_21k_oa_IPSL_eu_30s
#'\item \code{tasmax_A_MO_pmip2_21k_oa_MIROC3.2_eu_30s} downscaled GCM	maximum surface air temperature, MIROC3.2, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/tasmax_A_MO_pmip2_21k_oa_MIROC3.2_eu_30s
#'\item \code{tasmin_A_MO_pmip2_21k_oa_CCSM_eu_30s} downscaled GCM	minimum surface air temperature, CCSM, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/tasmin_A_MO_pmip2_21k_oa_CCSM_eu_30s
#'\item \code{tasmin_A_MO_pmip2_21k_oa_CNRM_eu_30s} downscaled GCM	minimum surface air temperature, CNRM, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/tasmin_A_MO_pmip2_21k_oa_CNRM_eu_30s
#'\item \code{tasmin_A_MO_pmip2_21k_oa_FGOALS_eu_30s} downscaled GCM	minimum surface air temperature, FGOALS, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/tasmin_A_MO_pmip2_21k_oa_FGOALS_eu_30s
#'\item \code{tasmin_A_MO_pmip2_21k_oa_IPSL_eu_30s} downscaled GCM	minimum surface air temperature, IPSL, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/tasmin_A_MO_pmip2_21k_oa_IPSL_eu_30s
#'\item \code{tasmin_A_MO_pmip2_21k_oa_MIROC3.2_eu_30s} downscaled GCM	minimum surface air temperature, MIROC3.2, LGM, 30x30sec	,	http://hs.pangaea.de/model/schmatz/tasmin_A_MO_pmip2_21k_oa_MIROC3.2_eu_30s
#'\itemize{
#'\item \code{pre_*,tas_*,tasmax_*,tasmin_*}: \code{startTime = 1}  is equivalent to LGM average Januar  \code{endTime = 12}  is equivalent to LGM average december
#'}
#'\item \code{TT_Luterbacher_Xoplaki_1659-1998} reconstructed climate average surface air temperature, reconstructed+CRU, 1659-1998, 0.5x0.5 deg	,	http://hs.pangaea.de/model/schmatz/TT_Luterbacher_Xoplaki_1659-1998
#'#'   \itemize{
#'\item \code{TT}: \code{startTime = 1}  is equivalent to the Januar 1659 \code{endTime = 4080}  is equvalent to December 1998
#'}
#'}
#'\code{m<-getGeoData('schmatzPangea', item="tasmax_A_MO_pmip2_21k_oa_CCSM_eu_30s",startTime=1,endTime=3)}
#'\code{m<-getGeoData('schmatzPangea', item="bioclim_A_MO_pmip2_21k_oa_CCSM_eu_30s",data="bio_1")}
#'\code{TT<- getGeoData('schmaztLGMData', item='TT_Luterbacher_Xoplaki_1659-1998')}
#'
#' \code{harrylist} is a list of world wide about 60.000 coordinates altitudes and names of summits \link{PeakList}\cr
#'If  \code{name=}'harrylist' you will download and clean the complete list\cr
#'    \code{getGeoData('harrylist')}\cr \cr
#'
#' \code{OSMp} is the OSM Point Data from the current OSM database\cr
#'If  \code{name}='OSMp' you must provide lat_min,lat_max,lon_min,lon_max for the boundig box. Additionally you must set  the switch 'all' to \code{FALSE} if you just want to download a specified item. Then you have to  provide the content of the desired items in the 'key' and 'val' argument. According to this combination you have to provide a tag list containing the Tags of the element c('name','ele').\cr\cr
#'    \code{getGeoData('OSMp', extent=c(11.35547,11.40009,47.10114,47.13512), key='natural',val='peak',taglist=c('name','ele'))}\cr \cr
#'
#' \code{tiroldem} refers to the 10 m Lidar based DEM as provided by the Authorithy of Tirol. For Copyright and further information  see: \link{DEM}\cr \cr
#'If  \code{name}='tiroldem' you must set the switch 'all' to \code{FALSE} if you just want to download a specified item you have to set data=item.
#'The list of allowd items is:
#'\itemize{
#'\item \code{IBK_DGM10} Innsbruck,
#'\item \code{IL_DGM10} Innsbruck Land,
#'\item \code{IM_DGM10} Imst,
#'\item \code{KB_DGM10} Kitzbuehl,
#'\item \code{KU_DGM10} Kufstein,
#'\item \code{LA_DGM10} Landeck,
#'\item \code{RE_DGM10} Reutte,
#'\item \code{SZ_DGM10} Schwaz,
#'\item \code{LZ_DGM10} Lienz (Osttirol).
#'}
#'For use in ArcGIS the data is correctly georeferenced. However for R you MUST use the following proj4 strings if you want to project other data acccording to the Austrian Datum. DO NOT USE the default EPSG Code string! All datasets except Lienz are projected with: ''+proj=tmerc +lat_0=0 +lon_0=10.33333333333333 +k=1 +x_0=0 +y_0=-5000000 +ellps=bessel +towgs84=577.326, 90.129, 463.919, 5.137, 1.474, 5.297, 2.4232 +units=m'. Item=lz_10m (Lienz) has an different Central_Meridian. You have to change it to 13.333333.\cr
#'
#'\code{getGeoData('tiroldem', item = 'KU_DGM10')} \cr
#'

#'@references
#'\url{http://www.worldclim.org}\cr
#'\url{http://www.gadm.org}\cr
#'\url{http://srtm.csi.cgiar.org/}\cr
#'\url{http://diva-gis.org/gdata}\cr
#'\url{http://www.tourenwelt.info}\cr
#'\url{https://www.tirol.gv.at/data/datenkatalog/geographie-und-planung/digitales-gelaendemodell-tirol/}\cr
#'\url{http://www.openstreetmap.org}\cr
#'\url{http://doi.pangaea.de/10.1594/PANGAEA.845883}\cr
#'


#'@export getGeoData
#'
#'@examples
#'#### Examples getGeoData
#'
#' \dontrun{
#' ## get SRTM data at a position
#' r<-getGeoData(name="SRTM",xtent = extent(11.,11.,50.,50.))
#' ## get SRTM data for an area
#' r<-getGeoData(name="SRTM",xtent = extent(11.,17.,50.,56.))
#' ## get SRTM data for an area with a buffer zone (e.g. for a cost or watershed analysis) zone is in degree
#' r<-getGeoData(name="SRTM",xtent = extent(11.,17.,50.,56.), zone = 3.0)
#' ## get SRTM Tile names
#' t<-getGeoData(name="SRTM",xtent = extent(11.,17.,50.,56.), zone = 3.0, download = FALSE)
#' ## get SRTM data for an area with a buffer and merge it
#' r<-getGeoData(name="SRTM",xtent = extent(11.,17.,50.,56.), zone = 3.0, merge = TRUE)
#'
#' ## get Schmatz et al. data please have a look at details
#' r<- getGeoData('schmatzPangea', item='tasmin_A_MO_pmip2_21k_oa_CNRM_eu_30s',endTime=12)
#' r<- getGeoData('schmatzPangea', item="bioclim_A_MO_pmip2_21k_oa_CCSM_eu_30s", layer="bio_1")
#'
#' ## get a single tile of the Tirolean DEM
#' r<- getGeoData('tiroldem', items='IBK_DGM10')
#' ## get a single 3 tiles of the Tirolean DEM as a merged raster
#' r<- getGeoData('tiroldem', item=c('IBK_DGM10','IL_DGM10','IM_DGM10'), merge =TRUE)
#'
#' # get arbitrary OSM point data
#' r<- getGeoData('OSMp', extent=c(11.35547,11.40009,47.10114,47.13512), key='natural',val='saddle',taglist=c('name','ele','direction'))
#'
#' # get Harald Breitkreutz' summit list
#' r<- getGeoData('harrylist', extent=c(11.35547,11.40009,47.10114,47.13512))
#'
#' ### the following datasets are retrieved according to Hijmans \code{getData}
#' r<- getGeoData('worldclim', var='tmin', res=0.5, lon=5, lat=45)
#' r<- getGeoData('worldclim', var='bio', res=10)
#' r<- getGeoData('CMIP5', var='tmin', res=10, rcp=85, model='AC', year=70)
#' v<- getGeoData('alt', country='FRA', mask=TRUE)
#' v<- getGeoData('GADM', country='FRA', level=1)
#' t<- ccodes()
#' }


getGeoData <- function(name='GADM', download=TRUE, path='', ...) {
  library(raster)
  library(osmar)
  library(sp)
  library(maptools)
  library(curl)
  library(doParallel)
  library(foreach)
  library(gdalUtils)
  path <- .getDataPath(path)
  if (name=='GADM') {
    .GADM(..., download=download, path=path)
  } else if (name=='SRTM') {
    .SRTM(..., download=download, path=path)
  } else if (name=='harrylist') {
    .harrylist(..., download=download, path=path)
  } else if (name=='tiroldem') {
    .tiroldem(..., download=download, path=path)
  } else if (name=='OSMp') {
    .OSMp(..., download=download, path=path)
  } else if (name=='alt') {
    .raster(..., name=name, download=download, path=path)
  } else if (name=='schmatzPangea') {
    .schmatz(..., download=download, path=path)
  } else if (name=='worldclim') {
    .worldclim(..., download=download, path=path)
  } else if (name=='CMIP5') {
    .cmip5(..., download=download, path=path)
  } else if (name=='ISO3') {
    ccodes()[,c(2,1)]
  } else if (name=='countries') {
    .countries(download=download, path=path, ...)
  } else {
    stop(name, ' not recognized as a valid name.')
  }
}


.download <- function(aurl, filename) {
  fn <- paste(tempfile(), '.download', sep='')
  res <- curl_download(url=aurl, destfile=fn,  quiet = TRUE, mode = "wb")
  #res <- curl_download(url=aurl, destfile=filename,  quiet = FALSE, mode = "wb")
  if (basename(res) != basename(filename)) {
    w <- getOption('warn')
    on.exit(options('warn' = w))
    options('warn'=-1)
    if (!file.rename(fn, filename) ) {
      if (!file.exists(file.path(dirname(filename)))){
        dir.create(file.path(dirname(filename)),recursive = TRUE)
      }
      # rename failed, perhaps because fn and filename refer to different devices
      file.copy(fn, filename)
      #file.remove(fn)
    }
  } else {
    stop('could not download the file' )
  }
}

.ISO <- function() {
  ccodes()
}

ccodes <- function() {
  path <- paste(system.file(package="raster"), "/external", sep='')
  d <- read.csv(paste(path, "/countries.csv", sep=""), stringsAsFactors=FALSE, encoding="UTF-8")
  return(as.matrix(d))
}


.getCountry <- function(country='') {
  country <- toupper(trim(country[1]))
  #  if (nchar(country) < 3) {
  #  	stop('provide a 3 letter ISO country code')
  #	}
  cs <- ccodes()
  try (cs <- toupper(cs))

  iso3 <- substr(toupper(country), 1, 3)
  if (iso3 %in% cs[,2]) {
    return(iso3)
  } else {
    iso2 <- substr(toupper(country), 1, 3)
    if (iso2 %in% cs[,3]) {
      i <- which(country==cs[,3])
      return( cs[i,2] )
    } else if (country %in% cs[,1]) {
      i <- which(country==cs[,1])
      return( cs[i,2] )
    } else {
      stop('provide a valid name or 3 letter ISO country code; you can get a list with: getData("ISO3")')
    }
  }
}


.getDataPath <- function(path) {
  path <- trim(path)
  if (path=='') {
    path <- .dataloc()
  } else {
    if (substr(path, nchar(path)-1, nchar(path)) == '//' ) {
      p <- substr(path, 1, nchar(path)-2)
    } else if (substr(path, nchar(path), nchar(path)) == '/'  | substr(path, nchar(path), nchar(path)) == '\\') {
      p <- substr(path, 1, nchar(path)-1)
    } else {
      p <- path
    }
    if (!file.exists(p) & !file.exists(path)) {
      stop('path does not exist: ', path)
    }
  }
  if (substr(path, nchar(path), nchar(path)) != '/' & substr(path, nchar(path), nchar(path)) != '\\') {
    path <- paste(path, "/", sep="")
  }
  return(path)
}


.GADM <- function(country, level, download, path) {
  #	if (!file.exists(path)) {  dir.create(path, recursive=T)  }

  country <- .getCountry(country)
  if (missing(level)) {
    stop('provide a "level=" argument; levels can be 0, 1, or 2 for most countries, and higer for some')
  }

  filename <- paste(path, country, '_adm', level, ".RData", sep="")
  if (!file.exists(filename)) {
    if (download) {
      theurl <- paste("http://biogeo.ucdavis.edu/data/gadm2/R/", country, '_adm', level, ".RData", sep="")
      .download(theurl, filename)
      if (!file.exists(filename))	{
        cat("\nCould not download file -- perhaps it does not exist \n")
      }
    } else {
      cat("\nFile not available locally. Use 'download = TRUE'\n")
    }
  }
  if (file.exists(filename)) {
    thisenvir = new.env()
    data <- get(load(filename, thisenvir), thisenvir)
    return(data)
  }
}




.countries <- function(download, path, ...) {
  #	if (!file.exists(path)) {  dir.create(path, recursive=T)  }
  filename <- paste(path, 'countries.RData', sep="")
  if (!file.exists(filename)) {
    if (download) {
      theurl <- paste("http://biogeo.ucdavis.edu/data/diva/misc/countries.RData", sep="")
      .download(theurl, filename)
      if (!file.exists(filename)) {
        cat("\nCould not download file -- perhaps it does not exist \n")
      }
    } else {
      cat("\nFile not available locally. Use 'download = TRUE'\n")
    }
  }
  if (file.exists(filename)) {
    thisenvir = new.env()
    data <- get(load(filename, thisenvir), thisenvir)
    return(data)
  }
}


.cmip5 <- function(var, model, rcp, year, res, lon, lat, path, download=TRUE) {
  if (!res %in% c(2.5, 5, 10)) {
    stop('resolution should be one of: 2.5, 5, 10')
  }
  if (res==2.5) { res <- '2-5' }
  var <- tolower(var[1])
  vars <- c('tmin', 'tmax', 'prec', 'bio')
  stopifnot(var %in% vars)
  var <- c('tn', 'tx', 'pr', 'bi')[match(var, vars)]

  model <- toupper(model)
  models <- c('AC', 'BC', 'CC', 'CE', 'CN', 'GF', 'GD', 'GS', 'HD', 'HG', 'HE', 'IN', 'IP', 'MI', 'MR', 'MC', 'MP', 'MG', 'NO')
  stopifnot(model %in% models)

  rcps <- c(26, 45, 60, 85)
  stopifnot(rcp %in% rcps)
  stopifnot(year %in% c(50, 70))

  m <- matrix(c(0,1,1,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,0,1,0,1,1,1,0,0,1,1,1,1,0,1,1,1,1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1), ncol=4)
  i <- m[which(model==models), which(rcp==rcps)]
  if (!i) {
    warning('this combination of rcp and model is not available')
    return(invisible(NULL))
  }

  path <- paste(path, '/cmip5/', res, 'm/', sep='')
  dir.create(path, recursive=TRUE, showWarnings=FALSE)

  zip <- tolower(paste(model, rcp, var, year, '.zip', sep=''))
  theurl <- paste('http://biogeo.ucdavis.edu/data/climate/cmip5/', res, 'm/', zip, sep='')

  zipfile <- paste(path, zip, sep='')
  if (var == 'bi') {
    n <- 19
  } else {
    n <- 12
  }
  tifs <- paste(extension(zip, ''), 1:n, '.tif', sep='')
  files <- paste(path, tifs, sep='')
  fc <- sum(file.exists(files))
  if (fc < n) {
    if (!file.exists(zipfile)) {
      if (download) {
        .download(theurl, zipfile)
        if (!file.exists(zipfile))	{
          cat("\n Could not download file -- perhaps it does not exist \n")
        }
      } else {
        cat("\nFile not available locally. Use 'download = TRUE'\n")
      }
    }
    unzip(zipfile, exdir=dirname(zipfile))
  }
  stack(paste(path, tifs, sep=''))
}

#.cmip5(var='prec', model='BC', rcp=26, year=50, res=10, path=getwd())


.worldclim <- function(var, res, lon, lat, path, download=TRUE) {
  if (!res %in% c(0.5, 2.5, 5, 10)) {
    stop('resolution should be one of: 0.5, 2.5, 5, 10')
  }
  if (res==2.5) { res <- '2-5' }
  stopifnot(var %in% c('tmean', 'tmin', 'tmax', 'prec', 'bio', 'alt'))
  path <- paste(path, 'wc', res, '/', sep='')
  dir.create(path, showWarnings=FALSE)

  if (res==0.5) {
    lon <- min(180, max(-180, lon))
    lat <- min(90, max(-60, lat))
    rs <- raster(nrows=5, ncols=12, xmn=-180, xmx=180, ymn=-60, ymx=90 )
    row <- rowFromY(rs, lat) - 1
    col <- colFromX(rs, lon) - 1
    rc <- paste(row, col, sep='')
    zip <- paste(var, '_', rc, '.zip', sep='')
    zipfile <- paste(path, zip, sep='')
    if (var  == 'alt') {
      bilfiles <- paste(var, '_', rc, '.bil', sep='')
      hdrfiles <- paste(var, '_', rc, '.hdr', sep='')
    } else if (var  != 'bio') {
      bilfiles <- paste(var, 1:12, '_', rc, '.bil', sep='')
      hdrfiles <- paste(var, 1:12, '_', rc, '.hdr', sep='')
    } else {
      bilfiles <- paste(var, 1:19, '_', rc, '.bil', sep='')
      hdrfiles <- paste(var, 1:19, '_', rc, '.hdr', sep='')
    }
    theurl <- paste('http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/tiles/cur/', zip, sep='')
  } else {
    zip <- paste(var, '_', res, 'm_bil.zip', sep='')
    zipfile <- paste(path, zip, sep='')
    if (var  == 'alt') {
      bilfiles <- paste(var, '.bil', sep='')
      hdrfiles <- paste(var, '.hdr', sep='')
    } else if (var  != 'bio') {
      bilfiles <- paste(var, 1:12, '.bil', sep='')
      hdrfiles <- paste(var, 1:12, '.hdr', sep='')
    } else {
      bilfiles <- paste(var, 1:19, '.bil', sep='')
      hdrfiles <- paste(var, 1:19, '.hdr', sep='')
    }
    theurl <- paste('http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/', zip, sep='')
  }
  files <- c(paste(path, bilfiles, sep=''), paste(path, hdrfiles, sep=''))
  fc <- sum(file.exists(files))
  if (fc < 24) {
    if (!file.exists(zipfile)) {
      if (download) {
        .download(theurl, zipfile)
        if (!file.exists(zipfile))	{
          cat("\n Could not download file -- perhaps it does not exist \n")
        }
      } else {
        cat("\nFile not available locally. Use 'download = TRUE'\n")
      }
    }
    unzip(zipfile, exdir=dirname(zipfile))
    for (h in paste(path, hdrfiles, sep='')) {
      x <- readLines(h)
      x <- c(x[1:14], 'PIXELTYPE     SIGNEDINT', x[15:length(x)])
      writeLines(x, h)
    }
  }
  if (var  == 'alt') {
    st <- raster(paste(path, bilfiles, sep=''))
  } else {
    st <- stack(paste(path, bilfiles, sep=''))
  }
  projection(st) <- "+proj=longlat +datum=WGS84"
  return(st)
}



.raster <- function(country, name, mask=TRUE, path, download, keepzip=FALSE, ...) {

  country <- .getCountry(country)
  path <- .getDataPath(path)
  if (mask) {
    mskname <- '_msk_'
    mskpath <- 'msk_'
  } else {
    mskname<-'_'
    mskpath <- ''
  }
  filename <- paste(path, country, mskname, name, ".grd", sep="")
  if (!file.exists(filename)) {
    zipfilename <- filename
    extension(zipfilename) <- '.zip'
    if (!file.exists(zipfilename)) {
      if (download) {
        theurl <- paste("http://biogeo.ucdavis.edu/data/diva/", mskpath, name, "/", country, mskname, name, ".zip", sep="")
        .download(theurl, zipfilename)
        if (!file.exists(zipfilename))	{
          cat("\nCould not download file -- perhaps it does not exist \n")
        }
      } else {
        cat("\nFile not available locally. Use 'download = TRUE'\n")
      }
    }
    ff <- unzip(zipfilename, exdir=dirname(zipfilename))
    if (!keepzip) {
      file.remove(zipfilename)
    }
  }
  if (file.exists(filename)) {
    rs <- raster(filename)
  } else {
    #patrn <- paste(country, '.', mskname, name, ".grd", sep="")
    #f <- list.files(path, pattern=patrn)
    f <- ff[substr(ff, nchar(ff)-3, nchar(ff)) == '.grd']
    if (length(f)==0) {
      warning('something went wrong')
      return(NULL)
    } else if (length(f)==1) {
      rs <- raster(f)
    } else {
      rs <- sapply(f, raster)
      cat('returning a list of RasterLayer objects\n')
      return(rs)
    }
  }
  projection(rs) <- "+proj=longlat +datum=WGS84"
  return(rs)
}

.getSRTMfn <- function(xtent,zone=0){
  srtmNames<-NULL
  x<-extent(xtent)
  lon<- floor(x@xmin-zone)
  lat<-floor(x@ymin-zone)
  lonFac<-ceiling(((x@xmax+zone)-lon)/5)
  latFac<-ceiling(((x@ymax+zone)-lat)/5)

  ytiles<-seq(1,latFac)
  xtiles<-seq(1,lonFac)
  for (i in ytiles){
    for (j in xtiles){
      if (lon < -180) {lon <- -180}
      if (lon > 180) {lon <- 180 }
      if (lat < -60) {lat<- -60}
      if (lat > 60) {lat<- 60}


      rs <- raster(nrows=24, ncols=72, xmn=-180, xmx=180, ymn=-60, ymx=60 )
      rowTile <- rowFromY(rs, lat)
      colTile <- colFromX(rs, lon)
      if (rowTile < 10) { rowTile <- paste('0', rowTile, sep='') }
      if (colTile < 10) { colTile <- paste('0', colTile, sep='') }

      f <- paste('srtm_', colTile, '_', rowTile, sep="")
      srtmNames <- c(srtmNames, paste(f, ".ZIP", sep=""))
      #tiffilename <- paste(path, "/", f, ".TIF", sep="")
      lon<-lon+5
    }
    lon<-floor(x@xmin-3)
    lat<-lat+5
  }
  return(srtmNames)
}

.SRTM <- function(xtent=NULL, zone = 0, download = TRUE, path = tempfile(), merge = FALSE) {

  if (!download) {
    return(.getSRTMfn(xtent,zone))
  }

  x<-extent(xtent)
  lon<- floor(x@xmin-zone)
  lat<-floor(x@ymin-zone)
  lonFac<-ceiling(((x@xmax+zone)-lon)/5)
  if (lonFac == 0){lonFac<-1}
  latFac<-ceiling(((x@ymax+zone)-lat)/5)
  if (latFac == 0){latFac<-1}
  cat('#########################\nYou are going to download ',
      lonFac*latFac, ' SRTM tiles with approximately ',
      lonFac*latFac*35,' MByte\nNOTE: this may take a while...\n')
  ytiles<-seq(1,latFac)
  xtiles<-seq(1,lonFac)
  for (i in ytiles){
    for (j in xtiles){
      if (lon < -180) {lon <- -180}
      if (lon > 180) {lon <- 180 }
      if (lat < -60) {lat<- -60}
      if (lat > 60) {lat<- 60}

      rs <- raster(nrows=24, ncols=72, xmn=-180, xmx=180, ymn=-60, ymx=60 )
      rowTile <- rowFromY(rs, lat)
      colTile <- colFromX(rs, lon)
      if (rowTile < 10) { rowTile <- paste('0', rowTile, sep='') }
      if (colTile < 10) { colTile <- paste('0', colTile, sep='') }

      f <- paste('srtm_', colTile, '_', rowTile, sep="")
      zipfilename <- paste(path.expand(path), "srtm/", f, ".ZIP", sep="")
      tiffilename <- paste(path.expand(path), "srtm/", f, ".tif", sep="")
      if (!file.exists(tiffilename)) {
        if (!file.exists(zipfilename)) {
          if (download) {
            cat('Downloading: ',basename(zipfilename),'\n')
            theurl <- paste("ftp://srtm.csi.cgiar.org/SRTM_v41/SRTM_Data_GeoTIFF/", f, ".ZIP", sep="")
            test <- try (.download(theurl, zipfilename) , silent=TRUE)
            if (class(test) == 'try-error') {
              theurl <- paste("ftp://xftp.jrc.it/pub/srtmV4/tiff/", f, ".zip", sep="")
              test <- try (.download(theurl, zipfilename) , silent=TRUE)
              if (class(test) == 'try-error') {
                theurl <- paste("http://hypersphere.telascience.org/elevation/cgiar_srtm_v4/tiff/zip/", f, ".ZIP", sep="")
                .download(theurl, zipfilename)
              }
            }
          } else {cat('file not available locally, use download=TRUE\n') }
        }
        if (file.exists(zipfilename)) {
          unzip(zipfilename, exdir=dirname(zipfilename))
          file.remove(zipfilename)
        }
      }

      lon<-lon+5
    }
    lon<-floor(x@xmin-3)
    lat<-lat+5
  }

  if (merge){
    listTif<-list.files(paste0(path.expand(path), "srtm"), pattern = glob2rx("srtm*.tif"),
                        full.names = TRUE, recursive = FALSE)
    cat ("merging:\n",paste(listTif,'\n'))
    mosaicSRTM<-mosaic_rasters(gdalfile=listTif,
                               dst_dataset=paste0(path.expand(path), "srtm/mosaicSRTM.tif"),
                               output_Raster=FALSE,
                               of="GTiff",
                               verbose=FALSE,
                               seperate=FALSE,
                               overwrite= TRUE
    )
    if (! (xtent@xmin-zone == xtent@xmax+zone || xtent@ymin-zone == xtent@ymax+zone)) {
      mosaicSRTM<- gdal_translate(paste0(path.expand(path), "srtm/mosaicSRTM.tif"),
                                  paste0(path.expand(path), "srtm/cMosaicSRTM.tif"),
                                  projwin=c(xtent@xmin-zone,xtent@ymax+zone,xtent@xmax+zone,xtent@ymin-zone),
                                  output_Raster = TRUE,
                                  overwrite= TRUE,
                                  verbose=TRUE
      )} else{

        mosaicSRTM<- gdal_translate(paste0(path.expand(path), "srtm/mosaicSRTM.tif"),
                                    paste0(path.expand(path), "srtm/cMosaicSRTM.tif"),

                                    output_Raster = TRUE,
                                    overwrite= TRUE,
                                    verbose=TRUE)

      }
    projection(mosaicSRTM) <- "+proj=longlat +datum=WGS84"
    return(mosaicSRTM)

  }


}

.tiroldem <- function(items = NULL, path = tempfile(), download = TRUE, all = FALSE, merge = FALSE) {
  if (all == TRUE){
    items<-c('IBK_DGM10','IL_DGM10','IM_DGM10','KB_DGM10','KU_DGM10','LA_DGM10','RE_DGM10','SZ_DGM10','LZ_DGM10')
  }

  for (item in items){
    f <- item
    zipfilename <- paste(path,  f, ".zip", sep="")
    ascfilename <- paste(path,  f, ".asc", sep="")

    if (!file.exists(zipfilename)) {
      if (download) {
        theurl <- paste("https://gis.tirol.gv.at/ogd/geografie_planung/DGM/", f,".zip", sep="")
        test <- try (.download(theurl, zipfilename) , silent=TRUE)
      } else {cat('file not available locally, use download=TRUE\n') }
    }
    if (file.exists(zipfilename)) {
      unzip(zipfilename,junkpath=TRUE, exdir=dirname(zipfilename))
      #file.remove(zipfilename)
    }
  }

  filenames <- list.files(pattern="*.asc", full.names=TRUE)
  rs <- lapply(filenames,.importAsc,'+proj=tmerc +lat_0=0 +lon_0=10.33333333333333 +k=1 +x_0=0 +y_0=-5000000 +ellps=bessel +towgs84=577.326,90.129,463.919,5.137,1.474,5.297,2.4232 +units=m +no_defs')
  if (merge){
    names(rs)[1:2] <- c('x', 'y')
    rs$fun <- mean
    rs <- do.call(mosaic, rs)
  }

  return(rs)

}

.harrylist <- function(extent=c(-180,180,-90,90),path,download=TRUE) {
  # use the download.file function to access online content. note we change already the filename and
  # we also pass the .php extension of the download address
  zipFn=paste0(path,'bergliste-komplett.kmz')
  kmlFn=paste0(path,'bergliste-komplett.kml')
  if (!file.exists(zipFn)) {
    if (download) {
      theurl <-'http://www.tourenwelt.info/commons/download/bergliste-komplett.kmz.php'
      test <- try (.download(theurl) , silent=TRUE)
    } else {cat('file not available locally, use download=TRUE\n') }
  }
  if (file.exists(zipFn)) {
    unzip(zipFn,junkpath=TRUE, exdir=dirname(zipFn))
    file.remove(zipfn)
  }

  if (file.exists(kmlFn)) {

    # convert to csv file with babel (you need to install the babel binaries on your system)
    system("gpsbabel -i kml -f bergliste-komplett.kml -o unicsv -F bergliste-komplett.csv")

    # read into data.frame
    df=read.csv("bergliste-komplett.csv",  header = TRUE, sep = ",", dec='.')

    # extract altitude out of Description column that is full of  html garbage
    altitude<-as.numeric(substring(df$Description, regexpr('H&ouml;he:</td><td>', df$Description)+19,regexpr("</td></tr>", df$Description)-1))

    # delete the unused cols
    df$Description <- NULL
    df$No <- NULL

    # and put altitude values into df
    df$Altitude<- altitude
    # making a subset of the for reaonable Lat Lon Values
    df.sub = subset(df, df$Longitude >= extent$xmin & df$Longitude <= extent$xmax & df$Latitude >= extent$ymin & df$Latitude  <= extent$ymax)
    #df.sub[,!(names(df.sub) %in%  c("optional"))]
    coordinates(df.sub)<- ~Longitude+Latitude
    proj4string(df.sub)<- "+proj=longlat +ellps=WGS84"

    return(df.sub)
  } else {
    stop("Harry's Peaklist file not found")
  }
}



.OSMp <- function(extent,key,val,taglist,download, path) {
  # use the download.file function to access online content. note we change already the filename and
  # we also pass the .php extension of the download address

  # define the spatial extend of the OSM data we want to retrieve
  osm.extend <- corner_bbox(extent[1],extent[3],extent[2],extent[4])

  # download all osm data inside this area, note we have to declare the api interface with source
  cat('Retrieving OSM data. Be patient...')
  osm <- get_osm(osm.extend, source = osmsource_api())
  # find the first attribute key&val
  node.id <- find(osm, node(tags(k == key & v == val)))

  # find downwards (according to the osmar object level hierarchy)
  # all other items that have the same attributes
  all.nodes <- find_down(osm, node(node.id))

  ### to keep it clear and light we make subsets corresponding to the identified objects of all  data
  .sub <- subset(osm, node_ids = all.nodes$node_ids)

  # now we need to extract the corresponding variables and values separately
  # create sub-subsets of the tags 'name' and 'ele' and attrs 'lon' , 'lat'

  .coords<- subset(.sub$nodes$attrs[c('id',"lon", "lat")],)
  i=1
  for(elements in taglist){
    .tmp<- subset(.sub$nodes$tags,(k==elements ))[,-2]
    names(.tmp)[2]<-elements
    if (i==1){
      .stmp<- merge(.coords,.tmp, by="id",all.x=TRUE)
      i=i+1
    }else{
      .stmp<- merge(.stmp,.tmp, by="id",all.x=TRUE)
    }
  }

  # clean the df and rename the cols
  m.df<-.stmp
  colnames(m.df)<- c("osmID","Longitude","Latitude","Name","Altitude")

  # convert the osm.peak df to a SpatialPoints object and assign reference system
  coordinates(m.df) <- ~Longitude+Latitude
  proj4string(m.df)<- CRS("+proj=longlat +datum=WGS84 +no_defs")
  # save to shapefile
  writePointsShape(m.df,"OSMNode.shp")

  # return Spatial Point Object projected in target projection and clipped by the DEM extent
  return(m.df)
}

.schmatz <- function(item, startTime=1,endTime=1, layer=NULL, path, download=TRUE) {
  validItems<-c('prec_eu_wc_30s', 'tave_eu_wcpi_30s', 'tmax_eu_wcpi_30s',  'tmin_eu_wcpi_30s',
                'bioclim_A_MO_pmip2_21k_oa_CCSM_eu_30s', 'bioclim_A_MO_pmip2_21k_oa_CNRM_eu_30s',
                'bioclim_A_MO_pmip2_21k_oa_FGOALS_eu_30s', 'bioclim_A_MO_pmip2_21k_oa_IPSL_eu_30s',
                'bioclim_A_MO_pmip2_21k_oa_MIROC3.2_eu_30s', 'pr_A_MO_pmip2_21k_oa_CCSM_eu_30s',
                'pr_A_MO_pmip2_21k_oa_CNRM_eu_30s',  'pr_A_MO_pmip2_21k_oa_FGOALS_eu_30s',
                'pr_A_MO_pmip2_21k_oa_IPSL_eu_30s',  'pr_A_MO_pmip2_21k_oa_MIROC3.2_eu_30s',
                'tas_A_MO_pmip2_21k_oa_CCSM_eu_30s',  'tas_A_MO_pmip2_21k_oa_CNRM_eu_30s',
                'tas_A_MO_pmip2_21k_oa_FGOALS_eu_30s',  'tas_A_MO_pmip2_21k_oa_IPSL_eu_30s',
                'tas_A_MO_pmip2_21k_oa_MIROC3.2_eu_30s',  'tasmax_A_MO_pmip2_21k_oa_CCSM_eu_30s',
                'tasmax_A_MO_pmip2_21k_oa_CNRM_eu_30s',  'tasmax_A_MO_pmip2_21k_oa_FGOALS_eu_30s',
                'tasmax_A_MO_pmip2_21k_oa_IPSL_eu_30s',  'tasmax_A_MO_pmip2_21k_oa_MIROC3.2_eu_30s',
                'tasmin_A_MO_pmip2_21k_oa_CCSM_eu_30s',  'tasmin_A_MO_pmip2_21k_oa_CNRM_eu_30s',
                'tasmin_A_MO_pmip2_21k_oa_FGOALS_eu_30s',  'tasmin_A_MO_pmip2_21k_oa_IPSL_eu_30s',
                'tasmin_A_MO_pmip2_21k_oa_MIROC3.2_eu_30s',  'TT_Luterbacher_Xoplaki_1659-1998')
  stopifnot(item %in% validItems)

  f <- item
  if (f != 'TT_Luterbacher_Xoplaki_1659-1998'){
    extentNC <- extent(-13,40,35,70)
    ncfilename <- paste(path,  f, ".nc4", sep="")
    ext <- ".nc4"
  } else {
    extentNC <- extent(-30,40.5,35,70.5)
    ncfilename <- paste(path,  f, ".nc", sep="")
    ext <- ".nc"
  }

  var<-unlist(strsplit(item,"_"))[1]
  if (is.null(layer)){
    dataset<-var
  } else {
    dataset<-layer
  }
  if (!file.exists(ncfilename)) {
    if (download) {
      theurl <- paste("http://hs.pangaea.de/model/schmatz/", f,ext, sep="")
      cat( ncfilename,'  is loading from tape...\n
           Processing and download  may take a while...\n
           Further  information: http://doi.pangaea.de/10.1594/PANGAEA.845883?format=html#lcol5.ds12635729')
      test <- try (.download(theurl, ncfilename) , silent=TRUE)
    } else {cat('file not available locally, use download=TRUE\n') }
  }
  if (file.exists(ncfilename)) {
    # register number of cores for parallel operations
    registerDoParallel(cores=detectCores())
    # necessary because of nc file see configuration options http://www.gdal.org/frmt_netcdf.html
    Sys.setenv(GDAL_NETCDF_BOTTOMUP="NO")

    cat('############### import and convert big netCDF raw data - This will need a few moments... ###############')

    # create month sequence
    months<-1*(startTime:endTime)

    # do parallel
    #foreach(i = 1:length(months),.packages='raster') %dopar% {
    for (i in 1:length(months)){
      # create tiffFilename
      tifFilename <- paste(path,  f,"_", i,".tif", sep="")
      gdal_translate(paste0("NETCDF:",ncfilename,":",dataset), tifFilename,
                     b=i,
                     of="GTiff",
                     output_Raster=FALSE,
                     verbose=TRUE,
                     overwrite=TRUE)
    }

  }

  # create list of tiffiles
  tiffFiles <- list.files(dirname(ncfilename), pattern = glob2rx(paste0(var,"*.tif")),
                          full.names = TRUE, recursive = TRUE)

  if (length(tiffFiles) > 0) {


    cat('creating and georeferencing corresponding rasterstack...')
    rss <- stack(tiffFiles)
    # due to GDAL_NETCDF_BOTTOMUP="NO" we have to flip the raster
    rss <- flip(rss, direction='y')
    # assign extent and projection
    extent(rss) <- extentNC #paste0("extent",item)
    projection(rss) <- '+proj=longlat +datum=WGS84 +no_defs'
    return(rss)

  } else {
    stop('file not found')
  }
}



.dataloc <- function() {
  d <- getOption('rasterDataDir')
  if (is.null(d) ) {
    d <- getwd()
  } else {
    d <- trim(d)
    if (d=='') {
      d <- getwd()
    }
  }
  return(d)
}

.importAsc<-function(x,z) {
  y <- raster(x)
  projection(y) <- CRS(z)
  return(y)
ii}
