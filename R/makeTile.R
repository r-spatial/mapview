if (!isGeneric('makeTile')) {
  setGeneric('makeTile', function(x, ...)
    standardGeneric('makeTile'))
}

#' Create a leaflet conform raster tile layer from a GDAL/raster input
#'
#' @description makeTile takes a raster object or a GDAL raster file  and creates  a leaflet compatible tile structure in a given directory.
#'
#'@param x raster object or gdal file
#'@param outPath tmpDir()
#'@param scale NULL
#'@param gray TRUE
#'@param rgb FALSE
#'@param rgba FALSE
#'@param s_srs "+proj=longlat +datum=WGS84"
#'@param t_srs "+proj=longlat +datum=WGS84"
#'@param epsg NULL
#'@param rType "near"
#'@param zoom NULL
#'
#'
#'@details soon more
#' @author
#' Chris Reudenbach
#'
#' @examples
#' \dontrun{
#'  # we need a running gdal with all python bindings  ###
#'  require(raster)
#'  require(gdalUtils)
#'  require("curl")
#'  ### get some data for the antartica
#'
#'   curl_download(url="ftp://ftp.uninett.no/pub/quantarctica/Quantarctica2.zip", destfile="~/tmp/quantartic.zip"  ,  quiet = TRUE, mode = "wb")
#'   unzip("~/tmp/quantartic.zip")
#'   # define correct projection
#'   epsg3031<-"+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
#'   # choose a dataset
#'   x="/home/creu/proj/antarctica/Quantarctica2/Basemap/Terrain/ETOPO1_DEM.tif"
#'   log<-makeTile(x=x,outPath="/home/creu/proj/makeTile/etopo2DGM", s_srs= epsg3031,t_srs= epsg3031)
#'}
#'@name makeTile
#'@export makeTile
#'@rdname makeTile


library(raster)
tempfile()

makeTile <- function(x=NULL,
                     outPath=tmpDir(),
                     scale=NULL,
                     gray=TRUE,
                     rgb=FALSE,
                     rgba=FALSE,
                     s_srs="+proj=longlat +datum=WGS84",
                     t_srs="+proj=longlat +datum=WGS84",
                     epsg=NULL,
                     rType="near",
                     zoom=NULL
                     )
{
  if (class(x)=="raster"|class(x)=="rasterbrick"){
    writeRaster(x,paste0(outPath,"/tile.tif"))
    fnx<-raster(x)
  } else {
    fnx<-raster(x)
  }
  tmp <- makeTmpPath(outPath)
  tmpPath <- tmp[[1]][1]
  pathRasFn <- tmp[[2]][1]
  rasFn <- tmp[[3]][1]
  rasType <- tmp[[4]][1]

  if (t_srs != s_srs){
    gdalwarp(path.expand(fnx@file@name),
             path.expand(paste0(tmpPath,"/rawTile.tif")) ,
             s_srs = s_srs,
             t_srs = t_srs,
             r = rType,
             overwrite = TRUE
    )
    fnTranslate<-path.expand(paste0(tmpPath,"/rawTile.tif"))
  } else {
    fnTranslate<-  path.expand(fnx@file@name)
  }

  rx<- gdal_translate(fnTranslate,
                      path.expand(paste0(tmpPath,"/",rasFn)) ,
                      output_Raster = FALSE,
                      overwrite= TRUE,
                      verbose=TRUE,
                      scale=scale,
                      of=rasType
  )

  # calculate zoom level from extent of input raster
  if (is.null(zoom))  {  zoom<-estimateZoom(fnx,t_srs)[[1]]}

  # make tiles
  r <- system(paste0("inst/htmlwidgets/lib/gdaltiles/gdal2tiles-multiprocess.py   -l  --profile=raster   -z  0-",zoom," -w all --verbose ", path.expand(paste0(tmpPath,"/",rasFn)) ," ", path.expand(paste0(tmpPath,"/tiles"))),intern=T)

  return(c(estimateZoom(fnx,t_srs),256,outPath,t_srs,fnx))
}

### makepath creates temp paths and filenames =================================================
makeTmpPath <- function (p=NULL){
  if (is.null(p)){
    tmpPath <- tempfile()
  } else {tmpPath <- p}
  dir.create(tmpPath,recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0(tmpPath,"/tiles"),recursive = TRUE, showWarnings = FALSE)
  baseFn <- "rawTile"
  extFn <- "JPEG"
  rasFn <- paste0(baseFn,".",extFn)
  pathRasFn <- paste0(tmpPath,"/",rasFn)
  return(list(tmpPath,pathRasFn,rasFn,extFn))
}

estimateZoom <- function(ext=NULL,proj4){
  srs <- grep("+units=m", proj4,fixed=TRUE)
  rad<-pi/180
  earth<- 6378137
  if (!srs) {
    lat1<-ext@extent@ymin
    lon1<-ext@extent@xmin
    lat2<-ext@extent@ymax
    lon2<-ext@extent@xmax
    dLat <- (lat2-lat1)*rad
    dLon <- (lon2-lon1)*rad
    a <- sin(dLat/2) * sin(dLat/2) + cos(lat1*rad) * cos(lat2*rad) * sin(dLon/2) * sin(dLon/2)
    c <- 2 * atan2(sqrt(a), sqrt(1-a))
    dist <- earth * c
    #https://msdn.microsoft.com/en-us/library/bb259689.aspx
    groundResolution<- (cos(lat1* rad) * dist / ext@ncols)
    #gR<-(cos(lat1 * rad) * 2 * pi * dist) / (256 * 2 **(i-1))
    mScale =  groundResolution * 96 / 0.0254
    latL <- earth * cos(lat1*rad)
    UlatL <- 2 * latL * pi
    fulldiskPixel<-UlatL/dist*ext@ncols

  } else {
    y1<-ext@extent@ymin
    x1<-ext@extent@xmin
    y2<-ext@extent@ymax
    x2<-ext@extent@xmax
    dist<- abs(x1-x2)
    groundResolution<- dist / ext@ncols
    fulldiskPixel<- 2*earth*pi/groundResolution
  }
  for (i in seq(1,32)) {
    tmpRes<-256 * 2**i
    if (tmpRes>ext@nrows) {
      return(c(i,groundResolution,dist,ext))}
  }
}

