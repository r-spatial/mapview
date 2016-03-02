if (!isGeneric('makeTile')) {
  setGeneric('makeTile', function(x, ...)
    standardGeneric('makeTile'))
}

#' Create a tile layer from a raster
#'
#' @description makeTile takes a raster object and creates  a leaflet compatible tile structure.
#'

#'
#
#' @author
#' Chris Reudenbach
#'
#' @examples
#' \dontrun{
#' #we need sp and raster ###
#'  library(sp)
#'  library(raster)
#' ### Now we go  bigger
#' ### get some SRTM data
#'   r<-getGeoData(name="SRTM",extent(83.8,27,9,99,34),download=TRUE)
#'   prj3035<-"proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#'   epsg3031<-"+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
#'   makeTile(x="mosaicSRTM.tif",outPath="/home/creu/proj/makeTile", t_srs= prj3035,scale=c(1000,5000))
#'  makeTile(x=fn,outPath="/home/creu/proj/makeTile", s_srs= epsg3031,t_srs= epsg3031)
#'}
#'@name makeTile
#'@export makeTile
#'@rdname makeTile


library(raster)


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
                     zoom=NULL)
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
  # epsg3031<-"+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  # wmcrs <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
  # dProj4="+proj=aea +lat_1=15 +lat_2=65 +lat_0=30 +lon_0=95 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  # prj3035='+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
  # s_projTiles<-"+proj=longlat +datum=WGS84"
  #
  #crs = sp::CRS(wmcrs)
  if (t_srs != s_srs){
  gdalwarp(fnx@file@name,
           paste0(tmpPath,"/rawTile.tif") ,
           s_srs = s_srs,
           t_srs = t_srs,
           r = rType,
           overwrite = TRUE
           )
    fnTranslate<-paste0(tmpPath,"/rawTile.tif")
  } else {
    fnTranslate<-  fnx@file@name
  }

   rx<- gdal_translate(fnTranslate,
                       paste0(tmpPath,"/",rasFn) ,
                       output_Raster = FALSE,
                       overwrite= TRUE,
                       verbose=TRUE,
                       scale=scale,
                       of=rasType
   )

  # calculate zoom level from extent of input raster
  if (is.null(zoom))  {  zoom<-estimateZoom(fnx,t_srs)}

  # make tiles
  r <- system(paste0("inst/htmlwidgets/lib/gdaltiles/gdal2tiles-multiprocess.py   -l  --profile=raster   -z  0-",zoom," -w all --verbose ", paste0(tmpPath,"/",rasFn) ," ", paste0(tmpPath,"/tiles")),intern=T)





  #system(paste0("inst/htmlwidgets/lib/gdaltiles/gdal2tiles-multiprocess.py   -l  --profile=raster  -s ",t_srs," -z  0-",zoom," -w all "," ",tmpPath,"/rawTile.tif "," ",tmpPath,"/tiles"),intern=T)
  return(r)
}

### makepath creates temp paths and filenames =================================================
makeTmpPath <- function (p=NULL){
  if (is.null(p)){
    tmpPath <- tempfile()
  } else {tmpPath <- p}
  dir.create(tmpPath)
  dir.create(paste0(tmpPath,"/tiles"))
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
    return(i)}
  }
  #mapwidth = map height = 256 * 2**7 pixels
}

#r<- system(paste0("gdalwarp -s_srs EPSG:4326 -t_srs '",projlatlon,"' -r bilinear -overwrite  -of GTiFF ",r@file@name, paste0(" ",tmpPath,"/",rasFn)),intern=T)
#system(paste("listgeo -tfw ", fnx@file@name))

#r <-  system(paste0("gdal_translate  -scale -of ", rasType," ",  fnx@file@name," ",paste0(tmpPath,"/",rasFn)),intern=T)

# x <- raster::projectRaster(
#  ras, raster::projectExtent(ras, crs = sp::CRS(wmcrs)),
#  method = "bilinear")
# writeRaster(x,paste0(tmpPath,"/",rasFn))

# get rid of aux.xml file
# if (file.exists(paste0(tmpPath,"/",rasFn,".aux.xml"))) r <- file.remove (paste0(tmpPath,"/",rasFn,".aux.xml"))
#proj4<- gdalinfo(pathRasFn,proj4 = TRUE)[24]