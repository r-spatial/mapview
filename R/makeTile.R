if (!isGeneric('makeTile')) {
  setGeneric('makeTile', function(x, ...)
    standardGeneric('makeTile'))
}

#' Create a leaflet conform raster tile layer from a GDAL/raster input file
#'
#'@description makeTile takes a raster object or a GDAL raster file  and creates  a leaflet compatible tile structure in a given directory.  Additionally it produces a correct map.types list for the use with \link{projView}
#'
#'@usage makeTile(x, outPath, scale, s_srs, t_srs, t_epsg, rType, attribution)
#'
#'@param x raster object or gdal file
#'@param outPath tmpDir()
#'@param scale scale of grey values for jpeg creation default c(0,8848)
#'@param s_srs source proj4 string default  "+proj=longlat +datum=WGS84"
#'@param t_srs target proj4 string  "+proj=longlat +datum=WGS84"
#'@param t_epsg target EPSG code "EPSG:4326"
#'@param rType resampling type default is near "near"
#'@param attribution string how attribute the tile map
#'
#'
#'@details soon more
#' @author
#' Chris Reudenbach
#'
#' @examples
#' \dontrun{
#'  ## we need a running gdal with all python bindings  ###
#'  require(raster)
#'  require(gdalUtils)
#'  require("curl")
#'
#'  ## get some data for the antartica
#'   curl_download(url="ftp://ftp.uninett.no/pub/quantarctica/Quantarctica2.zip", destfile="~/tmp/quantartic.zip"  ,  quiet = TRUE, mode = "wb")
#'   unzip("~/tmp/quantartic.zip")
#'
#'  ## define target projection in this case also used for source
#'   proj3031<-"+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
#'
#'  ## define target epsg code
#'   epsg3031<-"EPSG:3031"
#'
#'  ## choose a dataset from quantartica2
#'   x<-"/home/creu/proj/antarctica/Quantarctica2/Basemap/Terrain/ETOPO1_DEM.tif"
#'
#'  ## create tiles
#'   map.typesList<-makeTile(x=x,outPath="/home/creu/proj/makeTile/etopo2DGM", s_srs= proj3031,t_srs= proj3031,t_epsg=epsg3031,cLat=-90,cLon=0)
#'}
#'@name makeTile
#'@export makeTile
#'@rdname makeTile


library(raster)


makeTile <- function(x=NULL,
                     outPath=tmpDir(),
                     scale=c(0,8848),
                     s_srs="+proj=longlat +datum=WGS84",
                     t_srs="+proj=longlat +datum=WGS84",
                     t_epsg="EPSG:4326",
                     rType="bilinear",
                     attribution="still to be done",
                     cLat=NULL,
                     cLon=NULL
)
{  tmp <- makeTmpPath(outPath)
if (class(x)=="raster" | class(x)=="rasterbrick" | class(x)=="RasterLayer" || class(x)=="RasterBrick"){
  writeRaster(x,paste0(outPath,"/tile.tif"),overwrite =TRUE)
  x@file@name <-paste0(outPath,"/tile.tif")
  fnx<-x
} else {
  fnx<-raster(x)
}

#nodata<- as.numeric(strsplit(gdalinfo(path.expand(x))[grep("NoData Value=",gdalinfo(path.expand(x)))], split='=', fixed=TRUE)[[1]][2])

nodata<- fnx@file@nodatavalue

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
           srcnodata = nodata,
           multi = TRUE,
           overwrite = TRUE,
           q = TRUE
  )
  fnTranslate<-path.expand(paste0(tmpPath,"/rawTile.tif"))
  s_srs<-t_srs
} else {
  fnTranslate<-  path.expand(fnx@file@name)
}

rx<- gdal_translate(fnTranslate,
                    path.expand(paste0(tmpPath,"/",rasFn)) ,
                    output_Raster = FALSE,
                    overwrite= TRUE,
                    verbose=TRUE,
                    scale=scale,
                    of=rasType,
                    q = TRUE
)

# calculate zoom level from extent of input raster
zoom<-estimateZoom(fnx,t_srs)[[1]]
# have to check if the estimated zoom is fitting to the native zoom of the raster
if (zoom < log2(fnx@nrows/256) || zoom < log2(fnx@ncols/256))
{
  zoom <- zoom + 1
}



# make tiles take care of the correct zoom!
r <- system(paste0("inst/htmlwidgets/lib/gdaltiles/gdal2tiles-multiprocess.py   -l  --profile=raster   -z  0-",zoom," -w all --verbose ", path.expand(paste0(tmpPath,"/",rasFn)) ," ", path.expand(paste0(tmpPath,"/tiles"))),intern=T)
fnx<-raster(fnTranslate)
map.typesList<-makeMapTypesList(outPath,s_srs,t_srs,t_epsg,fnx,zoom,attribution,cLat,cLon)
return(map.typesList)
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
  if (length(grep("+proj=longlat", proj4,fixed=TRUE))>= 1){
    lola<-TRUE
  }else
  {lola <- FALSE}
  rad<-pi/180
  earth<- 6378137
  if (lola) {
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

makeMapTypesList <- function(outPath=NULL,s_srs=NULL,t_srs=NULL,t_epsg=NULL,fnx=NULL,zoom=NULL,attribution,cLat,cLon){
  #c(zoom,256,outPath,t_srs,fnx)
  pathtoTile<-outPath
  layerName<-names(fnx)
  minx<-fnx@extent@xmin
  miny<-fnx@extent@ymin
  maxx<-fnx@extent@xmax
  maxy<-fnx@extent@ymax

  if (is.null(cLat)||is.null(cLon)){

    tmpPoly = Polygon(cbind(c(minx,minx,maxx,maxx,minx),c(miny,maxy,maxy,miny,miny)))
    tmpPoly = Polygons(list(tmpPoly), ID = "bbox")
    bbox = SpatialPolygons(list(tmpPoly))

    proj4string(bbox) <-CRS(s_srs)
    bbox <- sp::spTransform(bbox, CRS(paste0("+init=epsg",substr(t_epsg, 5, nchar(t_epsg))," ",t_srs)))
    xt<-extent(bbox)
    # get map center and extent
    xtrLL<-extent(xt)
    cLat <- (xtrLL@ymax-xtrLL@ymin) * 0.5  + xtrLL@ymin
    cLon <- (xtrLL@xmax-xtrLL@xmin) * 0.5 + xtrLL@xmin

  }

  olx<-minx
  oly<-maxy

  map.types<-list(layerName=list(service="OSM",
                                 L.tileLayer="tiles/",
                                 layer=list( layerName   = list("{z}/{y}/{x}")
                                 ),
                                 format="image/png",
                                 tileSize="256",
                                 subdomains="abc",
                                 minZoom=0,
                                 maxZoom=zoom,
                                 noWrap ="true",
                                 attribution=attribution,
                                 params=list(t_epsg=t_epsg,
                                             t_srs=t_srs,
                                             mapCenter=list(cLat=cLat,
                                                            cLon=cLon),
                                             initialZoom="0",
                                             zoomLevels=zoom,
                                             initialResolution="256",
                                             ovlBounds=list(minx=minx,
                                                            miny=miny,
                                                            maxx=maxx,
                                                            maxy=maxy),
                                             origin=list(olx=olx,
                                                         oly=oly),
                                             relUrl=""

                                 )) # end of list
  ) # end of total list
  save(map.types,file = paste0(pathtoTile,"/",layerName,".rda"))
  return(map.types)
}
