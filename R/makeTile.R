if (!isGeneric('makeTile')) {
  setGeneric('makeTile', function(x, ...)
    standardGeneric('makeTile'))
}

#'Create a leaflet conform set of raster tiles from a GDAL/raster input file
#'
#'@description makeTile takes a raster object or a GDAL raster file  and creates
#'  a leaflet compatible tile structure in a given directory.  Additionally it
#'  produces a correct map.types list for the use with \link{projView}
#'
#'@usage makeTile(x = NULL, outPath = NULL, scalec = (0,8848), s_srs =
#'  "+proj=longlat +datum=WGS84", t_srs = "+proj=longlat +datum=WGS84", t_epsg =
#'  "EPSG:4326", rType = "average", attribution = "to be done", cLat = NULL,
#'  cLon = NULL, zoom = NULL, res = NULL, srvrUrl = "http://localhost:4321/")
#'
#'@param x raster object or gdal file
#'@param outPath path where the tiles will be generated. Note it is always
#'  extented by \code{tiles/}
#'@param scale scale of grey values for jpeg creation
#'@param s_srs source proj4 string
#'@param t_srs target proj4 string
#'@param t_epsg target EPSG code
#'@param rType Resampling method
#'  (average,near,bilinear,cubic,cubicspline,lanczos,antialias) - default
#'  'average'
#'@param attribution string how attribute the tile map default is "still to be
#'  done"
#'@param cLat center of Latitude of the leaflet map object has to be in decimal
#'  degrees
#'@param cLon center of Longitude of the leaflet map object has to be in decimal
#'  degrees
#'@param zoom if you want to override the automatic calculation of maxZoom you
#'  can provide here a value up to 21
#'@param res if you want to override the automatic calculation of the resolution
#'  you can  provide a predifined list like c(512,256,128)
#'@param srvrUrl is the root address of the locale http daemon default see
#'  \link{details}, \link{seealso}
#'
#'@details The integration of local tiles in leaflet is first of all
#'  straightforward. You can easyly generate them with
#'  \href{http://www.gdal.org/gdal2tiles.html}{gdal2tyles}. Actually the
#'  \href{https://github.com/commenthol/gdal2tiles-leaflet}{modified version} of
#'  \href{https://github.com/commenthol}{commenthol} is used, that has some
#'  leaflet specific arguments and engages all available cpus for tiling.
#'
#'  The basic concept is focused on piping the result to \link{projView}. That
#'  means for each tile a nested map.types list is generated. For convenience
#'  reasons this list is also saved in the root directory of the tiles for later
#'  use. For more information about this list and serving have a look at online
#'  help of \href{http://gisma.github.io/projView/projView1_0_9.html}{projView}
#'
#'  Nevertheless one has to take care of some pitfalls.\cr
#'
#'  For the first it seems to be more efficient to override the concept of
#'  \link{createWidget} which copys all necessary files of a htmlwidget instance
#'  to the temporary path and then serves location with the the build in http
#'  daemon of RStudio. Imagine if you just have 6-8 SRTM tiles you need to copy
#'  about 1-2 GB of tiles to the temporary directory. An easy solution is to use
#'  a seperate http daemon as \link{httd} from the package servr.
#'
#'  Also there may problems arise while you use "exotic" projections like polar
#'  stereographic or similar.
#'
#'
#'@seealso
#'
#'NOTE: tile serving usually would work from the local file system without
#'engaging a http daemon. Due to security issues this scenario is a pretty
#'complex topic. To do soo you have to override the default security properties
#'of your browser wich is NOT encouraged. See
#'\href{https://en.wikipedia.org/wiki/JavaScript#Security}{Wikipedia} for some
#'basic expalanation. You may override this setting (here for
#'\href{http://www.chrome-allow-file-access-from-file.com/}{chrome/chromium}),
#'but again this is a first order security vulneration.\cr So it is strictly
#'recommended to use a local http server to provide full access to local tiles
#'and files. There are several solutions within R. Most easiest way to do so is
#'the package \code{\link{httd}}.\cr Nevertheless it is much more convenient to
#'install seperatly a http daemon. If you are not used to the topic the
#'\href{http://twistedmatrix.com}{twistd} daemon  is a very good cross platform
#'powerful, save and simple solution. For some basic information and
#'installation advices have a look at stackoverflow
#'\href{http://stackoverflow.com/questions/12905426/what-is-a-faster-alternative-to-pythons-simplehttpserver}{simplehttpserver}.
#'

#'@author
#' Chris Reudenbach
#'
#' @examples
#' \dontrun{
#' ### we need a running gdal with all python bindings  ###
#' require(raster)
#' require(gdalUtils)
#' require("curl")
#' require(servr)
#'
#' ### Typical workflow nevertheless you just can call maketiles by itself
#'
#' ## get Germany vector data
#'  gadmGER <- getGeoData(name="GADM",country="DEU",level="1")
#'
#' ## download  administartivve data from Germany
#'  gadmDNK <- getGeoData(name="GADM",country="DNK",level="1")
#'
#' ## get the SRTM data for this area
#'  r<-getGeoData(name="SRTM",xtent = extent(11,11,54,54), merge=TRUE)
#'
#' ## define the target projection
#'  s_epsg<-"EPSG:4326"
#'  t_epsg<-"EPSG:32632"
#'  t_srs<-"+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#'
#' ## create tiles and parameter list
#'  map.typesList<-makeTile(x=r,outPath="/home/creu/tmp/data/makeTile/srtm/", t_srs=t_srs,t_epsg=t_epsg,cLat=54,cLon=12,scale=c(-500,1500))
#'
#' ## strt http daemon
#'  servr::httd("~/tmp/data/makeTile/srtm",daemon=TRUE)
#'
#' ## map it
#'  mapview::projView(gadmGER , map.types = map.typesList$layerName)
#'
#' ## stop all daemon instances
#'  servr::daemon_stop(daemon_list())
#'
#' ### data from Quantartica2
#'
#' ## create persistant temp folder
#'  tmpDir<-"~/tmp/data/quantartica"
#'
#' ## choose a file
#'  fn<-paste0(tmpDir,"/Quantarctica2/Basemap/Terrain/ETOPO1_DEM.tif")
#'
#' ## define target projection
#'  s_epsg="EPSG:3031"
#'  t_epsg="EPSG:3031"
#'  t_srs="+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
#'
#' ## create tiles and parameter list
#'  map.typesList<-makeTile(x=localTileFile,outPath="/home/creu/tmp/data/makeTile/quantica",s_epsg=s_epsg, t_srs=t_srs,t_epsg=t_epsg,cLat=-90,cLon=0)
#'
#' ## strt http daemon
#'  servr::httd("/home/creu/tmp/data/makeTile/quantica/",daemon=TRUE)
#'
#' ## map it
#'  mapview::projView(gadmGER , map.types = map.typesList$layerName)
#'
#' ## stop all daemon instances
#'  daemon_stop(daemon_list())
#'
#'}
#'@name makeTile
#'@export makeTile
#'@rdname makeTile


library(raster)


makeTile <- function(x=NULL,
                     outPath=tmpDir(),
                     scale=c(-500,2500),
                     #s_srs="+proj=longlat +datum=WGS84 +ellps=WGS84",
                     t_srs="+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs",
                     s_epsg="NULL",
                     t_epsg="EPSG:3857",
                     rType="average",
                     attribution="still to be done",
                     cLat=NULL,
                     cLon=NULL,
                     zoom=NULL,
                     res=NULL,
                     srvrUrl="http://localhost:4321/"
)
{  tmp <- makeTmpPath(outPath)
if (nchar(x)>0){
  fnx<-raster(x)}
#else if (class(x)=="raster" | class(x)=="rasterbrick" | class(x)=="RasterLayer" || class(x)=="RasterBrick"){
#  writeRaster(x,paste0(outPath,"/tile.tif"),overwrite =TRUE)
#  x@file@name <-paste0(outPath,"/tile.tif")
#  fnx<-x
#}

 else{
  cat("no idea about the input type")
  return()
 }

#nodata<- as.numeric(strsplit(gdalinfo(path.expand(x))[grep("NoData Value=",gdalinfo(path.expand(x)))], split='=', fixed=TRUE)[[1]][2])

nodata<- fnx@file@nodatavalue


tmpPath <- tmp[[1]][1]
pathRasFn <- tmp[[2]][1]
rasFn <- tmp[[3]][1]
rasType <- tmp[[4]][1]

#gdalwarp -overwrite -t_srs EPSG:3857 -r lanczos -multi -dstnodata 0 -of GTiff /home/creu/proj/antarctica/mapview/srtm/cMosaicSRTM.tif /home/creu/tmp/data/makeTile/srtm/rawTileQG.tif
#gdalwarp -overwrite -t_srs EPSG:32632 -r lanczos -multi -dstnodata 0 -of GTiff /home/creu/proj/antarctica/mapview/srtm/cMosaicSRTM.tif /home/creu/tmp/data/makeTile/srtm/rawTileQG.tif

# #if (t_srs != s_srs){
  gdalwarp(path.expand(x),
           path.expand(paste0(tmpPath,"/rawTile.tif")) ,
           s_srs = s_epsg,
           t_srs = t_epsg,
           r = rType,
           #srcnodata = nodata,
           dstnodata = -9999,
           multi = TRUE,
           overwrite = TRUE,
           q = TRUE
  )
fnTranslate<-path.expand(paste0(tmpPath,"/rawTile.tif"))

#} else {
#  fnTranslate<-  path.expand(x@file@name)
#}

rx<- gdal_translate(path.expand(paste0(tmpPath,"/rawTile.tif")),
                    path.expand(paste0(tmpPath,"/",rasFn)) ,
                    output_Raster = FALSE,
                    overwrite= TRUE,
                    verbose=TRUE,
                    scale=scale,
                    of=rasType,
                    q = TRUE
)

# calculate zoom level from extent of input raster
if ( is.null(zoom)){
  zoom<-estimateZoom(fnx,t_srs)[[1]]

}

fnTranslate<-path.expand(paste0(tmpPath,"/",rasFn))

# make tiles take care of the correct zoom!
r <- system(paste0("inst/htmlwidgets/lib/gdaltiles/gdal2tiles-multiprocess.py -l  --profile=raster -r ",rType,"  -z  0-",zoom," -s  ",t_epsg," -w all --verbose ", fnTranslate ," ", path.expand(paste0(tmpPath,"/tiles"))),intern=T)
fnx<-raster(fnTranslate)
map.typesList<-makeMapTypesList(outPath,s_srs,t_srs,t_epsg,fnx,zoom,attribution,cLat,cLon,srvrUrl)
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
  #for (i in seq(1,32)) {
  #  tmpRes<-256 * 2**i
  #  if (tmpRes>ext@nrows) {
  #    return(c(i,groundResolution,dist,ext))}
  #}
  # calculate native zoom of the input raster
  zoom<- ceiling(max(log2(ext@nrows/256),log2(ext@ncols/256)))
  return(c(zoom,groundResolution,dist,ext))
}

makeMapTypesList <- function(outPath=NULL,s_srs=NULL,t_srs=NULL,t_epsg=NULL,fnx=NULL,zoom=NULL,attribution,cLat,cLon,srvrUrl){
  #c(zoom,256,outPath,t_srs,fnx)
  ts<-TRUE
  pathtoTile<-outPath
  layerName<-names(fnx)
  minx<-fnx@extent@xmin
  miny<-fnx@extent@ymin
  maxx<-fnx@extent@xmax
  maxy<-fnx@extent@ymax

  # calculate resolution

  res<-max(maxx-minx,maxy-miny)
  #if (is.null(res)) {res<-abs(ulx) + abs(uly)}
  maxResolution <- res / 256
  resolution<- list()
  for ( i in seq(0,zoom)){
    resolution[i+1] <- maxResolution /  2^i
  }
  minRes<-as.numeric(resolution[length(resolution)])
  # create resolution string
  # resolution<-c(paste(resolution,collapse = ","))
  # LProjResolution<-paste0("[",tmpres,"]")


  # To derive the correct map Center coordinats in lat lat lon !!!
  # and the bounds in the target projection it seems to be easier to
  # co    # reproject it to latlong

  #tmp<-sp::spTransform(bbox, CRS("+proj=longlat +datum=WGS84 +no_defs"))

  if (is.null(cLat)||is.null(cLon)){
    # create an polygon from the xtend of the raster image
    tmpPoly = Polygon(cbind(c(minx,minx,maxx,maxx,minx),c(miny,maxy,maxy,miny,miny)))
    tmpPoly = Polygons(list(tmpPoly), ID = "bbox")
    bbox = SpatialPolygons(list(tmpPoly))

    # if the raster was reprojected
    if (s_srs != t_srs) {
      # assign the target projection
      sp::proj4string(bbox) <-CRS(t_srs)
      # reproject it to latlong
      tmp<-sp::spTransform(bbox, CRS("+proj=longlat +datum=WGS84 +no_defs"))
      #bbox <- sp::spTransform(bbox, CRS(paste0("+init=epsg",substr(t_epsg, 5, nchar(t_epsg))," ",t_srs)))
      # get the xtent
      xt<-extent(tmp)
      # get map center and extent
      xtrLL<-extent(xt)
      cLat <- (xtrLL@ymax-xtrLL@ymin) * 0.5  + xtrLL@ymin
      cLon <- (xtrLL@xmax-xtrLL@xmin) * 0.5 + xtrLL@xmin
    } else {
      proj4string(bbox) <-CRS(s_srs)
      # reproject it to latlong
      tmp<-sp::spTransform(bbox, CRS("+proj=longlat +datum=WGS84 +no_defs"))
      xt<-extent(bbox)
      # get map center and extent
      xtrLL<-extent(xt)
      cLat <- (xtrLL@ymax-xtrLL@ymin) * 0.5  + xtrLL@ymin
      cLon <- (xtrLL@xmax-xtrLL@xmin) * 0.5 + xtrLL@xmin
    }
  }
  # to do +proj=stere +lat_0=-90 +lat_0=+90
  if (t_epsg == "EPSG:3031" || t_epsg == "EPSG:3031" ) {
    res<-max(maxx-minx,maxy-miny)
    #if (is.null(res)) {res<-abs(ulx) + abs(uly)}
    maxRes <- res / 256
    for (i in seq(1,21)) {
      tmp <-256 * 2**i
      if (tmp>minRes & ts) {
        tileSize<-tmp
        ts<-FALSE
      }
    }

    initRes<-log(256, base = 2)
    if (initRes <= 0) {initRes <-1}
    #as.numeric(resolution[length(resolution)])
    tmpres<-2^(initRes:(zoom + initRes))
    tmpres<-sort(tmpres,decreasing = TRUE)
    resolution<-tmpres

    diff <-(sqrt(abs(minx)**2+abs(miny)**2)/2)/zoom
    #oly<-(sqrt(abs(minx)**2+abs(miny)**2)/2)/zoom
    olx<-(minx-diff)
    oly<- (miny-diff)* (-1)

  } else {

     initRes<-log(256, base = 2)
     if (initRes <= 0) {initRes <-1}
     #as.numeric(resolution[length(resolution)])
     tmpres<-2^(initRes:(zoom + initRes))
     tmpres<-sort(tmpres,decreasing = TRUE)
     resolution<-tmpres
#
#     diff <-(sqrt(abs(minx)**2+abs(miny)**2)/2)/zoom
#     #oly<-(sqrt(abs(minx)**2+abs(miny)**2)/2)/zoom
    olx<-minx #(minx-diff)
    oly<-maxy # (miny-diff)* (-1)
    minSide<-min(fnx@ncols,fnx@nrows)
    maxSide<-max(fnx@ncols,fnx@nrows)
    mainScale<-minSide/maxSide
    minSide256<-zoom**2*256
    tileSize<-(minSide/minSide256)*256*mainScale

  }
  # create param list
  map.types<-list(layerName=list(service="OSM",
                                 L.tileLayer=srvrUrl,
                                 layer=list( layerName   = list("tiles","{z}/{x}/{y}")
                                 ),
                                 format="image/png",
                                 tileSize=tileSize,
                                 tms="false",
                                 minZoom=0,
                                 maxZoom=zoom,
                                 noWrap ="true",
                                 continuousworld="true",
                                 attribution=attribution,
                                 params=list(t_epsg=t_epsg,
                                             t_srs=t_srs,
                                             mapCenter=list(cLat=cLat,
                                                            cLon=cLon),
                                             initialZoom="0",
                                             zoomLevels=zoom,
                                             initialResolution=c(resolution),
                                             ovlBounds=list(minx=minx,
                                                            miny=miny,
                                                            maxx=maxx,
                                                            maxy=maxy),
                                             origin=list(olx=olx,
                                                         oly=oly),
                                             useBounds="FALSE"
                                 )) # end of list
  ) # end of total list
  save(map.types,file = paste0(pathtoTile,"/",layerName,".rda"))
  return(map.types)
}
