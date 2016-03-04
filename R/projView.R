if (!isGeneric('projView')) {
  setGeneric('projView', function(x, ...)
    standardGeneric('projView'))
}
#'create projected local tiles for leaflet and maps it
#'
#'@description projView creates and maps a tiles from a given raster or GDAL
#'  object.
#'@usage
#'  projView(url="http://localhost:4321/{z}/{x}/{y}.png")
#'@param url  local/remote url(s) pointing to the tiles to serve
#'@param urlLabel label(s) corresponding to the tile layers
#'@param zoom maximum zoom level. corresponds with the available number of tile
#'  levels -1
#'@param epsgCode e.g. \code{ urn:ogc:def:crs:EPSG::3031} or \code{EPSG:4326}
#'  has to correspond with proj4Str
#'@param proj4Str valid proj4 string
#'@param resolution resolution of tiles for each zoom level
#'@param origin upper left corner of the raster tiles expressed in the new
#'  coordinate system
#'@param tileSize sie of the tiles in pixel default is 256,
#'@param attribution of the layer see details
#'@param makTile default is FALSE if true you can chain makeTile to generate a new tile before rendering it with \code{projView}
#'@param inputFile if \code{url{makeTile}} is TRUE you have to provide the location of the file to be tiled
#'@param pathtoTile path to the tile folder. It is crated automatically and the tiles are stored under \code{pathtoTile/tiles}
#'@param inputFileProj4 valid proj4 string of the input file that is going to be tiled
#'
#'@details \code{vector of labels corresponding to the tile layers} vector of local or remote raster tiles. at the moment only
#'\href{http://leafletjs.com/reference.html#tilelayer}{L.tileLayer} conform
#'adresses are supported.\cr \cr
#'\code{urlLabel} vector of labels corresponding to the tile layers\cr\cr
#' \code{epsgCode} projection code. best practise is using the
#'full \href{http://www.opengeospatial.org/ogcUrnPolicy}{OGC Urn Policy} code
#'(e.g. \code{ urn:ogc:def:crs:EPSG::3031}. In most cases you can use the short
#'version as provided by \href{http://spatialreference.org}{spatialreference}
#'e.g. \code{EPSG:4326}, \code{ESRI:37234}, \code{IAU2000:29918}\cr \cr
#'
#'\code{proj4Str} proj4 projection parameter string. best to retrieve both EPSG and PROJ4 from
#'\href{http://spatialreference.org}{spatialreference.org}\cr \cr
#'\code{resolution}
#'string with the tile resolution/zoom level. It is the number of zoom level + 1
#'multiplied by the tileSize. e.g. zoom = 5, tileSize = 256 => resolution =
#'"8192,4096,2048,1024,512,256"\cr\cr
#'\code{origin}  The upper left corner of the tile image in projected coordinates. Webservices usually provide the
#'correct information but this can be tricky especially for complex projections
#'and local tiles. E.g. for polarstereographic projections you can calculate it
#'as follows: sqrt(abs(minx)**2+abs(miny)**2)/2*zoom\cr\cr
#'\code{attribution} a string
#'with the map references. Please take care of correct referencing of your data.\cr
#'
#'\code{<a href='https://wiki.earthdata.nasa.gov/display/GIBS'> NASA EOSDIS GIBS</a>}\cr
#'
#' There tons of security issues one have to regard if reading locals files with a browser. To avoid this complications it makes sense to serve the local tiles via a http daemon. The most easiest way for common purposes is to use a R internal server like\code{\link{servr}}\cr

#'
#'@author Chris Reudenbach
#'
#' @examples
#' \dontrun{
#' ## requiered packages
#'  require(curl)
#'  require(servr)
#'  require(rgdal)
#'  require(gdalUtils)
#'
#'  ### like all the time we need some data in this case from the great QGIS \href{http://www.quantarctica.org/}{Quantartica project} of the \href{http://www.npolar.no/en}{Norwegian Polar Institute}, ftp://ftp.uninett.no/pub/quantarctica/Quantarctica2.zip
#'
#'  ## creating a tmp dir because this is a lot of data
#'  ## create a permanent tempdir
#'  tmpDir<-"~/tmp/data/quantartica"
#'  dir.create(tmpDir,recursive = TRUE, showWarnings = FALSE)

#'  ## download will take a while (~7 GB)
#'  curl_download(url="ftp://ftp.uninett.no/pub/quantarctica/Quantarctica2.zip", destfile=paste0(tmpDir,"/Quantarctica2.zip"),  quiet = FALSE, mode = "wb")
#'
#'  ## unzip it
#'  unzip(paste0(tmpDir,"/Quantarctica2.zip"), exdir=tmpDir)
#'
#'  ## and choose a data set of interest for tiling...
#'  localFile<-paste0(tmpDir,"/Quantarctica2/Basemap/Terrain/ETOPO1_DEM.tif")
#'
#'  ## serve the directory that contains the valid tile subfolder structure with tiles to serve.
#'  servr::httd("~/proj/makeTile/etopo/tiles",daemon=TRUE)
#'
#'  ## the default usage...
#'  projView(url = c("http://localhost:4321/{z}/{x}/{y}.png","http://localhost:4321/{z}/{x}/{y}.png"),
#'            urlLabel = c("BaseTile","overlaytile"),,
#'            zoom = 5,
#'            epsgCode = "urn:ogc:def:crs:EPSG::3031",
#'            proj4Str = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
#'            tileSize = 256,
#'            resolution = "[8192,4096,2048,1024,512,256]",
#'            origin = "[-639860.046230607,639860.046230607]",
#'            bounds = "[-4524493,-4524493],[4524493,4524493]",
#'            attribution = "<a href='https://github.com/kartena/Proj4Leaflet'> Proj4Leaflet</a> | Projection: <a href='http://spatialreference.org/ref/epsg/wgs-84-antarctic-polar-stereographic/'> EPSG:3031</a> | <a href='www.quantarctica.org'>Norwegian Polar Institute</a>")
#'
#'  ## ...yields the same result as
#'  projView(url = c("http://localhost:4321/{z}/{x}/{y}.png","http://localhost:4321/{z}/{x}/{y}.png"),
#'            urlLabel = c("BaseTile","overlaytile"),,
#'            zoom = 5)
#'
#'  ## pipe the process of tiling and viewing with makTile = TRUE,
#'  projView( makeTile = TRUE,
#'              inputFile = localFile,
#'              pathtoTile = "~/proj/Tiles/antartica/etopo1",
#'              inputFileProj4 = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
#' }
#'@name projView
#'@export projView
#'@rdname projView

projView<- function(url=NULL,
                    urlLabel=NULL,
                    zoom = 5,
                    epsgCode = "urn:ogc:def:crs:EPSG::3031",
                    proj4Str = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
                    tileSize = 256,
                    resolution = "[8192,4096,2048,1024,512,256]",
                    origin = "[-639860.046230607,639860.046230607]",
                    bounds = "[-4194304,-4194304],[4194304,4194304]",
                    attribution = "<a href='https://github.com/kartena/Proj4Leaflet'> Proj4Leaflet</a> | Projection: <a href='http://spatialreference.org/ref/epsg/wgs-84-antarctic-polar-stereographic/'> EPSG:3031</a> | <a href='www.quantarctica.org'>Norwegian Polar Institute</a>",
                    makeTile = FALSE,
                    inputFile = NULL,
                    pathtoTile = NULL,
                    inputFileProj4 = NULL,
                    port="4321",
                    relUrl="")
{

  if (makeTile) {

   # servr::httd(pathtoTile,daemon=TRUE)

    param<-makeTile(x=inputFile,outPath=pathtoTile, s_srs=inputFileProj4 ,t_srs= proj4Str)
    # get zoom level
    zoom<-param[[1]]

    # calculate the resolution as derived by tileSize and zoom level
    startRes<-log(param[[5]], base = 2)
    tmp<-2^(startRes:(startRes+zoom))
    tmp<-sort(tmp,decreasing = TRUE)
    tmp<- paste(tmp,collapse = ",")
    resolution<-paste0("[",tmp,"]")
    # create local url
    url<-c(paste0("http://localhost:",port,"/",relUrl,"{z}/{x}/{y}.png"),paste0("http://localhost:",port,"/",relUrl,"{z}/{x}/{y}.png"))
    urlLabel<- url
    proj4Str<- param[[7]]

    if(epsgCode ==""){
      EPSG<-make_EPSG()
      epsgCode<- paste0("EPSG:",EPSG[grep(proj4Str, EPSG$prj4,fixed=TRUE),1])
      if (epsgCode ==""){return(cat(' no valid EPSG or similar code available')) }
    }
    tileSize<-param[[5]]
    # calculate orig for polarstereographic
    # extract the extent needing only positive half of it
    #tmpOrig<- as.numeric(unlist(strsplit(unlist(strsplit(gsub("[()]","",gdalinfo(x)[grep("Upper Left",gdalinfo(x))]), split='  ', fixed=TRUE))[2], split=',', fixed=TRUE))[2])
    # use pythagoras to shift the ul corner in the correct position
    #  orig<- c(-3199300/(5), 3199300/(5))
    tmpOrig<-abs(param[[4]]@extent@xmin)
    origin <- paste0("[-",(sqrt(tmpOrig**2+tmpOrig**2)/2)/zoom,",",(sqrt(tmpOrig**2+tmpOrig**2)/2)/zoom,"]")

    bounds <- paste0("[",param[[4]]@extent@xmin,",",param[[4]]@extent@ymin,"],[",param[[4]]@extent@xmax,",",param[[4]]@extent@ymax,"]")
    attribution<-attribution
  }

  #epsg<-paste('var ProjCode = "',epsgCode,'";')
  #proj<-paste('var Proj4String ="',Proj4Str,'";')

  # create the CRS object JS code
  crs1<-paste0('var crs =  new L.Proj.CRS("',epsgCode,'",')
  crs2<-paste0('"',proj4Str,'",')
  crs3<-paste0('{resolutions: ',resolution,',')
  crs4<-paste0('origin: ',origin,',')
  crs5<-paste0('bounds: L.bounds(',bounds,')')
  crs6<-paste0('});')
  ## create temp dir and dump it
  tmpCRS <- createTempDataTransfer(f="crs.js")
  fileConn<-file(tmpCRS)
  writeLines(c(crs1,crs2,crs3,crs4,crs5,crs6), fileConn)
  close(fileConn)
  # generate attribution
  if (is.null(attribution)) {
    attribution <-"<a href='https://wiki.earthdata.nasa.gov/display/GIBS'> NASA EOSDIS GIBS</a> &nbsp;|| &nbsp; "}




  layer.name = deparse(substitute(url,env = parent.frame()))
  use.layer.names =TRUE

  # create list of user data that is passed to the widget
  # lst_x <- list(layer = x,
  #               data  = 'undefined',
  #               layername = layer.name,
  #               zoom = zoom)
  lst_x <- list(data  = 'undefined',
                layer = c(url),
                layername=urlLabel,
                zoom = zoom,
                #              epsgcode=epsgCode,
                #              epsgproj=proj4Str,
                tilesize=tileSize,
                attribution=attribution)

  # creating the widget
  projViewInternal(f = tmpCRS ,  x = lst_x)

}

### bViewInternal creates fpView widget =================================================

projViewInternal <- function(f = NULL, x = NULL) {
  data_dir <- dirname(f)
  data_file <- basename(f)
  name<-tools::file_path_sans_ext(data_file)
  dep1 <- htmltools::htmlDependency(name = name,
                                    version = "1",
                                    src = c(file = data_dir),
                                    script = list(data_file))
  deps <- list(dep1)
  sizing = htmlwidgets::sizingPolicy(
    browser.fill = TRUE,
    viewer.fill = TRUE,
    viewer.padding = 5
  )
  # create widget
  htmlwidgets::createWidget(
    name = 'projView',
    x,
    dependencies = deps,
    sizingPolicy = sizing,
    package = 'mapview'
  )
}

### Widget output function for use in Shiny =================================================
#
projViewOutput <- function(outputId, width = '100%', height = '800px') {
  htmlwidgets::shinyWidgetOutput(outputId, 'projView', width, height, package = 'mapview')
}

### Widget render function for use in Shiny =================================================
#
renderprojView<- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(expr, projViewOutput, env, quoted = TRUE)
}

### makeTmppath creates temp paths and filenames =================================================
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

###  creates temporary file structure for data transfer =================================================
createTempDataTransfer <- function (f){
  tmpPath <- tempfile(pattern="007")
  dir.create(tmpPath)
  pathFN <- paste0(tmpPath,"/",f)
  return(pathFN)
}
