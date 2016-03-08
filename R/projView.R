if (!isGeneric('projView')) {
  setGeneric('projView', function(x, ...)
    standardGeneric('projView'))
}
#'maps projected local and remote tiles and vector data using leaflet
#'
#'@description projView maps existing local or online tiles. Optional it creates
#'  a local tile from a given GDAL file. object.
#'
#'@usage projView(map.types, layer.name, zoom, t_epsg, t_srs, tileSize, resolution,
#'  origin, bounds, attribution, makeTile, inputFile, pathtoTile,
#'  inputFileProj4, mapCenter, port, relUrl)
#'
#'@param map.types  local/remote map.types(s) pointing to the tiles to serve
#'@param layer.name label(s) corresponding to the tile layers
#'@param zoom maximum zoom level. corresponds with the available number of tile
#'  levels -1
#'@param t_epsg e.g. \code{ urn:ogc:def:crs:EPSG::3031} or \code{EPSG:4326}
#'  has to correspond with t_srs
#'@param t_srs valid proj4 string
#'@param resolution resolution of tiles for each zoom level
#'@param origin upper left corner of the raster tiles expressed in the new
#'  coordinate system
#'@param tileSize sie of the tiles in pixel default is 256,
#'@param attribution of the layer see details
#'@param makTile default is FALSE if true you can chain makeTile to generate a
#'  new tile before rendering it with \code{projView}
#'@param inputFile if \code{map.types{makeTile}} is TRUE you have to provide the
#'  location of the file to be tiled
#'@param pathtoTile path to the tile folder. It is crated automatically and the
#'  tiles are stored under \code{pathtoTile/tiles}
#'@param inputFileProj4 valid proj4 string of the input file that is going to be
#'  tiled
#'
#'@details
#'
#'NOTE: tile serving usually works from the file system without
#'engaging a http server. Unfortunately this setting it is a pretty complex topic. First of
#'all it is usually not alowed to injure or load data via the browser due to
#'security issues. Additionally browsers do what they want and finally RStudio
#'also starts a webserver.... \cr So it is stringly recommended to use a local http
#'server to provide full access to local tiles and files. There are several
#'solutions within R. Most easiest way to do so is the package \code{\link{httd}}.\cr
#'Nevertheless it is much more convienient to install seperatly a http daemon. If you are not used to the topic the \href{http://twistedmatrix.com}{twistd} daemon  is a very good cross platform powerful, save and simple solution.
#' For some basic information and installation advices have a look at stackoverflow
#'\href{http://stackoverflow.com/questions/12905426/what-is-a-faster-alternative-to-pythons-simplehttpserver}{simplehttpserver}.
#'
#'
#'\code{vector of labels corresponding to the tile layers} vector of local or
#'remote raster tiles. at the moment only
#'\href{http://leafletjs.com/reference.html#tilelayer}{L.tileLayer} conform
#'adresses are supported.\cr \cr \code{layer.name} vector of labels corresponding
#'to the tile layers\cr\cr \code{t_epsg} projection code. best practise is
#'using the full \href{http://www.opengeospatial.org/ogcUrnPolicy}{OGC Urn
#'Policy} code (e.g. \code{ urn:ogc:def:crs:EPSG::3031}. In most cases you can
#'use the short version as provided by
#'\href{http://spatialreference.org}{spatialreference} e.g. \code{EPSG:4326},
#'\code{ESRI:37234}, \code{IAU2000:29918}\cr \cr
#'
#'\code{t_srs} proj4 projection parameter string. best to retrieve both EPSG
#'and PROJ4 from \href{http://spatialreference.org}{spatialreference.org}\cr \cr
#'\code{resolution} string with the tile resolution/zoom level. It is the number
#'of zoom level + 1 multiplied by the tileSize. e.g. zoom = 5, tileSize = 256 =>
#'resolution = "8192,4096,2048,1024,512,256"\cr\cr \code{origin}  The upper left
#'corner of the tile image in projected coordinates. Webservices usually provide
#'the correct information but this can be tricky especially for complex
#'projections and local tiles. E.g. for polarstereographic projections you can
#'calculate it as follows: sqrt(abs(minx)**2+abs(miny)**2)/2*zoom\cr\cr
#'\code{attribution} a string with the map references. Please take care of
#'correct referencing of your data.\cr
#'
#'\code{<a href='https://wiki.earthdata.nasa.gov/display/GIBS'> NASA EOSDIS
#'GIBS</a>}\cr
#'


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
#'  ## NOTE currently depreceated the directory that contains the valid tile subfolder structure with tiles to serve.
#'  # servr::httd("~/proj/Tiles",daemon=TRUE)
#'  system(paste0("twistd -no web --path=","~/proj/Tiles"),wait=FALSE)
#'
#'  ### like all the time we need some data in this case from the great QGIS \href{http://www.quantarctica.org/}{Quantartica project} of the \href{http://www.npolar.no/en}{Norwegian Polar Institute}, ftp://ftp.uninett.no/pub/quantarctica/Quantarctica2.zip
#'
#'  ## creating a tmp dir because this is a lot of data
#'  ## create a permanent tempdir
#'  tmpDir<-"~/tmp/data/quantartica"
#'  dir.create(tmpDir,recursive = TRUE, showWarnings = FALSE)

#'  ## download will take a while (~7 GB)
#'  curl_download(map.types="ftp://ftp.uninett.no/pub/quantarctica/Quantarctica2.zip", destfile=paste0(tmpDir,"/Quantarctica2.zip"),  quiet = FALSE, mode = "wb")
#'
#'  ## unzip it
#'  unzip(paste0(tmpDir,"/Quantarctica2.zip"), exdir=tmpDir)
#'
#'  ## and choose a data set of interest for tiling...
#'  localFile<-paste0(tmpDir,"/Quantarctica2/Basemap/Terrain/ETOPO1_DEM.tif")
#'
#'
#'  ## choose vector data from Quantartica
#'  stations <-rgdal::readOGR(paste0(tmpDir,"/Quantarctica2/Basemap/Vector", "stations")
#'
#'  ##   for tile layers ...
#' projView(  stations,
#'            color=mapviewGetOption("raster.palette")(256),
#'            na.color=mapviewGetOption("na.color"),
#'            map.types= list(tL=list('bluemarble'=list(L.tileLayer='https://map1{s}.vis.earthdata.nasa.gov/wmts-antarctic/BlueMarble_ShadedRelief_Bathymetry/default/EPSG3031_500m/{z}/{y}/{x}.jpg',
#'                                                       layer="BlueMarble_ShadedRelief_Bathymetry",
#'                                                       format="image%2Fjpeg",
#'                                                       tileSize="512",
#'                                                       subdomains="abc",
#'                                                       noWrap ="true",
#'                                                       continuousWorld='true',
#'                                                       attribution="<a href='https://wiki.earthdata.nasa.gov/display/GIBS'> NASA EOSDIS GIBS</a> &nbsp;|| &nbsp; <a href='https://github.com/kartena/Proj4Leaflet'> Proj4Leaflet</a> | Projection: <a href='http://spatialreference.org/ref/epsg/wgs-84-antarctic-polar-stereographic/'> EPSG3031</a>"),
#'                                   ('EPSG3031_250m'= list(L.tileLayer='https://map1{s}.vis.earthdata.nasa.gov/wmts-antarctic/MODIS_Terra_Sea_Ice/default/2014-02-04/EPSG3031_1km/{z}/{y}/{x}.png',
#'                                                          layer='MODIS_Terra_Brightness_Temp_Band31_Day',
#'                                                          format="image%2Fjpeg",
#'                                                          tileSize="512",
#'                                                          subdomains="abc",
#'                                                          noWrap ="true",
#'                                                          continuousWorld='true',
#'                                                          attribution="<a href='https://wiki.earthdata.nasa.gov/display/GIBS'> NASA EOSDIS GIBS</a> &nbsp;|| &nbsp; <a href='https://github.com/kartena/Proj4Leaflet'> Proj4Leaflet</a> | Projection: <a href='http://spatialreference.org/ref/epsg/wgs-84-antarctic-polar-stereographic/'> EPSG3031</a>"
#'           ))
#'           )),
#'           lwd = 2,
#'           alpha = 0.6,
#'           legend = FALSE,
#'           legend.opacity = 1,
#'           t_epsg = "urn:ogc:def:crs:EPSG::3031",
#'           t_srs = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
#'           setBound=TRUE,
#'           setmapCenter =TRUE,
#'           mapCenter=c(-90,0),
#'           resolution= "[8192,4096,2048,1024,512,256]",
#'           bounds = "[-4194304,-4194304],[4194304,4194304]",
#'           origin= "[-4194304,4194304]")
#'
#' ### get some more data
#'  curl_download("http://download.geofabrik.de/antarctica-latest.shp.zip", destfile=paste0(tmpDir,"/geofabrikATA.zip"),  quiet = FALSE, mode = "wb")
#'  unzip(paste0(tmpDir,"/geofabrikATA.zip"), exdir=tmpDir)
#'  tmpDir<-"~/tmp/data/quantartica"
#'  naturalATA<- rgdal:::readOGR(dsn =path.expand(tmpDir), layer="waterways")
#'
#'
#'projView(  naturalATA,
#'                        map.types= list(tL=list('bluemarble'=list(L.tileLayer='https://map1{s}.vis.earthdata.nasa.gov/wmts-antarctic/BlueMarble_ShadedRelief_Bathymetry/default/EPSG3031_500m/{z}/{y}/{x}.jpg',
#'                                                                  layer="BlueMarble_ShadedRelief_Bathymetry",
#'                                                                  format="image%2Fjpeg",
#'                                                                  tileSize="512",
#'                                                                  subdomains="abc",
#'                                                                  noWrap ="true",
#'                                                                  continuousWorld='true',
#'                                                                  attribution="<a href='https://wiki.earthdata.nasa.gov/display/GIBS'> NASA EOSDIS GIBS</a> &nbsp;|| &nbsp; <a href='https://github.com/kartena/Proj4Leaflet'> Proj4Leaflet</a> | Projection: <a href='http://spatialreference.org/ref/epsg/wgs-84-antarctic-polar-stereographic/'> EPSG3031</a>"),
#'                                                                  ('EPSG3031_250m'= list(L.tileLayer='https://map1{s}.vis.earthdata.nasa.gov/wmts-antarctic/MODIS_Terra_Sea_Ice/default/2014-02-04/EPSG3031_1km/{z}/{y}/{x}.png',
#'                                                                  layer='MODIS_Terra_Brightness_Temp_Band31_Day',
#'                                                                  format="image%2Fjpeg",
#'                                                                  tileSize="512",
#'                                                                  subdomains="abc",
#'                                                                  noWrap ="true",
#'                                                                  continuousWorld='true',
#'                                                                  attribution="<a href='https://wiki.earthdata.nasa.gov/display/GIBS'> NASA EOSDIS GIBS</a> &nbsp;|| &nbsp; <a href='https://github.com/kartena/Proj4Leaflet'> Proj4Leaflet</a> | Projection: <a href='http://spatialreference.org/ref/epsg/wgs-84-antarctic-polar-stereographic/'> EPSG3031</a>"
#'                                       ))
#'                       )),
#'                       lwd = 3,
#'                       alpha = 0.9,
#'                       t_epsg = "urn:ogc:def:crs:EPSG::3031",
#'                       t_srs = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
#'                       setBound=TRUE,
#'                       setmapCenter =TRUE,
#'                       mapCenter=c(-90,0),
#'                       resolution= "[8192,4096,2048,1024,512,256]",
#'                       bounds = "[-4194304,-4194304],[4194304,4194304]",
#'                       origin= "[-4194304,4194304]")
#'
#'
#'  ### define a wms resource
#' resource<-list(wms=list('hav grundkarta'=list(L.tileLayer.wms='http://geodatatest.havochvatten.se/geoservices/ows',
#'                                                layers='hav-bakgrundskartor:hav-grundkarta',
#'                                                format='image/png',
#'                                                continuousWorld='true',
#'                                                attribution=' | &copy; <a href="https://www.havochvatten.se/kunskap-om-vara-vatten/kartor-och-geografisk-information/karttjanster.html">Havs- och vattenmyndigheten (Swedish Agency for Marine and Water Management)</a> | Projection: <a href="http://spatialreference.org/ref/epsg/2056"> EPSG:2056</a>'
#' )))
#'
#'
#' ### getting some vector data
#' sweden<-getGeoData("GADM", country="SWE", level = 2)
#'
#' projView(sweden,
#'  color=mapviewGetOption("raster.palette")(256),
#'  na.color=mapviewGetOption("na.color"),
#'  map.types= resource,
#' cex = 10,
#' lwd = 2,
#' alpha = 0.6,
#' legend = FALSE,
#' legend.opacity = 1,
#' verbose = mapviewGetOption("verbose"),
#' use.layer.names = FALSE,
#' t_epsg = 'EPSG:3006',
#' t_srs = '+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs',
#' resolution= "[8192, 4096, 2048, 1024, 512, 256, 128,64, 32, 16, 8, 4, 2, 1, 0.5]",
#' origin= "[0,0]",
#' relUrl="")
#'
#'
#'
#'### just change the projection changing EPSG Code and proj4 string
#' projView(sweden,
#'  map.types= resource,
#' lwd = 2,
#' alpha = 0.6,
#' t_epsg = 'EPSG:2056',
#' t_srs = '+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs',
#' resolution= "[8192, 4096, 2048, 1024, 512, 256, 128,64, 32, 16, 8, 4, 2, 1, 0.5]",
#' origin= "[0,0]")
#' }
#'@name projView
#'@export projView
#'@rdname projView
#'@import rgdal
#'@import gdalUtils

projView<- function( x,
                     zcol =NULL,
                     color=mapviewGetOption("raster.palette")(256),
                     na.color=mapviewGetOption("na.color"),
                     map.types='//tile.osm.ch/2056/{z}/{x}/{y}.png',
                     cex = 10,
                     lwd = 2,
                     alpha = 0.6,
                     legend = FALSE,
                     legend.opacity = 1,
                     verbose = mapviewGetOption("verbose"),
                     use.layer.names = FALSE,
                     layer.name = deparse(substitute(x,
                                                     env = parent.frame())),

                     trim = TRUE,
                     popup = popup,
                     t_epsg = "urn:ogc:def:crs:EPSG::2056",
                     t_srs = "+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs",
                     attribution = '&copy; <a href="//openstreetmap.org/copyright">OpenStreetMap</a> contributors. More <a href="//sosm.ch/swiss-lv95-projected-tiles/">information</a>',
                     subdomains='a,b,c',
                     tileSize = 256,
                     resolution= NULL,
                     setBound=FALSE,
                     setmapCenter=FALSE,
                     bounds=NULL,
                     mapCenter=c(-90,0.),
                     zoom=18,
                     initialZoom=5,
                     origin= "[0,0]",
                     relUrl="")
{


  ## temp dir
  tmpPath<- createTempDataTransfer()

  # check if x exist
  if (!is.null(x)) {
    # get epsg code and proj4 string from vector file
    string<-unlist(strsplit(x@proj4string@projargs, split='+', fixed=TRUE))
    epsg<-string[grepl("init",string)]
    unlist(strsplit(epsg, split=':', fixed=TRUE))[2]
    s_srs<-x@proj4string@projargs
    s_epsg <- paste0("epsg:",unlist(strsplit(epsg, split=':', fixed=TRUE))[2])
    # get extent from vector file

    # recalculate center of map in lat lon
    #ulc<-gdaltransform(s_srs=s_srs, t_srs=t_srs,coords=matrix(c(xt@xmin,xt@ymax), ncol = 2))[1:2]
    #lrc<-gdaltransform(s_srs=s_srs, t_srs=t_srs,coords=matrix(c(xt@xmax,xt@ymin), ncol = 2))[1:2]
    #bounds <- paste0("[",ulc[1],",",ulc[2],"],[",lrc[1],",",lrc[2],"]")

    # get map center and extent
    xtr <- mapview:::spCheckAdjustProjection(x)
    xtrLL<-extent(xtr)
    if (!setmapCenter){
    mapCenterLat <- (xtrLL@ymax-xtrLL@ymin) * 0.5  + xtrLL@ymin
    mapCenterLon <- (xtrLL@xmax-xtrLL@xmin) * 0.5 + xtrLL@xmin}
    else{
    mapCenterLat = mapCenter[1]
    mapCenterLon = mapCenter[2]
      }

    # optional calculate the resolution as derived by local tiles and zoom level
    startRes<-log(tileSize, base = 2)
    tmpres<-2^(startRes:(startRes+zoom))
    tmpres<-sort(tmpres,decreasing = TRUE)
    tmpres<- paste(tmpres,collapse = ",")

    if (is.null(resolution)) {resolution<-paste0("[",tmpres,"]")}

    # NOW transform x to target projection
    x <- sp::spTransform(x, CRSobj = t_srs)
    xt<-extent(x)
    if ( !setBound  & is.null(bounds)) {bounds <- paste0("[",xt@ymax,",",xt@xmin,"],[",xt@ymin,",",xt@xmax,"]")}
    # define jsonpath
    tmpJSON <-paste(tmpPath, ".jsondata", sep=.Platform$file.sep)
    # check and correct if sp object is of type dataframe
    x <- mapview:::toSPDF(x)
    # check if a correct WGSS84 proj4 string exist
    ##x@proj4string@projargs<-mapview:::compareProjCode(strsplit(x@proj4string@projargs,split = " "))
    # check and transform projection
    ##x <- spCheckAdjustProjection(x)

    # get the variable names
    keep <- colnames(x@data)

    # apply zcol
    if (!is.null(zcol)) {
      keep <- c(zcol)
    }
    x@data <- x@data[(names(x@data) %in% keep)]

    # write to a file to be able to use ogr2ogr
    rgdal::writeOGR(x, paste(tmpPath, "jsondata", sep=.Platform$file.sep), "OGRGeoJSON", driver="GeoJSON")

    # for fastet json read in a html document we wrap it with var data = {};
    # and we fix the crs item of ogrjson
    lns <- data.table::fread(paste(tmpPath, "jsondata", sep=.Platform$file.sep), header = FALSE, sep = "\n", data.table = FALSE)
    lns[1,] <-paste0('var jsondata = {')
    lns[3,]<-paste0('"crs": { "type": "name", "properties": { "name": "',t_epsg,'" } },')
    lns[length(lns[,1]),]<- '};'
    write.table(lns, paste(tmpPath, "jsondata", sep=.Platform$file.sep), sep="\n", row.names=FALSE, col.names=FALSE, quote = FALSE)

    if (class(x)[1] == 'SpatialPolygonsDataFrame'){
      noFeature <- length(x@polygons)
    } else if (class(x)[1] == 'SpatialLinesDataFrame'){
      noFeature <- length(x@lines)
    } else {
      # nrow(coordinates(x)
    }
  }
  # DEFINE GLOBAL VARIABLES



  if (!is.null(mapCenter)){
  varmapCenter<-paste0('var mapCenter = [',mapCenterLat,',',mapCenterLon,'];')}
  else{ varmapCenter<-paste0('var mapCenter = [',mapCenter,'];')
  }
  tmpCRS <- paste0(tmpPath,"/crs.js")
  write(paste('var initialZoom = ',initialZoom,';'),tmpCRS,append = TRUE)
  write(varmapCenter,tmpCRS,append = TRUE)
  write(paste0('var crs =  new L.Proj.CRS("',t_epsg,'",'),tmpCRS,append = TRUE)
  write(paste0('"',t_srs,'",'),tmpCRS,append = TRUE)
  write(paste0('{resolutions: ',resolution,','),tmpCRS,append = TRUE)
  if(setBound){write(paste0('origin: ',origin,','),tmpCRS,append = TRUE)
  write(paste0('bounds: L.bounds([',bounds,'])'),tmpCRS,append = TRUE)}
  else {write(paste0('origin: ',origin),tmpCRS,append = TRUE) }

  write(paste0('});'),tmpCRS,append = TRUE)

#  write("var map = new L.map(el, {",tmptL,append = TRUE)
#  write("crs : crs,",tmptL,append = TRUE)
#  write("continuousWorld: true,",tmptL,append = TRUE)
#  write("worldCopyJump: true",tmptL,append = TRUE)
#  write("})",tmptL,append = TRUE)

  #  create the CRS object JS code
  #crs1<-paste0('var crs =  new L.Proj.CRS("',t_epsg,'",')
  #crs2<-paste0('"',t_srs,'",')
  #crs3<-paste0('{resolutions: ',resolution,',')
  #crs4<-paste0('origin: ',origin,',')
  #crs5<-paste0('bounds: L.bounds(',bounds,')')
  #crs6<-paste0('});')
  ## create temp dir and dump it
  #tmpCRS <- paste0(tmpPath,"/crs.js")
  #fileConn<-file(tmpCRS)
  #if (bound){writeLines(c(varmapCenter,varmapZoom,crs1,crs2,crs3,crs4,crs5,crs6), fileConn)}
  #else{ writeLines(c(varmapCenter,varmapZoom,crs1,crs2,crs3,crs4,crs6), fileConn)}
  #close(fileConn)

  tmptL <- paste0(tmpPath,"/tL.js")
  if(!is.null(map.types)){
    write("// setup map layers\n var baseLayers = {};\n var overlayLayers = {};\n",tmptL,append = TRUE)
    if(!is.null(map.types$tL)){
      for (i in 1:length(map.types$tL)) {
        for (j in 1:length(map.types$tL[[i]])){
          if (j == 1 & i==1){
            v <- paste0("baseLayers['",map.types$tL[[i]]$layer, "'] =  L.tileLayer('",map.types$tL[[i]]$L.tileLayer,"',{")}
          else if (j == 1 & i>1) {v<-paste0("overlayLayers['",map.types$tL[[i]]$layer, "'] =  L.tileLayer('",map.types$tL[[i]]$L.tileLayer,"',{")}
          else{
            if (j != length(map.types$tL[[i]])){
              if (attributes(map.types$tL[[i]][j]) == "layer" | attributes(map.types$tL[[i]][j]) == "format" | attributes(map.types$tL[[i]][j]) == "subdomains"){
              v <- paste0(attributes(map.types$tL[[i]][j]),': "', map.types$tL[[i]][j],'",')}
              else { v <- paste0(attributes(map.types$tL[[i]][j]),": ", map.types$tL[[i]][j],",")}
            }else{
              v <- paste0(attributes(map.types$tL[[i]][j]),': "', map.types$tL[[i]][j],'"});')
            }
          }
          write(v,tmptL,append = TRUE)
        }
      }}
    if(!is.null(map.types$wms)){
      for (i in 1:length(map.types$wms)) {
        for (j in 1:length(map.types$wms[[i]])){
          if (j == 1 & i == 1 ){
            v <- paste0("baseLayers['",map.types$wms[[i]]$layer, "'] =  ",attributes(map.types$wms[[i]][j]),"('",map.types$wms[[i]]$L.tileLayer.wms,"',{")}
          else if (j == 1 & i>1){
            v <- paste0("overlayLayers['",map.types$wms[[i]]$layer, "'] =  ",attributes(map.types$wms[[i]][j]),"('",map.types$wms[[i]]$L.tileLayer.wms,"',{")
          }
          else{
            if (j != length(map.types$wms[[i]])){
              if (attributes(map.types$wms[[i]][j]) == "layers" | attributes(map.types$wms[[i]][j]) == "format"){
                v <- paste0(attributes(map.types$wms[[i]][j]),": '", map.types$wms[[i]][j],"',")
              } else { v <- paste0(attributes(map.types$wms[[i]][j]),": ", map.types$wms[[i]][j],",")}
            }else{
              v <- paste0(attributes(map.types$wms[[i]][j]),": '", map.types$wms[[i]][j],"'});")
            }
          }
          write(v,tmptL,append = TRUE)
        }
      }}
  } else {cat("you did not provide a map")}
  #layer.name = deparse(substitute(map.types,env = parent.frame()))
  #use.layer.names =TRUE

  # create list of user data that is passed to the widget
  lst_x <- list(data  = 'undefined',
                layername=layer.name,
                zoom = zoom,
                t_epsg=t_epsg,
                t_srs=t_srs,
                tilesize=tileSize,
                attribution=attribution,
                color = mapview:::col2Hex(color),
                #refpoint=refpoint,
                html = getPopupStyle(),
                opacity = alpha,
                weight = lwd,
                values = x@data,
                ext=xt
  )

  # creating the widget
  projViewInternal(f = paste0(tmpPath,"/crs.js") , jFn = paste(tmpPath, "jsondata", sep=.Platform$file.sep),  tmptL= paste0(tmpPath,"/tL.js"),x = lst_x)

}

### bViewInternal creates fpView widget =================================================

projViewInternal <- function(f = NULL, jFn= NULL, tmptL=NULL, x = NULL) {
  data_dir <- dirname(f)
  data_file <- basename(f)
  name<-tools::file_path_sans_ext(data_file)
  dep1 <- htmltools::htmlDependency(name = name,
                                    version = "1",
                                    src = c(file = data_dir),
                                    script = list(data_file))
  data_dir <- dirname(jFn)
  data_file <- basename(jFn)
  dep2 <- htmltools::htmlDependency(name = "jsondata",
                                    version = "1",
                                    src = c(file = data_dir),
                                    script = list(data_file))
  data_dir <- dirname(tmptL)
  data_file <- basename(tmptL)
  name<-tools::file_path_sans_ext(data_file)
  dep3 <- htmltools::htmlDependency(name = name,
                                    version = "1",
                                    src = c(file = data_dir),
                                    script = list(data_file))
  deps <- list(dep1,dep2,dep3)
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
###  creates temporary file structure for data transfer =================================================
createTempDataTransfer <- function (){
  tmpPath <- tempfile(pattern="007")
  dir.create(tmpPath)
  #pathFN <- paste0(tmpPath,"/",f)
  return(tmpPath)
}



### getPopupStyle creates popup style =================================================
getPopupStyle <- function(){
  htmlTemplate <- paste(
    "<html>",
    "<head>",
    "<style>",
    "#popup",
    "{font-family: Arial, Helvetica, sans-serif;width: 20%;border-collapse: collapse;}",
    "#popup td {font-size: 1em;border: 0px solid #85ADFF;padding: 3px 20px 3px 3px;}",
    "#popup tr.alt td {color: #000000;background-color: #F0F5FF;}",
    "#popup tr.coord td {color: #000000;background-color: #A8E6A8;}",
    "div.scrollableContainer {max-height: 200px;max-width: 100%;overflow-y: auto;overflow-x: auto;margin: 0px;background: #D1E0FF;}",
    "</style>",
    "</head>",
    "<body>",
    "<div class='scrollableContainer'>",
    "<table class='popup scrollable'>",
    "<table id='popup'>")
  return(htmlTemplate)
}
