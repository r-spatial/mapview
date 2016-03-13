if (!isGeneric('projView')) {
  setGeneric('projView', function(x, ...)
    standardGeneric('projView'))
}
#'projView generates projected leaflet maps using (local) or remote tiles and arbitrary vector for obverlaying.
#'
#'@description projView maps existing local or online tiles in the choosen target projection. The first raster tile is taken for the default background.
#'
#'@usage projView( x, zcol, color, na.color, map.types, cex, lwd, alpha, legend, legend.opacity, verbose, use.layer.names,layer.name,popup,internalList, externalList)
#'
#'@param x a \code{\link{sp}}* object
#'@param map.types an optionallist of map tiles see \link{details}
#'@param color color (palette) for points/polygons/lines
#'@param na.color color for missing values
#'@param use.layer.names should layer names of the Raster* object be used?
#'@param values a vector of values for the visualisation of the layers.
#' Per default these are calculated based on the supplied raster* object.
#'@param map.types character spcifications for the base maps.
#' see \url{http://leaflet-extras.github.io/leaflet-providers/preview/}
#' for available options.
#'@param alpha opacity of the lines or points
#'@param legend should a legend be plotted
#'@param legend.opacity opacity of the legend
#'@param verbose should some details be printed during the process
#'@param layer.name the name of the overlay layer to be shown on the map
#'@param popup a character vector of the HTML content for the popups. See
#' \code{\link{addControl}} for details.
#'@param internalList default is FALSE if set to TRUE it is possible to pipe a command with externalList
#'@param externalList list of two strings  first item is the keyword for the kind of data (currently just "arctic-nasa" is implemented, second is the R command to be evaluated. example: externalList = c("arctic-nasa","visEarthPole(groupList='1000',dateString='2014-02-04',createList = TRUE)"))
#'@param zcol attribute name(s) or column number(s) in attribute table
#' of the column(s) to be rendered
#'@param cex attribute name(s) or column number(s) in attribute table
#' of the column(s) to be used for defining the size of circles
#'@param lwd line width
#'
#'@details Please note: If you use coordinates with numbers that are smaller then the Latitude/Longitude Values (i.e. -180/+180, -90/+90) projView assumes that you provide coordinates in the target srs and will not touch them. This is especially helpful for pole standing polarstereographic projections and projections that are not "common" or well described by an EPSG, ESRI or other code. Otherwise it construct a rectangle and project the coordinates towards the target system. \cr For all other information  please have a look at the vignette or at \href{http://gisma.github.io/projView/projView1_0_9.html}{projView}
#'
#'@references
#'Online maps and tile services\cr
#'The NASA Earthdata project: \href{https://wiki.earthdata.nasa.gov/display/GIBS}{NASA EOSDIS GIBS}\cr
#'The Artic Biodiversity Assesment: \href{http://www.arcticbiodiversity.is/}{CAFF}, \href{http://dev.caff.is:8080/geoserver/web/}{CAFF Geoserver}\cr
#'The  Havs- och vattenmyndigheten (Swedish Agency for Marine and Water Management): \href{https://www.havochvatten.se/kunskap-om-vara-vatten/kartor-och-geografisk-information/karttjanster.html}{HAV}\cr\cr
#'For the used overlay data see: \code{\link{campsQ2}}, \code{\link{roadsGRL}}\cr
#'JS libraries\cr
#'Leaflet 0.7.7: \href{http://leafletjs.com/}{Leaflet}\cr
#'The kartena projection plugin for leaflet: \href{http://kartena.github.io/Proj4Leaflet/}{Proj4Leaflet}\cr
#'
#'
#'@author Chris Reudenbach
#'
#'@examples
#' \dontrun{
#' ##  packages
#'  require(mapview)
#'  require(raster)
#'
#'  ## load data of the arctic stations
#'  data("campsQ2")
#'
#'  ## load Greenlands roads data from geofabrik
#'  data("roadsGRL")
#'
#'  ## We need to define online data providers. this is a bit tricky and yields sometimes just frustration...
#'  ## under details you'll find a link for further explanations.
#'  ## The map.typeList as provided contains five examples. 2 OSM and 3 WMS services
#'  data("map.typeList")
#'
#' ### finally let's start mapping
#'
#' ## map the antarctic facilities data using the NASA EarthData tiles
#' projView(campsQ2, map.types= "map.typesList$NASA")
#'
#' ## same as before but now using the visEarthPole function as a "plugin"
#' projView(campsQ2, map.types= "map.typesList$NASA",
#'                   internalList =TRUE,
#'                   externalList = c("arctic-nasa","visEarthPole(groupList='1000',dateString='2014-02-04',createList = TRUE)"))
#'
#' ### similiar job in the North
#'
#' ## map Greenland's roads using the CAFF tiles for sea and landsurface temerature
#' mapview::projView(roadsGRL, map.types= "map.typesList$CAFF")
#'
#' ## again Greenland's roads using the HAV map tiles
#' mapview::projView(roadsGRL, map.types= "map.typesList$HAV")
#'
#' ## reproject HAV and roadsGRL to EPSG:3995
#' map.typesList$HAV$params$t_srs <- "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
#' map.typesList$HAV$params$t_epsg <- "EPSG:3995"
#' mapview::projView(roadsGRL, map.types= "map.typesList$HAV")
#'
#' ## move center
#' map.typesList$HAV$params$mapCenter$cLon="70"
#' map.typesList$HAV$params$mapCenter$cLat="15"
#' mapview::projView(roadsGRL, map.types= "map.typesList$HAV")
#'
#' }
#'@name projView
#'@export projView
#'@rdname projView


projView<- function( x=NULL,
                     zcol =NULL,
                     color=mapviewGetOption("raster.palette")(256),
                     na.color=mapviewGetOption("na.color"),
                     map.types=NULL,
                     cex = 6,
                     lwd = 1,
                     alpha = 0.7,
                     legend = FALSE,
                     legend.opacity = 1,
                     verbose = mapviewGetOption("verbose"),
                     use.layer.names = FALSE,
                     layer.name = deparse(substitute(x,
                                                     env = parent.frame())),

                     trim = TRUE,
                     popup = popup,
                     internalList =FALSE,
                     externalList=NULL)
{


  # creates the temporyry directory for the CRS, data and layer transfer
  tmpPath<- createTempDataTransfer()
  if (! class(map.types) == "list") {
    ovl<-eval(parse(text = map.types))}
  else {
      ovl<-map.types
  }

  #deparse(substitute (ovl))
  geoLatLon<- FALSE
  estimateMapCenter=FALSE
  noBounds<-FALSE
  calcRes<-FALSE
  res<-NULL
  # redefine vars not neccessry but less confusing
    minx<-as.numeric(ovl$params$ovlBounds$minx)
    miny<-as.numeric(ovl$params$ovlBounds$miny)
    maxx<-as.numeric(ovl$params$ovlBounds$maxx)
    maxy<-as.numeric(ovl$params$ovlBounds$maxy)
    if (length(minx) == 0 || length(maxx) == 0 || length(miny) == 0 || length(maxy) == 0){
      noBounds<-TRUE
    } else if ( ((minx > -360 & minx < 360)|| (maxx < 360 & maxx > -360) || (miny > -360 & miny < 360) || maxy < 360 & maxy > -360 ))
      {geoLatLon<-TRUE }
  if (! is.null(ovl$params$origin$olx) || ! is.null(ovl$params$origin$oly)) {
  ulx<-as.numeric(ovl$params$origin$olx)
  uly<-as.numeric(ovl$params$origin$oly)
  } else{
    cat("No upper left corner provided. Can not head on. ")
    return()
  }
  # get tileSize if provided otherwise assume 256 pix
  if (! is.null(ovl$tileSize)) {
    tileSize<-as.numeric(ovl$tileSize)
    calcRes<-TRUE
  } else {
    cat("No tileSize provided. use default 256.")
    tileSize = 256
  }
  if (! is.null(ovl$params$zoomLevels)) {
    maxZoom<-as.numeric(ovl$params$zoomLevels)
  } else {
    maxZoom = 18
  }
  if (! is.null(ovl$params$initialResolution)) {
    if (length(ovl$params$initialResolution)>1){
      initialRes<-paste(ovl$params$initialResolution,collapse = ",")
    } else {
      initialRes<-as.numeric(ovl$params$initialResolution)
      calcRes=TRUE
    }
  } else {
    cat("No initialResolution provided. use default 256.")
    initialRes = 256
  }

  if (! is.null(ovl$params$t_epsg)) {
    t_epsg<-ovl$params$t_epsg
  } else {
    cat(" No target EPSG code provided. Don't know how to project the map...")
    return()
  }
  if (! is.null(ovl$params$t_srs)) {
    t_srs<-ovl$params$t_srs
  } else {
    cat(" No target SRS provided. Don't know how to project the map...")
    return()
  }

  if (!is.null(ovl$params$mapCenter$cLat) || !is.null(ovl$params$mapCenter$cLon)){
    mCLon<-ovl$params$mapCenter$cLon
    mCLat<-ovl$params$mapCenter$cLat
  } else {
    cat("WARNING: No MapCenter provided - I will try to estimate it but may fail...")
    estimateMapCenter <- TRUE
  }
  if (! is.null(ovl$params$initialZoom)) {
    initialZoom<-ovl$params$initialZoom
  } else {
    initialZoom<-3
    cat(" No initialZoom. I just set it arbitrary....")
    return()
  }





  # DEFINE GLOBAL VARIABLES


  #create a bounding box of the overlay extent
  # note we have to do it from the LAT Lon Bounds
  # but leaflet needs the targes srs.
  # Unfortunately in most cases this is set to the mapserver bounds
  # so we calculate it if possible from the original bounds

  if ( geoLatLon) {

    tmpPoly = Polygon(cbind(c(minx,minx,maxx,maxx,minx),c(miny,maxy,maxy,miny,miny)))
    tmpPoly = Polygons(list(tmpPoly), ID = "bbox")
    bbox = SpatialPolygons(list(tmpPoly))

    proj4string(bbox) <-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    bbox <- sp::spTransform(bbox, crs(t_srs))
    xt<-extent(bbox)
    # create the "bounds" string
    bounds <- paste0("bounds: L.bounds([",xt@ymax,",",xt@xmin,"],[",xt@ymin,",",xt@xmax,"])")
    origin <- paste0("origin: [",xt@ymax,",",xt@xmax,"]")

    res<-max(maxx-minx,maxy-miny)
  } else {
    bounds <- paste0("bounds: L.bounds([",miny,",",minx,"],[",maxy,",",maxx,"])")
    origin <- paste0("origin: [",ulx,",",uly,"]")

  }
  # estimate resolution will be overriden if  length(resStart) > 1
  if  (calcRes)
  {
    #initRes<-log(resStart, base = 2)
    #if (initRes <= 0) {initRes <-1}
    #tmpres<-2^(initRes:(maxZoom + initRes))
    #tmpres<-sort(tmpres,decreasing = TRUE)
    #tmpres<- paste(tmpres,collapse = ",")
    if (tileSize != initialRes) {
      div<-tileSize/initialRes*tileSize
    } else {
        div<-tileSize
    }


    if (is.null(res)) {res<-abs(ulx) + abs(uly)}
    maxResolution <- res / div
    resolution<- list()
    for ( i in seq(0,maxZoom)){
      resolution[i+1] <- maxResolution /  2^i
    }
    tmpres<-paste(resolution,collapse = ",")
    # create CRS string
    LProjResolution<-paste0("{resolutions: [",tmpres,"],")

  } else
  {
    # create CRS string
    LProjResolution<-paste0("{resolutions: [",initialRes,"],")
  }
  # create CRS string
  LProjEpsgSrs<-paste0('var crs =  new L.Proj.CRS("',t_epsg,'","',t_srs,'",')

  # create CRS string
  CRSvarMapCenter<-paste0('var mapCenter = [',mCLat,',',mCLon,'];')

  # create CRS string
  CRSinitialZoom<-paste('var initialZoom = ',initialZoom,';')

  # assign tmpfilename for CRS definition
  tmpCRS <- paste0(tmpPath,"/crs.js")
  # write the proj4leaflet CRS
  write(CRSinitialZoom,tmpCRS,append = TRUE)
  write(CRSvarMapCenter,tmpCRS,append = TRUE)
  write(LProjEpsgSrs,tmpCRS,append = TRUE)
  write(LProjResolution,tmpCRS,append = TRUE)

  if (!noBounds){
    write(paste0(origin,","),tmpCRS,append = TRUE)
    write(bounds,tmpCRS,append = TRUE)
  } else {
    write(paste0(origin),tmpCRS,append = TRUE)
    }
  write(paste0('});'),tmpCRS,append = TRUE)

  if (!internalList) {
    # start with parsing map.type list
    # define a temp filename for the layers
    layFn <- paste0(tmpPath,"/layers.js")
    write("function loadLayers(){",layFn,append = TRUE)
    write("var baseLayers = {};",layFn,append = TRUE)
    write("var overlayLayers = {};",layFn,append = TRUE)
    if(ovl$service == "WMS"){


      for (i in seq(1,length(unlist(ovl$layers)), by = 1)) {
        for (j in seq(2,length(ovl)-1, by = 1)){
          if (j == 2 & i == 1 ){
            v <- paste0("var defaultLayer  =  ",attributes(ovl[j]),"('",ovl$L.tileLayer.wms,"',{")
            #v <- paste0("baseLayers['",ovl$layer[i], "'] =  ",attributes(ovl[j]),"('",ovl$L.tileLayer.wms,"',{")
          }
          else if (j == 2 & i>1){
            v <- paste0("overlayLayers['",attributes(ovl$layers[i]), "'] =  ",attributes(ovl[j]),"('",ovl$L.tileLayer.wms,"',{")
  #          v <- paste0("overlayLayers['",ovl$layers$layer[i], "'] =  ",attributes(ovl[j]),"('",ovl$L.tileLayer.wms,"',{")
          }
          else {
            #if (j != length(ovl)-1){
              if (attributes(ovl[j]) == "layers"){
                v <- paste0(attributes(ovl[j]),": '",attributes(ovl$layers[i]),"',")
              }
              else if (attributes(ovl[j]) == "format" ||
                       attributes(ovl[j]) == "attribution"){
                v <- paste0(attributes(ovl[j]),': "', ovl[j],'",')
              }
              else {
                v <- paste0(attributes(ovl[j]),": ", ovl[j],",")
              }
            #}
            #else {
            #  v <- paste0(attributes(ovl[j]),': "', ovl[j],'"});')
            #}
          }
          write(v,layFn,append = TRUE)
        }
        write("});",layFn,append = TRUE)
      }
    }

    if(ovl$service == "OSM"){
      layerNumber<- length(ovl$layer)
    #  if (grep(ovl$layer[layerNumber],pattern = "{z}",fixed=TRUE ) & grep(ovl$layer$layer[layerNumber],pattern = "{x}",fixed=TRUE ))
     # {
      #  layerNumber<-layerNumber-1
      #}
      for (i in seq(1,layerNumber, by = 1)) {        # for all layers
        for (j in seq(2,length(ovl)-1, by = 1)) {    # parse starting at pos 2 because 1 = service
          if (j == 2 & i==1) {                       # if first layer do
            #    if (ovl$service == "NASA") {
            url<-makeUrl(i,ovl)                    # parse the layer fragments for building the url
            #} else {
            #  url<-ovl$L.tileLayer
            #}
            # create the string for baseLayers
            v <- paste0("var defaultLayer  =  ",attributes(ovl[j]),"('",url,"',{")
          }
          else if (j == 2 & i>1){                    #for all other layers make overlayLayers
            url<-makeUrl(i,ovl)
            v <- paste0("overlayLayers['",attributes(ovl$layer[i]), "'] =  ",attributes(ovl[j]),"('",url,"',{")
          }
          else {

           # if (j != length(ovl)-2){
              #if (attributes(ovl[j]) == "layer" || attributes(ovl[j]) == "params" )
              #{
              #  v<- NULL # <- paste0(attributes(ovl[j]),': "', ovl[j],'",')
              #  }
            if ( attributes(ovl[j]) == "layer" ||
                 attributes(ovl[j]) == "params" ) {
              v<-NULL}

            else if (attributes(ovl[j]) == "subdomains" ||
                     attributes(ovl[j]) == "format" ||
                     attributes(ovl[j]) == "attribution" ){
              v  <- paste0(attributes(ovl[j]),': "', ovl[j],'",')

            }
            else {
              v <- paste0(attributes(ovl[j]),": ", ovl[j],",")

            }
          #  }
          #  else {
          #    v <- paste0(attributes(ovl[j]),":", ovl[j],",")
          #  }
          }

          if (!is.null(v)){write(v,layFn,append = TRUE)}
          v<-NULL
        }
        write("});",layFn,append = TRUE)
      }
    }
    write(paste0("baseLayers['",attributes(ovl$layer[1]), "'] = defaultLayer;"),layFn,append = TRUE)
    write(paste0("return{overlayLayers: overlayLayers, baseLayers: baseLayers,defaultLayer: defaultLayer }"),layFn,append = TRUE)
    write("};",layFn,append = TRUE)
  } else if (externalList[1] == "arctic-nasa"){
    extList<-eval(parse(text = externalList[2]))
  }




  #  write("var map = new L.map(el, {",tmptL,append = TRUE)
  #  write("crs : crs,",tmptL,append = TRUE)
  #  write("continuousWorld: true,",tmptL,append = TRUE)
  #  write("worldCopyJump: true",tmptL,append = TRUE)
  #  write("})",tmptL,append = TRUE)

  # check if an overlay vector datase (x) exist and add it
  if (!is.null(x)) {
    ##get epsg code and proj4 string from vector file
    #     string<-unlist(strsplit(x@proj4string@projargs, split='+', fixed=TRUE))
    #     epsg<-string[grepl("init",string)]
    #     unlist(strsplit(epsg, split=':', fixed=TRUE))[2]
    #     s_srs<-x@proj4string@projargs
    #     s_epsg <- paste0("epsg:",unlist(strsplit(epsg, split=':', fixed=TRUE))[2])
    # get extent from vector file
    # recalculate center of map in lat lon
    #ulc<-gdaltransform(s_srs=s_srs, t_srs=t_srs,coords=matrix(c(xt@xmin,xt@ymax), ncol = 2))[1:2]
    #lrc<-gdaltransform(s_srs=s_srs, t_srs=t_srs,coords=matrix(c(xt@xmax,xt@ymin), ncol = 2))[1:2]
    #bounds <- paste0("[",ulc[1],",",ulc[2],"],[",lrc[1],",",lrc[2],"]")

    # get map center and extent
    xtr <- mapview:::spCheckAdjustProjection(x)
    xtrLL<-extent(xtr)
    if ( estimateMapCenter ){
      mapCenterLat <- (xtrLL@ymax-xtrLL@ymin) * 0.5  + xtrLL@ymin
      mapCenterLon <- (xtrLL@xmax-xtrLL@xmin) * 0.5 + xtrLL@xmin
    }
    # optional calculate the resolution as derived by local tiles and zoom level

    # NOW transform x to target projection
    x <- sp::spTransform(x, CRS(paste0("+init=epsg",substr(t_epsg, 5, nchar(t_epsg))," ",t_srs)))

    # define jsonpath
    tmpJSON <-paste(tmpPath, ".jsondata", sep=.Platform$file.sep)
    # check and correct if sp object is of type dataframe
    x <- mapview:::toSPDF(x)

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

  #layer.name = deparse(substitute(map.types,env = parent.frame()))
  #use.layer.names =TRUE

  # create list of user data that is passed to the widget
  lst_x <- list(data  = 'undefined',
                layername=layer.name,
                overlayLayer=deparse(substitute(x,
                                                env = parent.frame())),
                zoom = initialZoom,
                t_epsg=t_epsg,
                t_srs=t_srs,
                tilesize=tileSize,
                color = mapview:::col2Hex(color),
                #refpoint=refpoint,
                html = getPopupStyle(),
                opacity = alpha,
                weight = lwd,
                values = x@data,
                cex=cex,
                internalList =internalList
  )

  if (internalList) {

    lst_x <- list(data  = 'undefined',
                  attribution=extList$attribution,
                  scale= extList$scale,
                  ulc= extList$ulc,
                  dateString = extList$dateString,
                  layer = extList$layer,
                  layername=extList$layername,
                  overlayLayer=deparse(substitute(x,
                                                      env = parent.frame())),
                  zoom = initialZoom,
                  t_epsg=t_epsg,
                  t_srs=t_srs,
                  tilesize=tileSize,
                  color = mapview:::col2Hex(color),
                  #refpoint=refpoint,
                  html = getPopupStyle(),
                  opacity = alpha,
                  weight = lwd,
                  values = x@data,
                  cex=cex,
                  internalList =internalList
    )

  }

  #lst_x<-c(lst_x, extList)}
  # creating the widget
  projViewInternal(f = paste0(tmpPath,"/crs.js") , jFn = paste(tmpPath, "jsondata", sep=.Platform$file.sep),  tmptL= paste0(tmpPath,"/layers.js"),x = lst_x)

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
# create the url for the nasa vis project
makeUrl <- function (i,ovl){
  fragPath<-"/"
  if (length(ovl$layer) >=1){
  tmpLayer<-ovl$layer[[i]]
  tmpLayer<-unlist(tmpLayer)
  fragPath<-NULL
  for (k in seq(1,length(tmpLayer), by = 1)) {
    fragPath<-paste0(fragPath,tmpLayer[k],"/")
  }
  }
  fragPath<-substr(fragPath, 1, nchar(fragPath)-1)
  extension<- strsplit(unlist(ovl$format[[1]]),"/",fixed = TRUE)[[1]][2]
  #layerName<-ovl$layer[[1]][[i]][1]
  url<-paste0(ovl$L.tileLayer,fragPath,".",extension)
  return(url)
}

