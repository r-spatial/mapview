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
#'@details For all other information  please have a look at the vignette or at \href{http://gisma.github.io/projView/projView1_0_9.html#some-examples}{projView}
#'
#'@references
#'\href{https://wiki.earthdata.nasa.gov/display/GIBS}{NASA EOSDIS GIBS}\cr
#'\href{http://kartena.github.io/Proj4Leaflet/}{Proj4Leaflet}
#'


#'
#'@author Chris Reudenbach
#'
#'@examples
#' \dontrun{
#' ##  packages
#'  require(curl)
#'  require(rgdal)
#'
#'  ## load data of the arctic stations
#'  data("campsQ2")
#'
#'  ## download Greenland data from geofabrik
#'  curl_download("http://download.geofabrik.de/north-america/greenland-latest.shp.zip", destfile=paste0(tmpDir,"/geofabrikGREEN.zip"),  quiet = FALSE, mode = "wb")
#'  unzip(paste0(tmpDir,"/geofabrikGREEN.zip"), exdir=tmpDir)
#'  ## import natural
#'  naturalGREEN<- rgdal:::readOGR(dsn =path.expand(tmpDir), layer="natural")
#'
#'  ## first we have to define some online data providers. this is a bit tricky and yields sometimes just frustration...
#'  ## please have a look at the vignette for further explanations. In the example you'll find
#'  ## a typical WMS service and a typical OSM tile server. For your preferred list copy and paste
#'  ## the values of your tileserver/WMS service into the list elements.
#'  ## for convenience you also can add this new sublist to the master list for further use.
#'
#' ovlLayer<-list( HAV=list(service="WMS", # typical WMS service
#'                          L.tileLayer.wms="http://geodatatest.havochvatten.se/geoservices/ows",
#'                          format="image/png",
#'                          layers=list(layer=list("hav-bakgrundskartor:hav-grundkarta")),
#'                          minZoom="0",
#'                          maxZoom="14",
#'                          continiuousWorld="true",
#'                          transparent="false",
#'                          attribution=" | &copy; <a href='https://www.havochvatten.se/kunskap-om-vara-vatten/kartor-och-geografisk-information/karttjanster.html'>Havs- och vattenmyndigheten (Swedish Agency for Marine and Water Management)</a> | Projection: <a href='http://spatialreference.org/ref/epsg/3006'> EPSG:3006</a>' | <a href='http://download.geofabrik.de/'> geofabrik/osm</a>",
#'                          params=list(t_epsg="EPSG:3575",
#'                                      t_srs="+proj=laea +lat_0=90 +lon_0=10 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ",
#'                                      mapCenter=list(cLat="90",
#'                                                     cLon="90"),
#'                                      initialZoom="0",
#'                                      zoomLevels="12",
#'                                      resolution="8",
#'                                      ovlBounds=list(minx="-4462340.35076383",
#'                                                     miny="-4467019.64923622",
#'                                                     maxx="4467019.64923617",
#'                                                     maxy="4462340.35076378"),
#'                                      origin=list(olx="-4462340.35076383",
#'                                                  oly="4462340.35076378"),
#'                                      relUrl=""
#'
#'                          )), # end of CAFF list
#'
#'                 CAFF=list(service="WMS", # typical WMS service templete
#'                           L.tileLayer.wms="http://dev.caff.is:8080/geoserver/ows",
#'                           format="image/png",
#'                           layers=list(layer=list("arctic_sdi:LandSurfaceTemperature","arctic_sdi:SeaSurfaceTemperature")),
#'                           minZoom="0",
#'                           maxZoom="12",
#'                           continiuousWorld="true",
#'                           transparent="true",
#'                           attribution=" | &copy; <a href=' http://www.arcticbiodiversity.is>Arctic Biodiversity Assessment</a> |<a href='http://download.geofabrik.de/'> geofabrik/osm</a> | Projection: <a href='http://spatialreference.org/ref/epsg/3575'> EPSG:3575</a>",
#'                           params=list(t_epsg="EPSG:3575",
#'                                       t_srs="+proj=laea +lat_0=90 +lon_0=10 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ",
#'                                       mapCenter=list(cLat="90",
#'                                                      cLon="90"),
#'                                       initialZoom="0",
#'                                       zoomLevels="12",
#'                                       resolution="8",
#'                                       ovlBounds=list(minx="-4462340.35076383",
#'                                                      miny="-4467019.64923622",
#'                                                      maxx="4467019.64923617",
#'                                                      maxy="4462340.35076378"),
#'                                       origin=list(olx="-4462340.35076383",
#'                                                   oly="4462340.35076378"),
#'                                       relUrl=""
#'
#'                           )), # end of CAFF list
#'                 # here we start a new map.types input list this time for OSM tiles
#'                 NASA=list(service="NASA",
#'                           L.tileLayer="https://map1{s}.vis.earthdata.nasa.gov/wmts-antarctic/",
#'                           layer=list(layer=list(list("BlueMarble_ShadedRelief_Bathymetry", path=list("EPSG3031_500m")),
#'                                                 list("AMSR2_Sea_Ice_Brightness_Temp_6km_89H",path= list("2014-02-04",
#'                                                                                                         "EPSG3031_1km")),
#'                                                 list(" MODIS_Terra_Snow_Cover",path= list("2014-02-04",
#'                                                                                           "EPSG3031_1km"))
#'                           )),
#'                           format="image/jpg",
#'                           tileSize="512",
#'                           subdomains="abc",
#'                           noWrap ="true",
#'                           attribution="<a href='https://wiki.earthdata.nasa.gov/display/GIBS'> NASA EOSDIS GIBS</a> &nbsp;|| &nbsp; <a href='https://github.com/kartena/Proj4Leaflet'> Proj4Leaflet</a> | Projection: <a href='http://spatialreference.org/ref/epsg/wgs-84-antarctic-polar-stereographic/'> EPSG3031</a>",
#'                           params=list(t_epsg="EPSG:3031",
#'                                       t_srs="+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
#'                                       mapCenter=list(cLat="-90",
#'                                                      cLon="0"),
#'                                       initialZoom="0",
#'                                       zoomLevels="5",
#'                                       resolution="256",
#'                                       ovlBounds=list(minx="-4194304",
#'                                                      miny="-4194304",
#'                                                      maxx="4194304",
#'                                                      maxy="4194304"),
#'                                       origin=list(olx="-4194304",
#'                                                   oly="4194304"),
#'                                       relUrl=""
#'
#'                           )) # end of NASA list
#' ) # end of total list
#'

#'
#'
#' ### now let's start mapping
#' #load the stations and the defined NASA Layers
#' projView(campsQ2, , map.types= "ovlLayer$NASA")
#'
#' ### use the visEarthPole function as a plugin
#' projView(campsQ2, map.types= "ovlLayer$NASA",
#'                   internalList =TRUE,
#'                   externalList = c("arctic-nasa","visEarthPole(groupList='1000',dateString='2014-02-04',createList = TRUE)"))
#' ### it also works for the North Pole
#' ### use Greenland and online tiles from the CAFF
#' mapview::projView(naturalGREEN, , map.types= "ovlLayer$CAFF")
#'
#' # again Greenland with online tiles from HAV
#' mapview::projView(naturalGREEN, , map.types= "ovlLayer$HAV")
#'
#' ## reproject HAV and Greenland to EPSG:3995
#' ovlLayer$HAV$params$t_srs <- "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
#' ovlLayer$HAV$params$t_epsg <- "EPSG:3995"
#' mapview::projView(naturalGREEN, , map.types= "ovlLayer$HAV")
#'
#' # move center
#' ovlLayer$HAV$params$mapCenter$cLon="70"
#' ovlLayer$HAV$params$mapCenter$cLat="15"
#' mapview::projView(naturalGreen, , map.types= "ovlLayer$HAV")
#'
#'
#'
#' }
#'@name projView
#'@export projView
#'@rdname projView
#'@import rgdal
#'@import gdalUtils

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
  #deparse(substitute (ovl))
  # redefine vars not neccessry but less confusing
  minx<-as.numeric(ovl$params$ovlBounds$minx)
  miny<-as.numeric(ovl$params$ovlBounds$miny)
  maxx<-as.numeric(ovl$params$ovlBounds$maxx)
  maxy<-as.numeric(ovl$params$ovlBounds$maxy)
  ulx<-as.numeric(ovl$params$origin$olx)
  uly<-as.numeric(ovl$params$origin$oly)
  # get tileSize if provided otherwise assume 256 pix
  if (! is.null(ovl$tileSize)) {
    tileSize<-as.numeric(ovl$tileSize)
  } else {
    tileSize = 256
  }
  if (! is.null(ovl$params$zoomLevels)) {
    maxZoom<-as.numeric(ovl$params$zoomLevels)
  } else {
    maxZoom = 18
  }
  if (! is.null(ovl$params$resolution)) {
    resStart<-as.numeric(ovl$params$resolution)
  } else {
    resStart = maxZoom
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
  estimateMapCenter=FALSE
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
  if ( (minx < -360 || maxx > 360 || miny < -360 || maxy > 360)) {
    bounds <- paste0("bounds: L.bounds([",miny,",",minx,"],[",maxy,",",maxx,"])")
    origin <- paste0("origin: [",ulx,",",uly,"]")

  } else{

    tmpPoly = Polygon(cbind(c(minx,minx,maxx,maxx,minx),c(miny,maxy,maxy,miny,miny)))
    tmpPoly = Polygons(list(tmpPoly), ID = "bbox")
    bbox = SpatialPolygons(list(tmpPoly))

    proj4string(bbox) <-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    bbox <- sp::spTransform(bbox, crs(t_srs))
    xt<-extent(bbox)
    # create the "bounds" string
    bounds <- paste0("bounds: L.bounds([",xt@ymax,",",xt@xmin,"],[",xt@ymin,",",xt@xmax,"])")
    origin <- paste0("origin: [",xt@ymax,",",xt@xmax,"]")

  }
  # estimate resolution will be overriden if  length(resStart) > 1
  if  (length(resStart) == 1)
  {
    initRes<-log(resStart, base = 2)
    if (initRes <= 0) {initRes <-1}
    tmpres<-2^(initRes:(maxZoom + initRes))
    tmpres<-sort(tmpres,decreasing = TRUE)
    tmpres<- paste(tmpres,collapse = ",")
    # create CRS string
    LProjResolution<-paste0("{resolutions: [",tmpres,"],")

  } else
  {
    # create CRS string
    LProjResolution<-paste0("{resolutions: [",resStart,"],")
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
  #  if(setBound)
  #  {
  write(paste0(origin,","),tmpCRS,append = TRUE)
  write(bounds,tmpCRS,append = TRUE)
  # }
  #  else
  #  {
  #    write(origin,tmpCRS,append = TRUE)
  #  }
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
            v <- paste0("baseLayers['",ovl$layers$layer[i], "'] =  ",attributes(ovl[j]),"('",ovl$L.tileLayer.wms,"',{")
          }
          else if (j == 2 & i>1){
            v <- paste0("overlayLayers['",ovl$layers$layer[i], "'] =  ",attributes(ovl[j]),"('",ovl$L.tileLayer.wms,"',{")
          }
          else {
            if (j != length(ovl)-1){
              if (attributes(ovl[j]) == "layers" )
              {ovl$layers$layer[i]
                v <- paste0(attributes(ovl[j]),": '",ovl$layers$layer[i],"',")
              }
              else if (attributes(ovl[j]) == "format"){
                v <- paste0(attributes(ovl[j]),": '", ovl[j],"',")
              }
              else {
                v <- paste0(attributes(ovl[j]),": ", ovl[j],",")
              }
            }
            else {
              v <- paste0(attributes(ovl[j]),': "', ovl[j],'"});')
            }
          }
          write(v,layFn,append = TRUE)
        }
      }
    }

    if(ovl$service == "NASA"){

      for (i in seq(1,length(ovl$layer$layer), by = 1)) {
        for (j in seq(2,length(ovl)-1, by = 1)){
          if (j == 2 & i==1){
            # create pathstring
            if (ovl$service == "NASA") {
              url<-makeNasaUrl(i,ovl)
            } else {
              url<-ovl$L.tileLayer
            }

            v <- paste0("baseLayers['",ovl$layer$layer[[i]][1], "'] =  ",attributes(ovl[j]),"('",url,"',{")
          }
          else if (j == 2 & i>1){
            # create pathstring
            if (ovl$service == "NASA") {
              url<-makeNasaUrl(i,ovl)
            } else {
              url<-ovl$L.tileLayer
            }
            v <- paste0("overlayLayers['",ovl$layer$layer[i], "'] =  ",attributes(ovl[j]),"('",url,"',{")
          }
          else {
            if (j != length(ovl)-2){
              if (attributes(ovl[j]) == "layer" )
              {
                v <- paste0(attributes(ovl[j]),": '",ovl$layer$layer[[i]][1],"',")
              }
              else if (attributes(ovl[j]) == "format" || attributes(ovl[j])== "subdomains" || attributes(ovl[j])== "attribution"){
                v <- paste0(attributes(ovl[j]),': "', ovl[j],'",')
              }
              else {
                v <- paste0(attributes(ovl[j]),": ", ovl[j],",")
              }
            }
            else {
              v <- paste0(attributes(ovl[j]),":", ovl[j],",")
            }
          }

          write(v,layFn,append = TRUE)
        }
        if (i==999){write("}).addTo(map);",layFn,append = TRUE)}else{write("});",layFn,append = TRUE)}
        #write("});",layFn,append = TRUE)
      }
    }
    write(paste0("return{overlayLayers: overlayLayers, baseLayers: baseLayers,defaultLayer: '",ovl$layer$layer[[1]][1],"' }"),layFn,append = TRUE)
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
    x <- sp::spTransform(x, CRSobj = t_srs)

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
makeNasaUrl <- function (i,ovl){

  t<-unlist(ovl$layer$layer[[i]][2])
  fragPath<-"/"
  for (k in seq(1,length(t), by = 1)) {
    fragPath<-paste0(fragPath,t[k],"/")
  }
  extension<- strsplit(unlist(ovl$format[[1]]),"/",fixed = TRUE)[[1]][2]
  layerName<-ovl$layer[[1]][[i]][1]
  url<-paste0(ovl$L.tileLayer,layerName,"/default",fragPath,"{z}/{y}/{x}.",extension)
  return(url)
}
