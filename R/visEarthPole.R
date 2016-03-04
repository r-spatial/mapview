if (!isGeneric('visEarthPole')) {
  setGeneric('visEarthPole', function(x, ...)
    standardGeneric('visEarthPole'))
}
#' usecase for local leaflet projections
#'
#' @description visEarthPole is an usecase interface to the Global Imagery
#'   Browse Services - GIBS  Basically the projection at the South Pole is EPSG
#'   3031 and somehow a perfect test implementation of proj4leaflet.
#'   It is up to now VERY basic and just demonstrate the possibilities of using it along with mapview.
#'
#'@usage visEarthPole(dateString="2011-10-04", layerList=c(12,10,11),groupList=NULL,scale=scale500,zoom=5)
#'
#'@param dateString a date in the convienient format "2011-10-04". Basically the retrieve of non existing time slots is corrected to the next existing.
#'@param layerList default is (12,10,11). You will find 32 layers  to choose. See Details for more info
#'@param groupList default = "500" there are two more "250" and "1000" predifined  group list according to the resolution of the data . if you choose "burst" you will get all layers.
#'@param scale set scale groups according to the resolution will be removed options are "scale250","scale500" "scale1000".
#'@param zoom set zoom level maximum is 5
#'
#'@details Layerlisting for details pleas look at \url{https://wiki.earthdata.nasa.gov/display/GIBS/GIBS+Available+Imagery+Products}\cr
#'[1]  "AMSR2_Sea_Ice_Concentration_12km" \cr
#'[2]  "AMSR2_Sea_Ice_Concentration_25km" \cr
#'[3]  "AMSR2_Sea_Ice_Brightness_Temp_6km_89H" \cr
#'[4]  "AMSR2_Sea_Ice_Brightness_Temp_6km_89V" \cr
#'[5]  "AMSRE_Sea_Ice_Concentration_12km" \cr
#'[6]  "AMSRE_Snow_Depth_Over_Ice" \cr
#'[7]  "AMSRE_Sea_Ice_Concentration_25km" \cr
#'[8]  "AMSRE_Sea_Ice_Brightness_Temp_89H" \cr
#'[9]  "AMSRE_Sea_Ice_Brightness_Temp_89V" \cr
#'[10]  "BlueMarble_NextGeneration" \cr
#'[11]  "BlueMarble_ShadedRelief" \cr
#'[12]  "BlueMarble_ShadedRelief_Bathymetry" \cr
#'[13]  "Coastlines" \cr
#'[14]  "Graticule" \cr
#'[15]  "MODIS_Terra_Snow_Cover" \cr
#'[16]  "MODIS_Terra_Sea_Ice" \cr
#'[17]  "MODIS_Terra_Brightness_Temp_Band31_Day" \cr
#'[18]  "MODIS_Terra_Brightness_Temp_Band31_Night" \cr
#'[19]  "MODIS_Terra_CorrectedReflectance_TrueColor" \cr
#'[20]  "MODIS_Terra_CorrectedReflectance_Bands367" \cr
#'[21]  "MODIS_Terra_CorrectedReflectance_Bands721" \cr
#'[22]  "MODIS_Aqua_Snow_Cover" \cr
#'[23]  "MODIS_Aqua_Sea_Ice" \cr
#'[24]  "MODIS_Aqua_Brightness_Temp_Band31_Day" \cr
#'[25]  "MODIS_Aqua_Brightness_Temp_Band31_Night" \cr
#'[26]  "MODIS_Aqua_CorrectedReflectance_TrueColor" \cr
#'[27]  "MODIS_Aqua_CorrectedReflectance_Bands721" \cr
#'[28]  "SCAR_Land_Mask" \cr
#'[29]  "SCAR_Land_Water_Map \cr"
#'[30]  "VIIRS_SNPP_CorrectedReflectance_TrueColor" \cr
#'[31]  "VIIRS_SNPP_CorrectedReflectance_BandsM11-I2-I1" \cr
#'[32]  "VIIRS_SNPP_CorrectedReflectance_BandsM3-I3-M11" \cr
#'
#'@references
#'\url{https://wiki.earthdata.nasa.gov/display/GIBS/Global+Imagery+Browse+Services+-+GIBS}\cr
#'\url{https://wiki.earthdata.nasa.gov/display/GIBS/GIBS+Available+Imagery+Products}\cr
#'\url{http://map1.vis.earthdata.nasa.gov/twms-antarctic/twms.cgi?request=GetTileService}\cr
#'\url{https://github.com/kartena/Proj4Leaflet}\cr

#' @author
#' Chris Reudenbach
#'
#' @examples
#' \dontrun{
#'
#'  visEarthPole(groupList="1000",dateString="2014-02-04")
#'}
#'@name visEarthPole
#'@export visEarthPole
#'@rdname visEarthPole

library(raster)


visEarthPole<- function(dateString="2011-10-04",
                        layerList=c(12,10,11),
                        groupList=NULL,
                        scale=scale500,
                        zoom=5)
{

  tileSize=512
  epsg3031Code <- "urn:ogc:def:crs:EPSG::3031"
  epsg3031String <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  attribution <-"<a href='https://wiki.earthdata.nasa.gov/display/GIBS'> NASA EOSDIS GIBS</a> &nbsp;|| &nbsp; <a href='https://github.com/kartena/Proj4Leaflet'> Proj4Leaflet</a> | Projection: <a href='http://spatialreference.org/ref/epsg/wgs-84-antarctic-polar-stereographic/'> EPSG3031</a>"
  scale1000 <- c(2048.0,1024.0,512.0,256.0)
  scale500  <- c(4096.0,2048.0,1024.0,512.0,256.0)
  scale250  <- c(8192.0,4096.0,2048.0,1024.0,512.0,256.0)
  ulc <- c(-4194304,4194304)

  # calculate zoom level from extent of input raster
  if (!is.null(groupList)) {
    if (groupList == "500") {
      layerList <- c(12,10,11,15,22)
      zoom<-4
      scale=scale500
    }
    if (groupList == "250") {
      layerList <- c(12,13,14,28,29,30,31,32,19,20,21,26,27)
      zoom<-5
      scale=scale250
    }
    if (groupList == "1000") {
      layerList <- c(12,1,2,3,4,5,6,7,8,9,16,17,18,23,24,25)
      zoom<-3
      scale=scale1000
    }
    if (groupList == "burst") {
      layerList <- c(12,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32)
      zoom<-5
      scale=scale1000
    }
  }
  tmp<-getParams(dateString,layerList)
  url<-tmp[[1]]
  layername<-tmp[[2]]
  drop1<-!is.na(url)
  drop2<-!is.na(layername)
  urls<-url[drop1]
  layernames<-layername[drop2]
  baselayerPos<-grep("BlueMarble", layernames,fixed=TRUE)
  tmplyr<-layernames[baselayerPos]
  tmpurls<-urls[baselayerPos]
  layernames[baselayerPos]<- layernames[1]
  urls[baselayerPos]<- urls[1]
  layernames[1]<- tmplyr
  urls[1]<- tmpurls

  leaf.height <- mapviewGetOption("leafletHeight")
  leaf.hidth <- mapviewGetOption("leafletWidth")
  map.types = mapviewGetOption("basemaps")
  layer.name = deparse(substitute(x,env = parent.frame()))
  use.layer.names =TRUE
  map= NULL


  # create list of user data that is passed to the widget
  lst_x <- list(data  = 'undefined',
                dateString = dateString,
                layer = urls,
                layername=layernames,
                zoom = zoom,
                scale= scale,
                ulc= ulc,
                epsgcode=epsg3031Code,
                epsgproj=epsg3031String,
                tilesize=tileSize,
                attribution=attribution)

  # creating the widget
  visEarthPoleInternal(  x = lst_x)

}

### bViewInternal creates fpView widget =================================================

visEarthPoleInternal <- function(x = NULL) {

  sizing = htmlwidgets::sizingPolicy(
    browser.fill = TRUE,
    viewer.fill = TRUE,
    viewer.padding = 5
  )
  # create widget
  htmlwidgets::createWidget(
    name = 'visEarthPole',
    x,
    sizingPolicy = sizing,
    package = 'mapview'
  )
}

### Widget output function for use in Shiny =================================================
#
visEarthPoleOutput <- function(outputId, width = '100%', height = '800px') {
  htmlwidgets::shinyWidgetOutput(outputId, 'visEarthPole', width, height, package = 'mapview')
}

### Widget render function for use in Shiny =================================================
#
rendertestproj4<- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(expr, visEarthPoleOutput, env, quoted = TRUE)
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

getParams <- function(dateString=as.character(Sys.Date()), layerList){


  startTime<-vector()
  endTime<-vector()
  code<-vector()
  prod<-vector()
  prodName<-vector()
  url<-vector()
  ext<-vector()
  layercode<-vector()
  # define
  ext[1] <-".png"
  ext[2] <- ".png"
  ext[3] <- ".png"
  ext[4] <- ".png"
  ext[5] <- ".png"
  ext[6] <- ".png"
  ext[7] <- ".png"
  ext[8] <- ".png"
  ext[9] <- ".png"
  ext[10] <-".jpg"
  ext[11] <-".jpg"
  ext[12] <-".jpg"
  ext[13] <-".png"
  ext[14] <-".png"
  ext[15] <-".png"
  ext[16] <-".png"
  ext[17] <-".png"
  ext[18] <-".png"
  ext[19] <-".jpg"
  ext[20] <- ".jpg"
  ext[21] <- ".jpg"
  ext[22] <- ".png"
  ext[23] <- ".png"
  ext[24] <- ".png"
  ext[25] <- ".png"
  ext[26] <- ".jpg"
  ext[27] <- ".jpg"
  ext[28] <- ".png"
  ext[29] <- ".png"
  ext[30] <- ".jpg"
  ext[31] <- ".jpg"
  ext[32] <- ".jpg"


  startTime[1] <-  "2016-01-13"
  startTime[2] <-  "2016-01-13"
  startTime[3] <-  "2016-01-13"
  startTime[4] <-  "2016-01-13"
  startTime[5] <-  "2002-06-01"
  startTime[6] <-  "2002-06-01"
  startTime[7] <-  "2002-06-01"
  startTime[8] <-  "2002-06-01"
  startTime[9] <-  "2002-06-01"
  startTime[10] <-  "no time"
  startTime[11] <-  "no time"
  startTime[12] <-  "no time"
  startTime[13] <-  "no time"
  startTime[14] <-  "no time"
  startTime[15] <- "2013-06-07"
  startTime[16] <- "2013-06-07"
  startTime[17] <- "2013-06-06"
  startTime[18] <- "2013-06-06"
  startTime[19] <- "2013-06-06"
  startTime[20] <- "2013-06-06"
  startTime[21] <- "2013-06-06"
  startTime[22] <- "2013-06-07"
  startTime[23] <- "2013-06-07"
  startTime[24] <- "2013-06-06"
  startTime[25] <- "2013-06-06"
  startTime[26] <- "2013-06-06"
  startTime[27] <- "2013-06-06"
  startTime[28] <-  "no time"
  startTime[29] <-  "no time"
  startTime[30] <- "2015-11-24"
  startTime[31] <- "2015-11-24"
  startTime[32] <- "2015-11-24"
  endTime[1] <-  as.character(Sys.Date())
  endTime[2] <-  as.character(Sys.Date())
  endTime[3] <-  as.character(Sys.Date())
  endTime[4] <-  as.character(Sys.Date())
  endTime[5] <-  "2011-10-04"
  endTime[6] <-  "2011-10-04"
  endTime[7] <-  "2011-10-04"
  endTime[8] <-  "2011-10-04"
  endTime[9] <-  "2011-10-04"
  endTime[10] <-  "no time"
  endTime[11] <-  "no time"
  endTime[12] <-  "no time"
  endTime[13] <-  "no time"
  endTime[14] <-  "no time"
  endTime[15] <- as.character(Sys.Date())
  endTime[16] <- as.character(Sys.Date())
  endTime[17] <- as.character(Sys.Date())
  endTime[18] <- as.character(Sys.Date())
  endTime[19] <- as.character(Sys.Date())
  endTime[20] <- as.character(Sys.Date())
  endTime[21] <- as.character(Sys.Date())
  endTime[22] <- as.character(Sys.Date())
  endTime[23] <- as.character(Sys.Date())
  endTime[24] <- as.character(Sys.Date())
  endTime[25] <- as.character(Sys.Date())
  endTime[26] <- as.character(Sys.Date())
  endTime[27] <- as.character(Sys.Date())
  endTime[28] <-  "no time"
  endTime[29] <-  "no time"
  endTime[30] <- as.character(Sys.Date())
  endTime[31] <- as.character(Sys.Date())
  endTime[32] <- as.character(Sys.Date())

  prodName[1] <-  "AMSR2_Sea_Ice_Concentration_12km"
  prodName[2] <-  "AMSR2_Sea_Ice_Concentration_25km"
  prodName[3] <-  "AMSR2_Sea_Ice_Brightness_Temp_6km_89H"
  prodName[4] <-  "AMSR2_Sea_Ice_Brightness_Temp_6km_89V"
  prodName[5] <-  "AMSRE_Sea_Ice_Concentration_12km"
  prodName[6] <-  "AMSRE_Snow_Depth_Over_Ice"
  prodName[7] <-  "AMSRE_Sea_Ice_Concentration_25km"
  prodName[8] <-  "AMSRE_Sea_Ice_Brightness_Temp_89H"
  prodName[9] <-  "AMSRE_Sea_Ice_Brightness_Temp_89V"
  prodName[10] <-  "BlueMarble_NextGeneration"
  prodName[11] <-  "BlueMarble_ShadedRelief"
  prodName[12] <-  "BlueMarble_ShadedRelief_Bathymetry"
  prodName[13] <-  "Coastlines"
  prodName[14] <-  "Graticule"
  prodName[15] <-  "MODIS_Terra_Snow_Cover"
  prodName[16] <-  "MODIS_Terra_Sea_Ice"
  prodName[17] <-  "MODIS_Terra_Brightness_Temp_Band31_Day"
  prodName[18] <-  "MODIS_Terra_Brightness_Temp_Band31_Night"
  prodName[19] <-  "MODIS_Terra_CorrectedReflectance_TrueColor"
  prodName[20] <-  "MODIS_Terra_CorrectedReflectance_Bands367"
  prodName[21] <-  "MODIS_Terra_CorrectedReflectance_Bands721"
  prodName[22] <-  "MODIS_Aqua_Snow_Cover"
  prodName[23] <-  "MODIS_Aqua_Sea_Ice"
  prodName[24] <-  "MODIS_Aqua_Brightness_Temp_Band31_Day"
  prodName[25] <-  "MODIS_Aqua_Brightness_Temp_Band31_Night"
  prodName[26] <-  "MODIS_Aqua_CorrectedReflectance_TrueColor"
  prodName[27] <-  "MODIS_Aqua_CorrectedReflectance_Bands721"
  prodName[28] <-  "SCAR_Land_Mask"
  prodName[29] <-  "SCAR_Land_Water_Map"
  prodName[30] <-  "VIIRS_SNPP_CorrectedReflectance_TrueColor"
  prodName[31] <-  "VIIRS_SNPP_CorrectedReflectance_BandsM11-I2-I1"
  prodName[32] <-  "VIIRS_SNPP_CorrectedReflectance_BandsM3-I3-M11"
  requestedDate<-dateString
  for (i in unlist(layerList)){
    if (startTime[i] != "no time") {
      if (as.Date(startTime[i]) > as.Date(dateString)) {
        dateString<-as.Date(startTime[i])
      }
      if (as.Date(endTime[i]) < as.Date(startTime[i])) {
        dateString<-as.Date(endTime[i])
      }
    }
    if (i==10 || i == 11 || i==12 || i==22 || i==15) {
      res<-"EPSG3031_500m"
    }
    if (i==13 || i == 14 || i==28 || i==29 || i==30 || i==31 || i==32 || i==19 || i==20 || i==21 || i==26 || i==27){
      res<-"EPSG3031_250"
    }
    if (i==1 || i == 2 || i==3 || i==4 || i==5 || i==6 || i==7 || i==8 || i==9 || i==16 || i==17 || i==18 || i==23 || i==24 || i==25 ) {
      res<-"EPSG3031_1km"
    }
    if (startTime[i] == "no time") {
    url[i] <- paste0("https://map1{s}.vis.earthdata.nasa.gov/wmts-antarctic/",prodName[i],"/default/",res,"/{z}/{y}/{x}",ext[i])
    layercode[i]<-prodName[i]
    } else{
      url[i] <- paste0("https://map1{s}.vis.earthdata.nasa.gov/wmts-antarctic/",prodName[i],"/default/",dateString,"/",res,"/{z}/{y}/{x}",ext[i])
      layercode[i]<-prodName[i]
    }

    dateString<-requestedDate

  }


  return(list(url,layercode))
}