### getPopupStyle creates popup style =================================================
getPopupStyle <- function() {
  # htmlTemplate <- paste(
  #   "<html>",
  #   "<head>",
  #   "<style>",
  #   "#popup",
  #   "{font-family: Arial, Helvetica, sans-serif;width: 20%;border-collapse: collapse;}",
  #   "#popup td {font-size: 1em;border: 0px solid #85ADFF;padding: 3px 20px 3px 3px;}",
  #   "#popup tr.alt td {color: #000000;background-color: #F0F5FF;}",
  #   "#popup tr.coord td {color: #000000;background-color: #A8E6A8;}",
  #   "div.scrollableContainer {max-height: 200px;max-width: 100%;overflow-y: auto;overflow-x: auto;margin: 0px;background: #D1E0FF;}",
  #   "</style>",
  #   "</head>",
  #   "<body>",
  #   "<div class='scrollableContainer'>",
  #   "<table class='popup scrollable'>",
  #   "<table id='popup'>")
  # return(htmlTemplate)
  fl <- system.file("templates/popup.brew", package = "mapview")
  pop <- readLines(fl)
  end <- grep("<%=pop%>", pop)
  return(paste(pop[1:(end-2)], collapse = ""))
}


### make path
makepathLarge <- function(group) {
  dirs <- list.dirs(tempdir())
  tmpPath <- grep(utils::glob2rx("*data_large*"), dirs, value = TRUE)
  if (length(tmpPath) == 0) {
    tmpPath <- tempfile(pattern = "data_large")
    dir.create(tmpPath)
  }
  baseFn <- paste("data_large", group, sep = "_")
  extFn <- "geojson"
  jsonFn <- paste0(baseFn, ".", extFn)
  pathJsonFn <- paste0(tmpPath, "/", jsonFn)
  return(list(tmpPath, pathJsonFn, jsonFn))
}


### calculate zoom
calcZoom <- function(data) {

  if (inherits(data, 'SpatialPolygons')) {
    noFeature <- length(data@polygons)
    noF <- noFeature / 1
  } else if (inherits(data, 'SpatialLines')) {
    noFeature <- length(data@lines)
    noF <- noFeature / 1
  } else {
    noFeature <- length(data@coords)
    noF <- noFeature / 5
  }

  zoom <- floor(-0.000000000429 * (noF^2) + 0.000148 * noF + 1)
  if (zoom > 14) {zoom <- 16}
  if (zoom < 9) {zoom <- 9}

  return(zoom)
}


## convert sp objects to dummy dataframes
toSPDF <- function(x) {
  cls <- class(x)[1]
  newcls <- paste0(cls, "DataFrame")
  if (cls %in% "SpatialPolygons") {
    x <- as(x, newcls)
  }

  if (cls %in% c("SpatialPoints", "SpatialLines")) {
    x <- as(x, newcls)
    x@data <- data.frame(dummy = rep(0, length(x)))
  }

  return(x)

}
