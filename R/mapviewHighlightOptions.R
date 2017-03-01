### define highlighting of features added via mapview

### line features =========================================================
highlightLineFeatures <- function(lwd = 2,
                                  stroke = TRUE,
                                  color = NULL,
                                  weight = 2,
                                  opacity = 1,
                                  fill = FALSE,
                                  fillColor = NULL,
                                  fillOpacity = 0,
                                  dashArray = NULL,
                                  bringToFront = TRUE,
                                  sendToBack = TRUE) {

  if (length(fillColor) != 1) fillColor <- color
  weight <- lwd + 2

  return(
    dropNULL(
      list(
        stroke = stroke,
        color = color,
        weight = weight,
        opacity = opacity,
        fill = fill,
        fillColor = fillColor,
        fillOpacity = fillOpacity,
        dashArray = dashArray,
        bringToFront = bringToFront,
        sendToBack = sendToBack
      )
    )
  )

}


### polygon features ======================================================
highlightPolygonFeatures <- function(alpha.regions = 0.6,
                                     lwd = 2,
                                     stroke = TRUE,
                                     color = NULL,
                                     weight = 2,
                                     opacity = 1,
                                     fill = NULL,
                                     fillColor = NULL,
                                     fillOpacity = 0.7,
                                     dashArray = NULL,
                                     bringToFront = TRUE,
                                     sendToBack = TRUE) {

  if (length(fillColor) != 1) fillColor <- color
  if (alpha.regions >= 0.8) {
    fillOpacity <- alpha.regions - 0.2
  } else {
    fillOpacity <- alpha.regions + 0.2
  }

  weight <- lwd + 1

  return(
    dropNULL(
      list(
        stroke = stroke,
        color = color,
        weight = weight,
        opacity = opacity,
        fill = fill,
        fillColor = fillColor,
        fillOpacity = fillOpacity,
        dashArray = dashArray,
        bringToFront = bringToFront,
        sendToBack = sendToBack
      )
    )
  )

}


### point features ========================================================
highlightPointFeatures <- function(stroke = NULL,
                                     color = NULL,
                                     weight = NULL,
                                     opacity = NULL,
                                     fill = NULL,
                                     fillColor = NULL,
                                     fillOpacity = NULL,
                                     dashArray = NULL,
                                     bringToFront = NULL,
                                     sendToBack = NULL) {

  if (length(fillColor) != 1) fillColor <- color

  return(
    list(
      stroke = stroke,
      color = color,
      weight = weight,
      opacity = opacity,
      fill = fill,
      fillColor = fillColor,
      fillOpacity = fillOpacity,
      dashArray = dashArray,
      bringToFront = bringToFront,
      sendToBack = sendToBack
    )
  )

}


### higlight options
mapviewHighlightOptions <- function(x, alpha.regions, lwd) {

  if (inherits(x, "Spatial")) {
    ls <- switch(class(x),
                 SpatialPointsDataFrame = highlightPointFeatures(),
                 SpatialPoints = highlightPointFeatures(),
                 SpatialLinesDataFrame = highlightLineFeatures(),
                 SpatialLines = highlightLineFeatures(),
                 SpatialPolygonsDataFrame = highlightPolygonFeatures(),
                 SpatialPolygons = highlightPolygonFeatures())
  } else {
    ls <- switch(getSFClass(sf::st_geometry(x)),
                 sfc_POINT           = highlightPointFeatures(),
                 sfc_MULTIPOINT      = highlightPointFeatures(),
                 sfc_LINESTRING      = highlightLineFeatures(lwd),
                 sfc_MULTILINESTRING = highlightLineFeatures(lwd),
                 sfc_POLYGON         = highlightPolygonFeatures(alpha.regions, lwd),
                 sfc_MULTIPOLYGON    = highlightPolygonFeatures(alpha.regions, lwd),
                 POINT               = highlightPointFeatures(),
                 MULTIPOINT          = highlightPointFeatures(),
                 LINESTRING          = highlightLineFeatures(lwd),
                 MULTILINESTRING     = highlightLineFeatures(lwd),
                 POLYGON             = highlightPolygonFeatures(alpha.regions, lwd),
                 MULTIPOLYGON        = highlightPolygonFeatures(alpha.regions, lwd))
  }

  return(ls)

}



dropNULL <- function (x) {
  if (length(x) == 0 || !is.list(x))
    return(x)
  x[!unlist(lapply(x, is.null))]
}


# highlight <- function(col, fact = 1.1) {
#   hsv3 <- rgb2hsv(col2rgb(color))
#   hsv3[3, ] <- hsv3[3, ] * fact
#   hsv(h = hsv3[1,], s = hsv3[2,], v = hsv3[3,])
# }

