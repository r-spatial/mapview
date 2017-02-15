### define highlighting of features added via mapview

### line features =========================================================
highlightLineFeatures <- function(stroke = TRUE,
                                  color = "#00ffff",
                                  weight = 4,
                                  opacity = 1,
                                  fill = FALSE,
                                  fillColor = NULL,
                                  fillOpacity = 0,
                                  dashArray = NULL,
                                  bringToFront = TRUE,
                                  sendToBack = TRUE) {

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


### polygon features ======================================================
highlightPolygonFeatures <- function(stroke = TRUE,
                                     color = "#00ffff",
                                     weight = 4,
                                     opacity = 1,
                                     fill = TRUE,
                                     fillColor = "#00ffff",
                                     fillOpacity = 0.2,
                                     dashArray = NULL,
                                     bringToFront = TRUE,
                                     sendToBack = TRUE) {

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
mapviewHighlightOptions <- function(x,
                                    ...) {

  if (inherits(x, "Spatial")) {
    ls <- switch(class(x),
                 SpatialPointsDataFrame = highlightPointFeatures(...),
                 SpatialPoints = highlightPointFeatures(...),
                 SpatialLinesDataFrame = highlightLineFeatures(...),
                 SpatialLines = highlightLineFeatures(...),
                 SpatialPolygonsDataFrame = highlightPolygonFeatures(...),
                 SpatialPolygons = highlightPolygonFeatures(...))
  } else {
    ls <- switch(getSFClass(sf::st_geometry(x)),
                 sfc_POINT           = highlightPointFeatures(...),
                 sfc_MULTIPOINT      = highlightPointFeatures(...),
                 sfc_LINESTRING      = highlightLineFeatures(...),
                 sfc_MULTILINESTRING = highlightLineFeatures(...),
                 sfc_POLYGON         = highlightPolygonFeatures(...),
                 sfc_MULTIPOLYGON    = highlightPolygonFeatures(...),
                 POINT               = highlightPointFeatures(...),
                 MULTIPOINT          = highlightPointFeatures(...),
                 LINESTRING          = highlightLineFeatures(...),
                 MULTILINESTRING     = highlightLineFeatures(...),
                 POLYGON             = highlightPolygonFeatures(...),
                 MULTIPOLYGON        = highlightPolygonFeatures(...))
  }

  return(ls)

}
