#' Garnish/decorate leaflet or mapview maps.
#'
#' @description
#' This function provides a versatile interface to add components to a
#' leaflet or mapview map. It takes functions such as "addMouseCoordinates"
#' or \code{\link{addLayersControl}} and their respective arguments and adds
#' them to the map. Arguments must be named. Functions can be plain or
#' character strings.
#'
#' @param map a mapview or leaflet object.
#' @param ... functions and their arguments to add things to a map.
#'
#' @examples
#' library(leaflet)
#' library(leafem)
#' library(leafpop)
#'
#' m <- leaflet() %>% addProviderTiles("OpenStreetMap")
#' garnishMap(m, leafem::addMouseCoordinates, style = "basic")
#'
#' ## add more than one with named argument
#' library(raster)
#'
#' m1 <- garnishMap(m, leafem::addMouseCoordinates, leafem::addHomeButton,
#'                  ext = extent(breweries))
#' m1
#'
#' ## even more flexible
#' m2 <- garnishMap(m1, addPolygons, data = franconia,
#'                  popup = leafpop::popupTable(franconia),
#'                  fillOpacity = 0.8, color = "black", fillColor = "#BEBEBE")
#' garnishMap(m2, addCircleMarkers, data = breweries)
#'
#' @export garnishMap
#' @name garnishMap
#' @rdname garnishMap
#' @aliases garnishMap
garnishMap <- function(map, ...) {

  .Deprecated(new = "leafem::garnishMap", package = "mapview",
              old = "mapview::garnishMap")

  leafem::garnishMap(map, ...)

  # if (inherits(map, "mapview")) map <- mapview2leaflet(map)
  # stopifnot(inherits(map, "leaflet"))
  #
  # ls <- list(...)
  #
  # funs <- sapply(ls, is.function)
  #
  # if (all(sapply(ls, is.null)) && all(sapply(funs, is.null))) {
  #   return(map)
  # } else {
  #   fn_lst <- lapply(ls[funs], function(i) {
  #     tst <- try(match.fun(i), silent = TRUE)
  #     if (class(tst) == "try-error") tst <- NULL
  #     return(tst)
  #   })
  #   fn_lst <- fn_lst[!sapply(fn_lst, is.null)]
  #
  #   args <- !funs
  #
  #   arg_lst <- ls[args]
  #   nms <- names(arg_lst)[names(arg_lst) != ""]
  #
  #   arg_nms <- lapply(fn_lst, function(i) {
  #     ma <- match.arg(c("map", nms), names(as.list(args(i))),
  #                     several.ok = TRUE)
  #     ma[!ma %in% "map"]
  #   })
  #
  #   for (i in seq(fn_lst)) {
  #     vec <- arg_nms[[i]]
  #     map <- do.call(fn_lst[[i]], append(list(map), arg_lst[vec]))
  #   }
  #   return(map)
  # }
}

### decorateMap lets you pass lists of functions with respective lists of
### named lists of arguments as in
### decorateMap(map, list(addCircleMarkers), list(list(data = breweries91)))
decorateMap <- function(map, funs, args) {
  for (i in seq(funs)) {
    map <- do.call(leafem::garnishMap, c(list(map), funs[[i]], args[[i]]))
  }
  return(map)
}
