#' Save mapview or leaflet map as HTML and/or image
#'
#' @description
#' Save a mapview or leaflet map as \code{.html} index file or \code{.png},
#' \code{.pdf}, or \code{.jpeg} image.
#'
#' @details
#' mapshot can be used to save both leaflet and mapview maps as html or png
#' files or both. In theory, it should also work for any and all other htmlwidgets
#' but has not been tested extensively for other htmlwidgets.
#'
#' In case you want to save larger maps mapshot is likely to fail. You can try
#' setting \code{selfcontained = FALSE} to avoid errors and create a valid
#' local html file.
#'
#' @param x \code{mapview} or \code{leaflet} object (or any other hmtlwidget).
#' @param url Output \code{.html} file. If not supplied and 'file' is specified,
#'   a temporary index file will be created.
#' @param file Output \code{.png}, \code{.pdf}, or \code{.jpeg} file.
#' @param remove_controls \code{character} vector of control buttons to be removed
#'   from the map when saving to file. Any combination of
#'   "zoomControl", "layersControl", "homeButton", "scaleBar", "drawToolbar",
#'   "easyButton". If set to \code{NULL} nothing will be removed. Ignord if \code{x}
#'   is not a mapview or leaflet map.
#' @param ... Further arguments passed on to \code{\link[htmlwidgets]{saveWidget}}
#'   and/or \code{\link[webshot]{webshot}}.
#'
#' @seealso
#' \code{\link{webshot}}, \code{\link{saveWidget}}.
#'
#' @examples
#' \dontrun{
#'   m = mapview(breweries)
#'
#'   ## create standalone .html
#'   mapshot(m, url = paste0(getwd(), "/map.html"))
#'
#'   ## create standalone .png; temporary .html is removed automatically unless
#'   ## 'remove_url = FALSE' is specified
#'   mapshot(m, file = paste0(getwd(), "/map.png"))
#'   mapshot(m, file = paste0(getwd(), "/map.png"),
#'           remove_controls = c("homeButton", "layersControl"))
#'
#'   ## create .html and .png
#'   mapshot(m, url = paste0(getwd(), "/map.html"),
#'           file = paste0(getwd(), "/map.png"))
#' }
#'
#' @export mapshot
#' @name mapshot
mapshot = function(x,
                   url = NULL,
                   file = NULL,
                   remove_controls = c("zoomControl",
                                       "layersControl",
                                       "homeButton",
                                       "scaleBar",
                                       "drawToolbar",
                                       "easyButton"),
                   ...) {

  ## if both 'url' and 'file' are missing, throw an error
  avl_url = !is.null(url)
  avl_file = !is.null(file)

  if (!avl_url & !avl_file)
    stop("Please provide a valid 'url' or 'file' argument (or both).")

  ## normalize path to ensure webshot is working
  if (avl_url) url = normalizePath(url, mustWork = FALSE)
  if (avl_file) file = normalizePath(file, mustWork = FALSE)

  ## if a 'mapview' object is supplied, extract x
  if (inherits(x, "mapview")) {
    x = mapview2leaflet(x)
  }

  if (!inherits(x, "leaflet") | is_literally_false(remove_controls)) {
    remove_controls = NULL
  }

  ## if no url provided -> set url to tempfile & remove junk
  if (!avl_url) {
    url = tempfile(fileext = ".html")
    x = removeMapJunk(x, remove_controls)
  }

  ## prepare arguments for saveWidget & webshot
  args = list(url = url, file = file, ...)
  sw_ls = args
  sw_ls[names(sw_ls) == "file"] = NULL
  names(sw_ls)[which(names(sw_ls) == "url")] = "file"

  ## the arguments to be passed to saveWidget
  sw_args = match.arg(names(sw_ls),
                      names(as.list(args(htmlwidgets::saveWidget))),
                      several.ok = TRUE)

  ## the arguments to be passed to webshot
  ws_args = match.arg(names(args),
                      names(as.list(args(webshot::webshot))),
                      several.ok = TRUE)


  ## saveWidget (either to provided url or tempfile)
  do.call(htmlwidgets::saveWidget, append(list(x), sw_ls[sw_args]))

  ## if file was rpovided
  if (avl_file) {

    ## if no junk to remove -> take webshot straight away & return
    if (is.null(remove_controls)) {
      do.call(webshot::webshot, args)
      return(invisible())
    }

    ## if we land here, we want both url & file with some junk removed
    tmp_url = tempfile(fileext = ".html")
    tmp_fls = paste0(tools::file_path_sans_ext(tmp_url), "_files")

    sw_ls = utils::modifyList(sw_ls, list("file" = tmp_url))
    args$url = tmp_url

    x = removeMapJunk(x, remove_controls)

    do.call(htmlwidgets::saveWidget, append(list(x), sw_ls[sw_args]))
    do.call(webshot::webshot, args[ws_args])

    return(invisible())

  }

}

#' Delete elements from a map.
#'
#' @details
#' Currently supports removal of
#'
#' \itemize{
#'   \item "zoomControl"
#'   \item "layersControl"
#'   \item "homeButton"
#'   \item "scaleBar"
#'   \item "drawToolbar"
#'   \item "easyButton"
#' }
#'
#' This is mainly useful when taking a static screenshot of a map.
#'
#' @param map the map from which to remove elements.
#' @param junk a charcter vector of elements to remove. If NULL (the default),
#'   nothing is removed and the map is returned as is. See Details for a list
#'   of currently supported elements.
#'
#' @examples
#' if (interactive()) {
#'   library(mapview)
#'
#'   map = mapview(franconia)
#'
#'   removeMapJunk(map, "zoomControl")
#' }
#'
#' @export
removeMapJunk = function(map, junk = NULL) {
  if (inherits(map, "mapview")) map = mapview2leaflet(map)

  if (is.null(junk)) return(map)

  for (jnk in junk) {
    map = switch(
      jnk,
      "zoomControl" = removeZoomControl(map),
      "layersControl" = leaflet::removeLayersControl(map),
      "homeButton" = removeHomeButtons(map),
      "scaleBar" = removeScalebar(map),
      "drawToolbar" = removeDrawToolbar(map),
      "easyButton" = removeEasyButton(map),
      NULL = map
    )
  }
  return(map)
}


removeZoomControl = function(map) {
  map$x$options = append(map$x$options, list("zoomControl" = FALSE))
  return(map)
}

removeHomeButtons = function(map) {
  idx = getCallEntryFromMap(map, "addHomeButton")
  if (length(idx) > 0) map$x$calls[idx] = NULL
  return(map)
}

removeScalebar = function(map) {
  idx = getCallEntryFromMap(map, "addScaleBar")
  if (length(idx) > 0) map$x$calls[idx] = NULL
  return(map)
}

removeDrawToolbar = function(map) {
  idx = getCallEntryFromMap(map, "addDrawToolbar")
  if (length(idx) > 0) map$x$calls[idx] = NULL
  return(map)
}

removeEasyButton = function(map) {
  idx = getCallEntryFromMap(map, "addEasyButton")
  if (length(idx) > 0) map$x$calls[idx] = NULL
  return(map)
}





