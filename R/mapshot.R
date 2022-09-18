#' Save mapview or leaflet map as HTML and/or image using webshot
#'
#' @description
#' Save a mapview or leaflet map as \code{.html} index file or \code{.png},
#' \code{.pdf}, or \code{.jpeg} image.
#'
#' @details
#' mapshot uses \code{\link[webshot]{webshot}} from the {webshot} package.
#' mapshot2 uses \code{\link[webshot2]{webshot}} from the {webshot2} package.
#'
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
#' \code{\link[webshot]{webshot}}, \code{\link[htmlwidgets]{saveWidget}}.
#'
#' @examples
#' \dontrun{
#'   library(utils)
#'
#'   m = mapview(breweries)
#'   html_fl = tempfile(fileext = ".html")
#'   png_fl = tempfile(fileext = ".png")
#'
#'   ## create standalone .html
#'   mapshot(m, url = html_fl)
#'   browseURL(html_fl)
#'
#'   ## create standalone .png; temporary .html is removed automatically unless
#'   ## 'remove_url = FALSE' is specified
#'   mapshot(m, file = png_fl)
#'   browseURL(png_fl)
#'   mapshot(m, file = png_fl,
#'           remove_controls = c("homeButton", "layersControl"))
#'   browseURL(png_fl)
#'
#'   ## create .html and .png
#'   mapshot(m, url = html_fl, file = png_fl)
#'   browseURL(png_fl)
#'   browseURL(html_fl)
#' }
#'
#' @export mapshot
#' @describeIn mapshot Save mapview or leaflet map as HTML and/or image using webshot
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

  stopifnot(requireNamespace("webshot", quietly = TRUE))

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

#' Save mapview or leaflet map as HTML and/or image using webshot2
#'
#' @details
#' mapshot2 uses \code{\link[htmlwidgets]{saveWidget}} and \code{\link[webshot2]{webshot}}
#' to save maps as \code{.html} and/or \code{.png|.jpg} files, respectively.
#' \code{\link[webshot2]{webshot}} assumes a findable installation of some Chrome
#' browser variant on your system. If you see the the following error:
#'
#' \code{
#'   `google-chrome` and `chromium-browser` were not found.
#'   Try setting the CHROMOTE_CHROME environment variable or adding one of these
#'   executables to your PATH.
#' }
#'
#' it means that \code{\link[chromote]{find_chrome}} cannot find a Chrome based
#' browser in your system. Please see
#' \url{https://github.com/rstudio/chromote#specifying-which-browser-to-use}
#' for more details.
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
#'   and/or \code{\link[webshot2]{webshot}}.
#'
#' @seealso
#' \code{\link[webshot2]{webshot}}.
#'
#' @examples
#' \dontrun{
#'   library(utils)
#'
#'   m = mapview(breweries)
#'   html_fl = tempfile(fileext = ".html")
#'   png_fl = tempfile(fileext = ".png")
#'
#'   ## create standalone .html
#'   mapshot2(m, url = html_fl)
#'   browseURL(html_fl)
#'
#'   ## create standalone .png; temporary .html is removed automatically unless
#'   ## 'remove_url = FALSE' is specified
#'   mapshot2(m, file = png_fl)
#'   browseURL(png_fl)
#'   mapshot2(m, file = png_fl,
#'            remove_controls = c("homeButton", "layersControl"))
#'   browseURL(png_fl)
#'
#'   ## create .html and .png
#'   mapshot2(m, url = html_fl, file = png_fl)
#'   browseURL(png_fl)
#'   browseURL(html_fl)
#' }
#'
#' @export mapshot2
#' @describeIn mapshot Save mapview or leaflet map as HTML and/or image using webshot2
mapshot2 = function(x,
                   url = NULL,
                   file = NULL,
                   remove_controls = c("zoomControl",
                                       "layersControl",
                                       "homeButton",
                                       "scaleBar",
                                       "drawToolbar",
                                       "easyButton",
                                       "control"),
                   ...) {

  stopifnot(requireNamespace("webshot2", quietly = TRUE))

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

  # if not a leaflet widget (but some other widget) or remove_controls = FALSE
  # set to NULL
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
                      names(as.list(args(webshot2::webshot))),
                      several.ok = TRUE)


  ## saveWidget (either to provided url or tempfile)
  do.call(htmlwidgets::saveWidget, append(list(x), sw_ls[sw_args]))

  ## if file was rpovided
  if (avl_file) {

    ## if no junk to remove -> take webshot straight away & return
    if (is.null(remove_controls)) {
      suppressMessages(
        do.call(webshot2::webshot, args)
      )
      return(invisible())
    }

    ## if we land here, we want both url & file with some junk removed
    tmp_url = tempfile(fileext = ".html")
    tmp_fls = paste0(tools::file_path_sans_ext(tmp_url), "_files")

    sw_ls = utils::modifyList(sw_ls, list("file" = tmp_url))
    args$url = tmp_url

    x = removeMapJunk(x, remove_controls)

    do.call(htmlwidgets::saveWidget, append(list(x), sw_ls[sw_args]))
    suppressMessages(
      do.call(webshot2::webshot, args[ws_args])
    )

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
      "control" = removeControl(map),
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

removeControl = function(map) {
  idx = getCallEntryFromMap(map, "addControl")
  if (length(idx) > 0) map$x$calls[idx] = NULL
  return(map)
}



