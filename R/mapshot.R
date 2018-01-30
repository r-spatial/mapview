#' Save mapview or leaflet map as HTML and/or image
#'
#' @description
#' Save a mapview or leaflet map as \code{.html} index file or \code{.png},
#' \code{.pdf}, or \code{.jpeg} image.
#'
#' @details
#' mapshot can be used to save both leaflet and mapview maps as html or png
#' files or both.
#'
#' NOTE 1: In case you want to save larger maps produced with mapview
#' (i.e. if you see the following warning: "the supplied feature layer has more
#' points/vertices than the set threshold. using special rendering function,
#' hence things may not behave as expected from a standard leaflet map") mapshot
#' is likely to fail. Try setting \code{selfcontained = FALSE} to avoid errors
#' and create a valid local html file.
#'
#' NOTE 2: In case you want to save a map with popupGraphs or popupImages
#' the respective graph/image files will be located one level above the
#' specified target location. In case you want to move the html file, make
#' sure to also move the respective *-graphs folder one level above.
#'
#' @param x \code{mapview} or \code{leaflet} object.
#' @param url Output \code{.html} file. If not supplied and 'file' is specified,
#' a temporary index file will be created.
#' @param file Output \code{.png}, \code{.pdf}, or \code{.jpeg} file.
#' @param remove_url \code{logical}. If \code{TRUE} (default), the \code{.html}
#' file is removed once processing is completed. Only applies if 'url' is not
#' specified.
#' @param ... Further arguments passed on to \code{\link{webshot}}.
#'
#' @seealso
#' \code{\link{webshot}}, \code{\link{saveWidget}}.
#'
#' @examples
#' \dontrun{
#' m <- mapview(breweries)
#'
#' ## create standalone .html
#' mapshot(m, url = paste0(getwd(), "/map.html"))
#'
#' ## create standalone .png; temporary .html is removed automatically unless
#' ## 'remove_url = FALSE' is specified
#' mapshot(m, file = paste0(getwd(), "/map.png"))
#'
#' ## create .html and .png
#' mapshot(m, url = paste0(getwd(), "/map.html"),
#'         file = paste0(getwd(), "/map.png"))
#' }
#'
#' @export mapshot
#' @name mapshot
mapshot <- function(x, url = NULL, file = NULL, remove_url = TRUE, ...) {

  ## if both 'url' and 'file' are missing, throw an error
  avl_url <- !is.null(url)
  avl_file <- !is.null(file)

  if (!avl_url & !avl_file)
    stop("Please provide a valid 'url' or 'file' argument (or both).")

  ## if a 'mapview' object is supplied, extract map
  if (inherits(x, "mapview")) {
    x <- mapview2leaflet(x)
  }

  ## remove layers control
  # x <- leaflet::removeLayersControl(x)

  ## if url is missing, create temporary .html file
  if (!avl_url)
    url <- gsub(tools::file_ext(file), "html", file)
    # url <- gsub("\\.png|\\.pdf|\\.jpeg|\\.jpg", ".html", file)

  if (dir.exists(file.path(tempdir(), "popup_graphs"))) {
    file.copy(from = file.path(tempdir(), "popup_graphs"),
              to = paste0(dirname(url), "/"), recursive = TRUE)
    file.copy(from = file.path(dirname(url),  "popup_graphs"),
              to = paste0(dirname(dirname(url)), "/"), recursive = TRUE)
    unlink(file.path(dirname(url),  "popup_graphs"), recursive = TRUE)
  }

  args <- list(url = url, file = file, ...)
  sw_ls <- args
  sw_ls[names(sw_ls) == "file"] <- NULL
  names(sw_ls)[which(names(sw_ls) == "url")] <- "file"
  sw_args <- match.arg(names(sw_ls),
                       names(as.list(args(htmlwidgets::saveWidget))),
                       several.ok = TRUE)

  do.call(htmlwidgets::saveWidget, append(list(x), sw_ls[sw_args]))

  ## save to file
  if (avl_file) {
    ws_args <- match.arg(names(args),
                         names(as.list(args(webshot::webshot))),
                         several.ok = TRUE)
    do.call(webshot::webshot, args[ws_args])
  }

  ## if url was missing, remove temporary .html file
  if (!avl_url & remove_url) {
    url_files = paste0(tools::file_path_sans_ext(url), "_files")
    unlink(c(url, url_files), recursive = TRUE)
  }

  return(invisible())
}
