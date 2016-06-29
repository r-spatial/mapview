#' Save mapview or leaflet map as HTML and/or image
#'
#' @description
#' Save a mapview or leaflet map as \code{.html} index file or \code{.png},
#' \code{.pdf}, or \code{.jpeg} image.
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
#' m <- mapview(breweries91)
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
  if (class(x) == "mapview") {
    x <- mapview2leaflet(x)
  }

  ## remove layers control
  x <- leaflet::removeLayersControl(x)

  ## if url is missing, create temporary .html file
  if (!avl_url)
    url <- gsub("\\.png|\\.pdf|\\.jpeg|\\.jpg", ".html", file)

  htmlwidgets::saveWidget(x, url)

  ## save to file
  if (avl_file) {
    webshot::webshot(url = url, file = file, ...)
  }

  ## if url was missing, remove temporary .html file
  if (!avl_url & remove_url)
    file.remove(url)

  return(invisible())
}
