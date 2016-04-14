#' Save mapview or leaflet map as image
#'
#' @description
#' Save a mapview or leaflet map as \code{.png}, \code{.pdf}, or \code{.jpeg}
#' image.
#'
#' @param x \code{mapview} or \code{leaflet} object.
#' @param url Filepath of the \code{.html} index file related with \code{x}. If
#' not supplied, a temporarily created index file will be used.
#' @param file Output filename. Currently supported data types are \code{.png},
#' \code{.pdf}, and \code{.jpeg}.
#' @param ... Further arguments passed on to \code{\link{webshot}}.
#'
#' @seealso
#' \code{\link{webshot}}, \code{\link{saveWidget}}.
#'
#' @examples
#' \dontrun{
#' m <- mapview(breweries91)
#' mapshot(m, file = "~/map.png")
#' }
#'
#' @export mapshot
#' @name mapshot
mapshot <- function(x, url = NULL, file, ...) {

  ## if a 'mapview' object is supplied, extract map
  if (class(x) == "mapview") {
    x <- mapview2leaflet(x)
  }

  ## if url is missing, create temporary .html file
  avl <- !is.null(url)

  if (!avl) {
    url <- gsub("\\.png|\\.pdf|\\.jpeg|\\.jpg", ".html", file)
    htmlwidgets::saveWidget(x, url)
  }

  ## save to file
  webshot::webshot(url = url, file = file, ...)

  ## if url was missing, remove temporary .html file
  if (!avl)
    file.remove(url)

  return(invisible())
}
