#' Create HTML strings for popup graphs
#'
#' @description
#' Create HTML strings for \code{popup} graphs used as input for
#' \code{\link{mapview}}.
#'
#' @param graphs A \code{list} of figures associated with \code{x}.
#' @param type Output filetype, one of "png" (default) and "svg".
#' @param ... Further arguments passed on to \code{\link{png}} or
#' \code{\link{svg}}.
#'
#' @return
#' A \code{list} of HTML strings required to create popup graphs.
#'
#' @seealso \code{\link{popupTable}}.
#'
#' @examples
#' \dontrun{
#' ### example: svg -----
#'
#' library(sp)
#'
#' data(meuse)
#' coordinates(meuse) <- ~ x + y
#' proj4string(meuse) <- CRS("+init=epsg:28992")
#'
#' ## create plots with points colored according to feature id
#' library(lattice)
#' p <- xyplot(copper ~ cadmium, data = meuse@data, col = "grey")
#' p <- mget(rep("p", length(meuse)))
#'
#' clr <- rep("grey", length(meuse))
#' p <- lapply(1:length(p), function(i) {
#'   clr[i] <- "red"
#'   update(p[[i]], col = clr)
#' })
#'
#' mapview(meuse, popup = popupGraph(p, type = "svg", width = 3, height = 2.5))
#'
#' ### example: png -----
#' pt <- data.frame(x = 174.764474, y = -36.877245)
#'
#' coordinates(pt) <- ~ x + y
#' proj4string(pt) <- "+init=epsg:4326"
#'
#' p2 <- levelplot(t(volcano), col.regions = terrain.colors(100))
#'
#' mapview(pt, popup = popupGraph(p2, width = 300, height = 400))
#' }
#'
#' @export popupGraph
#' @name popupGraph
popupGraph <- function(graphs, type = c("png", "svg"), ...) {

  ## if a single feature is provided, convert 'graphs' to list
  if (class(graphs) != "list")
    graphs <- list(graphs)

  ## create target folder and filename
  drs <- paste0(tempdir(), "/graphs")
  if (!dir.exists(drs)) dir.create(drs)

  pop <- if (type[1] == "svg") {
    popupSVGraph(graphs = graphs, dsn = drs, ...)
  } else {
    popupPNGraph(graphs = graphs, dsn = drs, ...)
  }

  ## remove target folder and return html strings
  # file.remove(drs)
  return(pop)
}


### svg -----
popupSVGraph <- function(graphs, dsn = tempdir(), ...) {
  lapply(1:length(graphs), function(i) {
    fls <- paste0(dsn, "/tmp_", i, ".svg")

    svg(filename = fls, ...)
    print(graphs[[i]])
    dev.off()

    lns <- paste(readLines(fls), collapse = "")
    # file.remove(fls)
    return(lns)
  })
}


### png -----
popupPNGraph <- function(graphs, dsn = tempdir(), ...) {
  lapply(1:length(graphs), function(i) {
    fls <- paste0(dsn, "/tmp_", i, ".png")

    png(filename = fls, ...)
    print(graphs[[i]])
    dev.off()

    paste0("<img src = ", paste0("../graphs/", basename(fls)), ">")
  })
}
