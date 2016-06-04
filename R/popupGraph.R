#' Create HTML strings for popup graphs
#'
#' @description
#' Create HTML strings for \code{popup} graphs used as input for
#' \code{\link{mapview}} or \code{\link{leaflet}}.
#'
#' @details
#' Type \code{svg} uses native \code{svg} encoding via \code{\link{readLines}}.
#' \code{height} and \code{width} are set via \code{...} and passed on to
#' \code{\link{svg}} \cr
#' Type \code{png} embeds via \code{"<img src = ..."}.
#' \code{height} and \code{width} are set via \code{...} and passed on to
#' \code{\link{png}} \cr
#' Type \code{html} embeds via \code{"<iframe src = ..."}.
#' \code{height} and \code{width} are set directly in pixels. \cr
#'
#' @param graphs A \code{list} of figures associated with \code{x}.
#' @param type Output filetype, one of "png" (default), "svg" or "html".
#' @param ... Further arguments passed on to \code{\link{png}} or
#' \code{\link{svg}}.
#'
#' @return
#' A \code{list} of HTML strings required to create popup graphs.
#'
#' @seealso \code{\link{popupTable}} and \code{\link{popupImage}}.
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
#'
#' ### example: html -----
#' library(scatterD3)
#' p <- lapply(1:length(meuse), function(i) {
#'   clr <-rep(0, length(meuse))
#'   clr[[i]] <- 1
#'   scatterD3(x = meuse$cadmium,
#'             y = meuse$copper,
#'             col_var = clr,
#'             legend_width = 0)
#' })
#'
#' mapview(meuse, popup = popupGraph(p, type = "html", width = 400, height = 300))
#'
#' mapview(pt, popup = popupGraph(mapview(pt)@map, type = "html"))
#' }
#'
#' @export popupGraph
#' @name popupGraph
popupGraph <- function(graphs, type = c("png", "svg", "html"), ...) {

  ## if a single feature is provided, convert 'graphs' to list
  if (class(graphs)[1] != "list")
    graphs <- list(graphs)

  ## create target folder and filename
  drs <- file.path(tempdir(), "graphs")
  if (!dir.exists(drs)) dir.create(drs)

  type <- type[1]

  pop <- switch(type,
                png = popupPNGraph(graphs = graphs, dsn = drs, ...),
                svg = popupSVGraph(graphs = graphs, dsn = drs, ...),
                html = popupHTMLGraph(graphs = graphs, dsn = drs, ...))

  # pop <- if (type[1] == "svg") {
  #   popupSVGraph(graphs = graphs, dsn = drs, ...)
  # } else {
  #   popupPNGraph(graphs = graphs, dsn = drs, ...)
  # }

  ## remove target folder and return html strings
  # file.remove(drs)
  return(pop)
}


### svg -----
popupSVGraph <- function(graphs, dsn = tempdir(), ...) {
  lapply(1:length(graphs), function(i) {
    nm <- paste0("tmp_", i, ".svg")
    fls <- file.path(dsn, nm)

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
    nm <- paste0("tmp_", i, ".png")
    fls <- file.path(dsn, nm)

    png(filename = fls, ...)
    print(graphs[[i]])
    dev.off()

    rel_path <- file.path("..", basename(dsn))

    paste0("<img src = ", file.path(rel_path, basename(fls)), ">")
  })
}

### html -----
popupHTMLGraph <- function(graphs, dsn = tempdir(),
                           width = 300, height = 300, ...) {
  lapply(1:length(graphs), function(i) {
    nm <- paste0("tmp_", i, ".html")
    fls <- file.path(dsn, nm)
    htmlwidgets::saveWidget(graphs[[i]], fls, ...)

    rel_path <- file.path("..", basename(dsn))

    popupIframe(file.path(rel_path, basename(fls)), width, height)

    # paste0("<iframe src='",
    #        file.path(rel_path, basename(fls)),
    #        "' frameborder='0' width=",
    #        width,
    #        " height=",
    #        height, "></iframe>")
  })
}


### iframe -----
popupIframe <- function(src, width = 300, height = 300) {
  paste0("<iframe src='",
         src,
         "' frameborder=0 width=",
         width,
         " height=",
         height,
         #" align=middle",
         "></iframe>")
}

