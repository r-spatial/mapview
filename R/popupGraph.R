#' Create HTML strings for popup graphs
#'
#' @description
#' Create HTML strings for \code{popup} graphs used as input for
#' \code{\link{mapview}}.
#'
#' @param graphs A \code{list} of figures associated with \code{x}.
#' @param ... Further arguments passed on to \code{\link{svg}}.
#'
#' @return
#' A \code{list} of HTML strings required to create popup graphs.
#'
#' @seealso \code{\link{popupTable}}.
#'
#' @examples
#' \dontrun{
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
#' mapview(meuse, popup = popupGraph(p, width = 3, height = 2.5))
#' }
#'
#' @export popupGraph
#' @name popupGraph
popupGraph <- function(graphs, ...) {

  ## if a single feature is provided, convert 'graphs' to list
  if (class(graphs) != "list")
    graphs <- list(graphs)

  ## create target folder and filename
  drs <- paste0(tempdir(), "/graphs")
  if (!dir.exists(drs)) dir.create(drs)

  ## loop over graphs
  pop <- lapply(1:length(graphs), function(i) {
    fls <- paste0(drs, "/tmp_", i, ".svg")

    svg(filename = fls, ...)
    print(graphs[[i]])
    dev.off()

    lns <- paste(readLines(fls), collapse = "")
    # file.remove(fls)
    return(lns)
  })

  ## remove target folder and return html strings
  # file.remove(drs)
  return(pop)
}
