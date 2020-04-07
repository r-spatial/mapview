#' View two or more (possibly synchronised) mapview or leaflet maps
#'
#' @description
#' These functions are deprecated.
#' Please use leafsync::\code{\link[leafsync]{sync}} and
#' leafsync::\code{\link[leafsync]{latticeView}} instead.
#'
#' @param ... any number of mapview or leaflet objects or a list thereof
#' @param ncol how many columns should be plotted
#' @param sync whether to synchronise zoom and pan for certain elements.
#' Possible values are "all" (default) to sync all maps, "none" to disable
#' synchronisation or a list of panel numbers, e.g. \code{list(c(1, 3), c(2, 4))}
#' will synchronise panels 1 & 3 and panels 2 & 4. Panels are drawn from top right
#' to bottom left.
#' @param sync.cursor whether to show cursor position in synced panels (default TRUE).
#' @param no.initial.sync whether to sync the initial view (default TRUE).
#'
#' @export latticeView
#' @name latticeView
#' @aliases latticeView
#'

latticeView <- function(...,
                        ncol = 2,
                        sync = "none",
                        sync.cursor = FALSE,
                        no.initial.sync = TRUE) {

  .Defunct(new = "leafsync::latticeView", package = "mapview")

}

#' @describeIn latticeView alias for ease of typing
#' @aliases latticeview
#' @export latticeview
latticeview <- function(...) {
  .Defunct(new = "leafsync::latticeview", package = "mapview")
}

#' @describeIn latticeView convenience function for syncing maps
#' @aliases sync
#' @export sync
sync <- function(...,
                 ncol = 2,
                 sync = "all",
                 sync.cursor = TRUE,
                 no.initial.sync = TRUE) {

  .Defunct(new = "leafsync::sync", package = "leafsync")

}
