#' Create HTML strings for popups
#'
#' @description
#' These functions are deprecated.
#' Please use leafpop::\code{\link[leafpop]{popupTable}},
#' leafpop::\code{\link[leafpop]{popupImage}}
#' and leafpop::\code{\link[leafpop]{popupGraph}} instead.
#'
#' @param x A \code{Spatial*} object.
#' @param zcol \code{numeric} or \code{character} vector indicating the columns
#' included in the output popup table. If missing, all columns are displayed.
#' @param row.numbers \code{logical} whether to include row numbers in the popup table.
#' @param feature.id \code{logical} whether to add 'Feature ID' entry to popup table.
#'
#' @export popupTable
#' @name popupTable
#' @rdname popup
popupTable = function(x, zcol, row.numbers = TRUE, feature.id = TRUE) {
  .Defunct(new = "leafpop::popupTable", package = "mapview")
}


#' Create HTML strings for popups
#'
#' @param img A character \code{vector} of file path(s) or
#' web-URL(s) to any sort of image file(s).
#' @param src Whether the source is "local" (i.e. valid file path(s)) or
#' "remote" (i.e. valid URL(s)).
#' @param embed whether to embed the (local) images in the popup html as
#' base64 ecoded. Set this to TRUE if you want to save and share your map, unless
#' you want render many images, then set to FALSE and make sure to copy ../graphs
#' when copying the map to a different location.
#' @param ... further arguments passed on to underlying methods such as
#' height and width.
#'
#' @export popupImage
#' @name popupImage
#' @rdname popup
popupImage = function(img, src = c("local", "remote"), embed = FALSE, ...) {
  .Defunct(new = "leafpop::popupImage", package = "mapview")
}



#' Create HTML strings for popups
#'
#' @param graphs A \code{list} of figures associated with \code{x}.
#' @param type Output filetype, one of "png" (default), "svg" or "html".
#' @param width popup width in pixels.
#' @param height popup height in pixels.
#'
#' @export popupGraph
#' @name popupGraph
#' @rdname popup
popupGraph = function(graphs, type = c("png", "svg", "html"),
                       width = 300, height = 300, ...) {
  .Defunct(new = "leafpop::popupGraph", package = "mapview")
}


popupLayoutDependencies <- function() {
  list(
    htmltools::htmlDependency(
      "PopupTable",
      '0.0.1',
      system.file("htmlwidgets/lib/popup", package = "mapview"),
      stylesheet = 'popup.css'
    ))
}
