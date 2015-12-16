#' A trellis theme to mimick the looks of mapview. This is a modified
#' version of latticeExtra::theEconomist.theme. (c) Felix Andrews
#'
#' @param win.fontfamily on Windows systems, sets the font by name.
#' @param with.bg if TRUE, uses a light blue background and a few other
#' corresponding changes; otherwise white.
#' @param box color for panel boxes, strip outlines, and axis ticks.
#' @param ... further arguments passed to simpleTheme and used to modify the theme.
#'
#' @seealso
#' \code{\link{theEconomist.theme}}
#'
#' @name mapviewTheme
#' @export mapviewTheme
#' @aliases mapviewTheme
#'
#' @examples
#' \dontrun{
#' library(rasterVis)
#'
#' levelplot(poppendorf[[10]], par.settings = mapviewTheme())
#' }

mapviewTheme <- function (win.fontfamily = NULL,
                           with.bg = FALSE,
                           box = "black",
                           ...) {

  theme <- list(background =
                  list(col = if (with.bg) "#D5E2E9" else "transparent"),
                plot.line = list(col = "#00526D", lwd = 2.5),
                superpose.line = list(col = c("#00526D", "#00A3DB",
                                              "#7A2713", "#939598",
                                              "#6CCFF6"), lwd = 2.5),
                plot.symbol = list(col = "#460000", pch = 21, cex = 2,
                                   fill = "#460000", alpha = 0.8),
                superpose.symbol = list(col = c("#00526D", "#00A3DB",
                                                "#7A2713", "#939598",
                                                "#6CCFF6"), pch = 16),
                plot.polygon = list(col = "#00526D"),
                superpose.polygon = list(col = c("#5F92A8", "#00526D",
                                                 "#6CCFF6", "#00A3DB",
                                                 "#A7A9AC")),
                regions = list(col = mapviewPalette(256)),
                reference.line = list(col = if (with.bg) "white" else "#aaaaaa",
                                      lwd = 1.75),
                dot.line = list(col = if (with.bg) "white" else "#aaaaaa",
                                lwd = 1.75),
                add.line = list(col = "#ED1C24", lwd = 1.5),
                axis.line = list(col = box),
                box.3d = list(col = box),
                strip.border = list(col = box),
                strip.background = list(col = if (with.bg) "white" else "#CBDDE6"),
                strip.shingle = list(col = if (with.bg) "#CBDDE6" else "#00A3DB",
                                     alpha = 0.5),
                par.main.text = list(font = 1, just = "left",
                                     x = grid::unit(5, "mm")),
                par.sub.text = list(font = 1, just = "left",
                                    x = grid::unit(5, "mm")),
                axis.text = list(cex = 0.8),
                box.dot = list(col = "#00526D", pch = "|", lwd = 1.75),
                box.rectangle = list(fill = "#00526D", alpha = 0.5, col = "#00526D",
                                     lwd = 1.75),
                box.umbrella = list(col = "#00526D", lty = 1, lwd = 1.75))

  if (.Platform$OS.type == "windows" && !is.null(win.fontfamily)) {
    grDevices::windowsFonts(TheEconomistLike = win.fontfamily)
    theme$grid.pars$fontfamily <- "TheEconomistLike"
  }
  else {
  }
  utils::modifyList(utils::modifyList(
    lattice::standard.theme("pdf"), theme), simpleTheme(...))
}
