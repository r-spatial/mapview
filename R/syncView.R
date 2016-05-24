#' View two or more synchronised mapview or leaflet maps
#'
#' @description
#' This function produces a synced view of two or more maps
#'
#' @param ... any number of mapview or leaflet objects
#'
#' @examples
#' \dontrun{
#' library(sp)
#'
#' data(meuse)
#' coordinates(meuse) <- ~x+y
#' proj4string(meuse) <- CRS("+init=epsg:28992")
#'
#' m1 <- mapview(meuse, zcol = "soil", burst = TRUE)
#' m2 <- mapview(meuse, zcol = "lead")
#'
#' syncView(m1, m2)
#'
#' ## 4 panels
#' m1 <- mapview(poppendorf[[1]], legend = FALSE)
#' m2 <- mapview(poppendorf[[2]], legend = FALSE)
#' m3 <- mapview(poppendorf[[3]], legend = FALSE)
#' m4 <- mapview(poppendorf[[4]], legend = FALSE)
#'
#' syncView(m1, m2, m3, m4)
#' }
#'
#' @export syncView
#' @name syncView
#' @aliases syncView
#'

syncView <- function(...) {

  ls <- list(...)

  for (i in seq(ls)) {
    if (inherits(ls[[i]], "mapview")) ls[[i]] <- mapview2leaflet(ls[[i]])
  }

  # if (inherits(map1, "mapview")) map1 <- mapview2leaflet(map1)
  # if (inherits(map2, "mapview")) map2 <- mapview2leaflet(map2)
  # if (inherits(map3, "mapview")) map3 <- mapview2leaflet(map3)
  # if (inherits(map4, "mapview")) map4 <- mapview2leaflet(map4)

  tg <- lapply(seq(ls), function(i) {
    htmltools::tags$div(style="display:inline;width:49%;float:left;border-style:solid;border-color:#BEBEBE;border-width:1px 1px 1px 1px;",ls[[i]])
  })

  tl <- htmltools::tagList(
    htmltools::tags$head(htmltools::tags$script(
      type="text/javascript",
      src="https://cdn.rawgit.com/turban/Leaflet.Sync/master/L.Map.Sync.js"
    )),
    tg,
    #for (i in seq(ls)) {
    #  htmltools::tags$div(style="display:inline;width:49%;float:left;border-style:solid;border-color:#BEBEBE;border-width:1px 1px 1px 1px;",ls[[1]]),
    #  },
    # htmltools::tags$div(style="display:inline;width:49%;float:left;border-style:solid;border-color:#BEBEBE;border-width:1px 1px 1px 1px;",ls[[2]]),
    # htmltools::tags$div(style="display:inline;width:49%;float:left;border-style:solid;border-color:#BEBEBE;border-width:1px 1px 1px 1px;",ls[[3]]),
    # htmltools::tags$div(style="display:inline;width:49%;float:left;border-style:solid;border-color:#BEBEBE;border-width:1px 1px 1px 1px;",ls[[4]]),
    htmlwidgets::onStaticRenderComplete(
      paste0('var leaf_widgets = Array.prototype.map.call(
        document.querySelectorAll(".leaflet"),
          function(ldiv){
            return HTMLWidgets.find("#" + ldiv.id);
          }
        );',
      Reduce(paste0, sapply(seq(ls), function(i) {
        first <-  do.call(c, lapply(seq(ls), function(j) {
          paste0("leaf_widgets[", j - 1, "]")
        }))
        paste0(first, ".sync(leaf_widgets[", i - 1, "]);")
      }))
      )
    )
  )
#
#       // make this easy since we know only two maps
#       leaf_widgets[0].sync(leaf_widgets[1]);
#       leaf_widgets[1].sync(leaf_widgets[0]);
#       // leaf_widgets[0].sync(leaf_widgets[2]);
#       // leaf_widgets[2].sync(leaf_widgets[0]);
#       // leaf_widgets[0].sync(leaf_widgets[3]);
#       // leaf_widgets[3].sync(leaf_widgets[0]);
#       '
#     )
#   )

  return(htmltools::browsable(tl))
}

#' @describeIn syncView alias for ease of typing
#' @aliases syncview
#' @export syncview
syncview <- function(...) syncView(...)
