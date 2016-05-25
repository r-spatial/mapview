#' View two or more possibly synchronised mapview or leaflet maps
#'
#' @description
#' This function produces a view of two or more maps. It is possible to sync
#' any combination of panels or all or none.
#'
#' @param ... any number of mapview or leaflet objects or a list thereof
#' @param ncol how many columns should be plotted
#' @param sync whether to synchronise zoom and pan for certain elements.
#' Possible values are "all" (default) to sync all maps, "none" to disable
#' synchronisation or a list of panel numbers, e.g. \code{list(c(1, 3), c(2, 4))}
#' will synchronise panels 1 & 3 and panels 2 & 4. Panels are drawn from top right
#' to bottom left.
#' @param sync.curser whether to show curser position in synced panels (default TRUE).
#' @param no.initial.sync whether to sync the initial view (default TRUE).
#'
#' @examples
#' \dontrun{
#' library(sp)
#'
#' data(meuse)
#' coordinates(meuse) <- ~x+y
#' proj4string(meuse) <- CRS("+init=epsg:28992")
#'
#' ## view different aspects of same data set
#' m1 <- mapview(meuse, zcol = "soil", burst = TRUE)
#' m2 <- mapview(meuse, zcol = "lead")
#' m3 <- mapview(meuse, zcol = "landuse")
#' m4 <- mapview(meuse, zcol = "dist.m")
#'
#' latticeView(m1, m2, m3, m4) # 4 panels
#' latticeView(m1, m2, m3, m4, sync.curser = FALSE) # 4 panels
#' latticeView(m1, m2) # 2 panels, split vertical
#' latticeView(m1, m2, ncol = 1) # 2 panels split horizontal
#' latticeView(m1, m2, m3, m4, sync = list(c(1, 2), c(3, 4))) # individual syncing
#' latticeView(m1, m2, m3, m4, sync = list(c(1, 2, 4)))
#'
#' ## view all layers of raster stack - not very resposive!!
#' map_list <- lapply(seq(nlayers(poppendorf)), function(i) {
#'   mapview(poppendorf[[i]], layer.name = names(poppendorf)[i])
#' })
#'
#' latticeView(map_list, ncol = 3)
#'
#' ## view multiple data sets
#' m1 <- mapview(meuse, zcol = "soil", burst = TRUE)
#' m2 <- mapview(atlStorms2005, zcol = "Name")
#' m3 <- mapview(breweries91)
#' m4 <- mapview(gadmCHE, color = "cornflowerblue")
#'
#' latticeView(m1, m2, m3, m4, sync = "none") # not synced
#' latticeView(m1, m2, m3, m4) # synced
#' latticeView(m1, m2, m3, m4, no.initial.sync = FALSE) # all maps zoomed to m4 extent
#'
#' }
#'
#' @export latticeView
#' @name latticeView
#' @aliases latticeView
#'

latticeView <- function(...,
                        ncol = 2,
                        sync = "all",
                        sync.curser = TRUE,
                        no.initial.sync = TRUE) {

  ## convert all ... objects to list or extract list if list was passed
  ls <- list(...)
  if (length(ls) == 1) ls <- ls[[1]]

  ## include dependency Leaflet.Sync in all maps
  sync_dep <- htmltools::htmlDependency(
    name = "Leaflet.Sync"
    ,version = "0.0.5"
    ,src = c(file = system.file("htmlwidgets/lib/Leaflet.Sync",
                                package = "mapview"))
    ,script = "L.Map.Sync.js"
  )

  for (i in seq(ls)) {
    if (inherits(ls[[i]], "mapview")) ls[[i]] <- mapview2leaflet(ls[[i]])
    if(length(ls[[i]]$dependencies) == 0){
      ls[[i]]$dependencies = list()
    }
    ls[[i]]$dependencies[[length(ls[[i]]$dependencies) + 1]] <- sync_dep
  }

  ## calculate div width depending on ncol and set div style
  wdth <- paste0("width:", round(1 / ncol * 100, 0) - 1, "%;")
  styl <- paste0("display:inline;",
                 wdth,
                 "float:left;border-style:solid;border-color:#BEBEBE;border-width:1px 1px 1px 1px;")

  ## htmltools stuff ... ?
  tg <- lapply(seq(ls), function(i) {
    htmltools::tags$div(style = styl, ls[[i]])
  })

  ## string operations for syncing, depending on sync argument
  if (is.list(sync)) {
    sync_strng <- Reduce(paste0, sapply(seq(sync), function(i) {
      first <-  do.call(c, lapply(seq(sync[[i]]), function(j) {
        rep(paste0("leaf_widgets[", sync[[i]][[j]] - 1, "]"), length(sync[[i]]) - 1)
      }))
      second <- rev(do.call(c, lapply(seq(sync[[i]]), function(j) {
        paste0(".sync(leaf_widgets[", sync[[i]][[j]] - 1,
               "], {syncCursor: ",
               tolower(sync.curser),
               ", noInitialSync: ",
               tolower(no.initial.sync),
               "});")
      })))
      paste0(first, second)
    }))
  } else if (sync == "all") {
    sync_strng <- Reduce(paste0, sapply(seq(ls), function(i) {
      first <-  do.call(c, lapply(seq(ls), function(j) {
        paste0("leaf_widgets[", j - 1, "]")
      }))[-i]
      second <- paste0(".sync(leaf_widgets[", i - 1,
                       "], {syncCursor: ",
                       tolower(sync.curser),
                       ", noInitialSync: ",
                       tolower(no.initial.sync),
                       "});")
      paste0(first, second)
    }))
  } else sync_strng <- ""
print(sync_strng)
  ## more htmltools stuff... ??
  tl <- htmltools::tagList(
    # htmltools::tags$head(htmltools::tags$script(
    #   type="text/javascript",
    #   src="https://cdn.rawgit.com/turban/Leaflet.Sync/master/L.Map.Sync.js"
    # )),
    tg,
    htmlwidgets::onStaticRenderComplete(
      paste0('var leaf_widgets = Array.prototype.map.call(
        document.querySelectorAll(".leaflet"),
          function(ldiv){
            return HTMLWidgets.find("#" + ldiv.id);
          }
        );',
             sync_strng
      )
    )
  )

  return(htmltools::browsable(tl))
}

#' @describeIn latticeView alias for ease of typing
#' @aliases latticeview
#' @export latticeview
latticeview <- function(...) latticeView(...)


# Reduce(paste0, sapply(seq(ls), function(i) {
#   first <-  do.call(c, lapply(seq(ls), function(j) {
#     paste0("leaf_widgets[", j - 1, "]")
#   }))
#   paste0(first, ".sync(leaf_widgets[", i - 1, "]);")
# }))
