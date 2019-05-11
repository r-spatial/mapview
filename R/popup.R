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

  .Deprecated(new = "leafpop::popupTable", package = "mapview",
              old = "mapview::popupTable")

  leafpop::popupTable(x = x,
                      zcol = zcol,
                      row.numbers = row.numbers,
                      feature.id = feature.id)

  # if (inherits(x, "sfc")) {
  #   return(NULL)
  # } else {
  #   if (!missing(zcol))
  #     x = x[, zcol, drop = FALSE]
  # }
  # brewPopupTable(x, row.numbers = row.numbers, feature.id = feature.id)
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

  .Deprecated(new = "leafpop::popupImage", package = "mapview",
              old = "mapview::popupImage")

  leafpop::popupImage(img = img,
                      src = src,
                      embed = embed,
                      ...)

  # if (!is.list(img)) img = as.list(img)
  # fex = sapply(img, file.exists)
  # srcs = sapply(fex, function(i) ifelse(i, "local", "remote"))
  #
  # pop = lapply(seq(img), function(i) {
  #   src = srcs[i]
  #   pop = switch(src,
  #                local = popupLocalImage(img = img[[i]], embed = embed, ...),
  #                remote = popupRemoteImage(img = img[[i]], ...))
  # })
  #
  # return(pop)

}


### local images -----
# popupLocalImage = function(img, width = NULL, height = NULL, embed = FALSE) {
#
#   .Deprecated(new = "leafpop::popupLocalImage", package = "mapview",
#               old = "mapview::popupLocalImage")
#
#   leafpop::popupLocalImage(img = img,
#                            width = width,
#                            height = height,
#                            embed = embed)

  # pngs = lapply(1:length(img), function(i) {
  #
  #   fl = img[[i]]
  #
  #   info = strsplit(
  #     sf::gdal_utils(
  #       util = "info",
  #       source = fl,
  #       quiet = TRUE
  #     ),
  #     split = "\n"
  #   )
  #   info = unlist(lapply(info, function(i) grep(glob2rx("Size is*"), i, value = TRUE)))
  #   cols = as.numeric(strsplit(gsub("Size is ", "", info), split = ", ")[[1]])[1]
  #   rows = as.numeric(strsplit(gsub("Size is ", "", info), split = ", ")[[1]])[2]
  #   yx_ratio = rows / cols
  #   xy_ratio = cols / rows
  #
  #   if (is.null(height) && is.null(width)) {
  #     width = 300
  #     height = yx_ratio * width
  #   } else if (is.null(height)) height = yx_ratio * width else
  #     if (is.null(width)) width = xy_ratio * height
  #
  #   if (embed) {
  #     plt64 = base64enc::base64encode(fl)
  #     pop = paste0("<img ",
  #                  " width=",
  #                  width,
  #                  " height=",
  #                  height,
  #                  " src='data:image/png;base64,", plt64, "' />")
  #   }
  #
  #   if (!embed) {
  #     nm = basename(fl)
  #     drs = file.path(tempdir(), "graphs")
  #     if (!dir.exists(drs)) dir.create(drs)
  #     fls = file.path(drs, nm)
  #     invisible(file.copy(fl, file.path(drs, nm)))
  #
  #     pop = paste0("<image src='../graphs/",
  #                  basename(img),
  #                  "' width=",
  #                  width,
  #                  " height=",
  #                  height,
  #                  ">")
  #   }
  #
  #   # return(uri)
  #   popTemplate = system.file("templates/popup-graph.brew", package = "mapview")
  #   myCon = textConnection("outputObj", open = "w")
  #   brew::brew(popTemplate, output = myCon)
  #   outputObj = outputObj
  #   close(myCon)
  #
  #   return(paste(outputObj, collapse = ' '))
  # })
  #
  # return(unlist(pngs))
  #
  #
  #
  # nm = basename(img)
  # drs = file.path(tempdir(), "graphs")
  # if (!dir.exists(drs)) dir.create(drs)
  # fls = file.path(drs, nm)
  # invisible(file.copy(img, file.path(drs, nm)))
  # rel_path = file.path("..", basename(drs), basename(img))
  #
  # # info = sapply(img, function(...) rgdal::GDALinfo(..., silent = TRUE))
  # info = sapply(img, function(...) gdalUtils::gdalinfo(...))
  # info = unlist(lapply(info, function(i) grep(glob2rx("Size is*"), i, value = TRUE)))
  # cols = as.numeric(strsplit(gsub("Size is ", "", info), split = ", ")[[1]])[1]
  # rows = as.numeric(strsplit(gsub("Size is ", "", info), split = ", ")[[1]])[2]
  # yx_ratio = rows / cols
  # xy_ratio = cols / rows
  #
  # if (missing(height) && missing(width)) {
  #   width = 300
  #   height = yx_ratio * width
  # } else if (missing(height)) height = yx_ratio * width else
  #   if (missing(width)) width = xy_ratio * height
  #
  # # maxheight = 2000
  # # width = width
  # # height = height + 5
  # pop = paste0("<image src='../graphs/",
  #              basename(img),
  #              "' width=",
  #              width,
  #              " height=",
  #              height,
  #              ">")
  #
  # popTemplate = system.file("templates/popup-graph.brew", package = "mapview")
  # myCon = textConnection("outputObj", open = "w")
  # brew::brew(popTemplate, output = myCon)
  # outputObj = outputObj
  # close(myCon)
  #
  # return(paste(outputObj, collapse = ' '))

# }


### remote images -----
# popupRemoteImage = function(img, width = 300, height = "100%") {
#
#   .Deprecated(new = "leafpop::popupRemoteImage", package = "mapview",
#               old = "mapview::popupRemoteImage")
#
#   leafpop::popupRemoteImage(img = img,
#                             width = width,
#                             height = height)

  # pop = paste0("<image src='",
  #              img,
  #              "' width=",
  #              width,
  #              " height=",
  #              height,
  #              ">")
  # maxheight = 2000
  # popTemplate = system.file("templates/popup-graph.brew", package = "mapview")
  # myCon = textConnection("outputObj", open = "w")
  # brew::brew(popTemplate, output = myCon)
  # outputObj = outputObj
  # close(myCon)
  #
  # return(paste(outputObj, collapse = ' '))
# }



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

  .Deprecated(new = "leafpop::popupGraph", package = "mapview",
              old = "mapview::popupGraph")

  leafpop::popupGraph(graphs = graphs,
                      type = type,
                      width = width,
                      height = height,
                      ...)

  # ## if a single feature is provided, convert 'graphs' to list
  # if (class(graphs)[1] != "list")
  #   graphs = list(graphs)
  #
  # ## create target folder and filename
  # drs = file.path(tempdir(), "popup_graphs")
  # if (!dir.exists(drs)) dir.create(drs)
  #
  # # type = type[1]
  # if (inherits(graphs[[1]], c("htmlwidget"))) {
  #   type = "html"
  # } else type = type[1]
  #
  # pop = switch(type,
  #              png = popupPNGraph(graphs = graphs, dsn = drs,
  #                                 width = width, height = height, ...),
  #              svg = popupSVGraph(graphs = graphs, dsn = drs,
  #                                 width = width, height = height, ...),
  #              html = popupHTMLGraph(graphs = graphs, dsn = drs,
  #                                    width = width, height = height, ...))
  #
  # # attr(pop, "popup") = "mapview"
  # return(pop)
}


### svg -----
# popupSVGraph = function(graphs, #dsn = tempdir(),
#                          width = 300, height = 300, ...) {
#
#   .Deprecated(new = "leafpop::popupSVGraph", package = "mapview",
#               old = "mapview::popupSVGraph")
#
#   leafpop::popupSVGraph(graphs = graphs,
#                         width = width,
#                         height = height,
#                         ...)

#   lapply(1:length(graphs), function(i) {
#     #nm = paste0("tmp_", i, ".svg")
#     #fls = file.path(dsn, nm)
#
#     inch_wdth = width / 72
#     inch_hght = height  / 72
#
#     #svg(filename = fls, width = inch_wdth, height = inch_hght, ...)
#     #print(graphs[[i]])
#     #dev.off()
#     lns <- svglite::svgstring(
#       width = inch_wdth,
#       height = inch_hght,
#       standalone = FALSE
#     )
#     print(graphs[[i]])
#     dev.off()
#
#     svg_str <- lns()
#
#     # this is a temporary solution to work around svglite
#     #   non-specific CSS styles
#     #   perhaps we should separate out into its own function/utility
#     #   also adds uuid dependency
#     svg_id <- paste0("x",uuid::UUIDgenerate())
#     svg_str <- gsub(
#       x = svg_str,
#       pattern = "<svg ",
#       replacement = sprintf("<svg id='%s'", svg_id)
#     )
#     # this style gets appended as defaults in all svglite
#     #    but might change if svglite changes
#     svg_css_rule <- sprintf(
#       "#%1$s line, #%1$s polyline, #%1$s polygon, #%1$s path, #%1$s rect, #%1$s circle {",
#       svg_id
#     )
#     svg_str <- gsub(
#       x = svg_str,
#       pattern = "line, polyline, polygon, path, rect, circle \\{",
#       replacement = svg_css_rule
#     )
#
#     #lns = paste(readLines(fls), collapse = "")
#     # file.remove(fls)
# #     return(
# # sprintf(
# # "
# # <div style='width: %dpx; height: %dpx;'>
# # %s
# # </div>
# # " ,
# #   width,
# #   height,
# #   svg_str
# # )
#     # )
#     pop = sprintf(
#       "<div style='width: %dpx; height: %dpx;'>%s</div>",
#       width,
#       height,
#       svg_str
#     )
#
#     popTemplate = system.file("templates/popup-graph.brew", package = "mapview")
#     myCon = textConnection("outputObj", open = "w")
#     brew::brew(popTemplate, output = myCon)
#     outputObj = outputObj
#     close(myCon)
#
#     return(paste(outputObj, collapse = ' '))
#
#   })
# }


### png -----
# popupPNGraph = function(graphs, dsn = tempdir(),
#                          width = 300, height = 300, ...) {
#
#   .Deprecated(new = "leafpop::popupPNGraph", package = "mapview",
#               old = "mapview::popupPNGraph")
#
#   leafpop:::popupPNGraph(graphs = graphs,
#                          dsn = dsn,
#                          width = width,
#                          height = height,
#                          ...)

  # # pngs = lapply(1:length(graphs), function(i) {
  # #   nm = paste0("tmp_", i, ".png")
  # #   fls = file.path(dsn, nm)
  # #
  # #   png(filename = fls, width = width, height = height, units = "px", ...)
  # #   print(graphs[[i]])
  # #   dev.off()
  # #
  # #   rel_path = file.path("..", basename(dsn), nm)
  # #   return(rel_path)
  # # })
  # #
  # # popupImage(pngs, width = width, height = height, src = "local")
  #
  # pngs = lapply(1:length(graphs), function(i) {
  #
  #   fl = tempfile(fileext = ".png")
  #
  #   png(filename = fl, width = width, height = height, units = "px", ...)
  #   print(graphs[[i]])
  #   dev.off()
  #
  #   plt64 = base64enc::base64encode(fl)
  #   pop = paste0('<img src="data:image/png;base64,', plt64, '" />')
  #
  #   # return(uri)
  #   popTemplate = system.file("templates/popup-graph.brew", package = "mapview")
  #   myCon = textConnection("outputObj", open = "w")
  #   brew::brew(popTemplate, output = myCon)
  #   outputObj = outputObj
  #   close(myCon)
  #
  #   return(paste(outputObj, collapse = ' '))
  # })
  #
  # return(pngs)
# }

### html -----
# popupHTMLGraph = function(graphs, dsn = tempdir(),
#                            width = 300, height = 300, ...) {
#
#   .Deprecated(new = "leafpop::popupHTMLGraph", package = "mapview",
#               old = "mapview::popupHTMLGraph")
#
#   leafpop::popupHTMLGraph(graphs = graphs,
#                           dsn = dsn,
#                           width = width,
#                           height = height,
#                           ...)

  # lapply(1:length(graphs), function(i) {
  #   nm = paste0("tmp_", i, ".html")
  #   fls = file.path(dsn, nm)
  #   htmlwidgets::saveWidget(graphs[[i]], fls, ...)
  #
  #   rel_path = file.path("..", basename(dsn))
  #
  #   popupIframe(file.path(rel_path, basename(fls)), width + 5, height + 5)
  #
  # })
# }


### iframe -----
# popupIframe = function(src, width = 300, height = 300) {
#
#   .Deprecated(new = "leafpop::popupIframe", package = "mapview",
#               old = "mapview::popupIframe")
#
#   leafpop::popupIframe(src = src,
#                        width = width,
#                        height = height)
#
#   # pop = paste0("<iframe src='",
#   #              src,
#   #              "' frameborder=0 width=",
#   #              width,
#   #              " height=",
#   #              height,
#   #              #" align=middle",
#   #              "></iframe>")
#   #
#   # popTemplate = system.file("templates/popup-graph.brew", package = "mapview")
#   # myCon = textConnection("outputObj", open = "w")
#   # brew::brew(popTemplate, output = myCon)
#   # outputObj = outputObj
#   # close(myCon)
#   #
#   # return(paste(outputObj, collapse = ' '))
# }



### controls ==============================================================
# create popup table of attributes
# brewPopupTable = function(x,
#                           width = 300,
#                           height = 300,
#                           row.numbers = TRUE,
#                           feature.id = TRUE) {
#
#   if (inherits(x, "Spatial")) x = x@data
#   if (inherits(x, "sf")) x = sf2DataFrame(x)
#
#   # ensure utf-8 for column names (column content is handled on C++ side)
#   colnames(x) = enc2utf8(colnames(x))
#
#   if (inherits(x, "SpatialPoints")) {
#     mat = NULL
#   } else {
#
#     # data.frame with 1 column
#     if (ncol(x) == 1 && names(x) == attr(x, "sf_column")) {
#       mat = as.matrix(class(x[, 1])[1])
#     } else if (ncol(x) == 1) {
#       mat = matrix(as.character(x[, 1]))
#     # data.frame with multiple columns
#     } else {
#
#       # check for list columns, if found format it
#       ids = which(sapply(x, is.list))
#
#       if (any(ids)) {
#         # nms = attr(ids, "names")[ids]
#         x[, ids] = sapply(sapply(x, class)[ids], "[[", 1) #format(x[, ids])
#         # #
#         # # for (i in nms) {
#         # #   x[[i]] = format(x[[i]])
#         # }
#       }
#
#       mat = df2String(x)
#     }
#
#     colnames(mat) = names(x)
#     # if (nrow(x) == 1) mat = t(mat)
#   }
#
#   if (feature.id) {
#     fid = rownames(x)
#     mat = cbind("Feature ID" = fid, mat)
#   }
#
#   ## create list with row-specific html code
#   cols = colnames(mat)
#
#   lst_html = listPopupTemplates(mat, cols,
#                                 system.file("templates/popup.brew",
#                                             package = "mapview"),
#                                 rowIndex = row.numbers)
#   attr(lst_html, "popup") = "mapview"
#   return(lst_html)
# }
#
#
# # create popup table odd row for sp objects -------------------------------
#
# brewPopupRow = function(col.name, value) {
#
#   paste0("<tr>",
#          paste0("<td>",
#                 col.name,
#                 "</td>"),
#          paste0("<td>",
#                 value,
#                 "</td>"),
#          "</tr>")
#
# }
#
#
# # create popup table even row for sp objects ------------------------------
#
# brewPopupRowAlt = function(col.name, value) {
#
#   paste0("<tr class='alt'>",
#          paste0("<td>",
#                 col.name,
#                 "</td>"),
#          paste0("<td>",
#                 value,
#                 "</td>"),
#          "</tr>")
#
# }


popupLayoutDependencies <- function() {
  list(
    htmltools::htmlDependency(
      "PopupTable",
      '0.0.1',
      system.file("htmlwidgets/lib/popup", package = "mapview"),
      stylesheet = 'popup.css'
    ))
}
