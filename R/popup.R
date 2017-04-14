#' Create HTML strings for popups
#'
#' @description
#' Create HTML strings for \code{popup} tables used as input for
#' \code{\link{mapview}} or \code{\link{leaflet}}.
#' This optionally allows the user to include only a subset of feature attributes.
#'
#' @param x A \code{Spatial*} object.
#' @param zcol \code{numeric} or \code{character} vector indicating the columns
#' included in the output popup table. If missing, all columns are displayed.
#' @param use_cpp \code{logical} determining whether or not to enable
#' \strong{Rcpp} functionality.
#'
#' @return
#' A \code{list} of HTML strings required to create feature popup tables.
#'
#' @examples
#' \dontrun{
#'
#' ## include columns 1 and 2 only
#' mapview(franconia, popup = popupTable(franconia, zcol = 1:2))
#' mapview(breweries, zcol = "founded", legend = TRUE,
#'         popup = popupTable(breweries, zcol = c("founded", "village")))
#' leaflet() %>% addCircleMarkers(data = breweries)
#' leaflet() %>% addCircleMarkers(data = breweries,
#'                                popup = popupTable(breweries))
#' }
#'
#' @export popupTable
#' @name popupTable
#' @rdname popup
popupTable <- function(x, zcol, use_cpp = TRUE) {

  if (inherits(x, "sfc")) {
    return(NULL)
  } else {
    if (!missing(zcol))
      x <- x[, zcol, drop = FALSE]
    brewPopupTable(x, use_cpp = use_cpp)
  }
}


#' Create HTML strings for popups
#'
#' @description
#' Create HTML strings for \code{popup} images used as input for
#' \code{\link{mapview}} or \code{\link{leaflet}}.
#'
#' @param img A character \code{vector} of file path(s) or
#' web-URL(s) to any sort of image file(s).
#' @param src Whether the source is "local" (i.e. valid file path(s)) or
#' "remote" (i.e. valid URL(s)).
#' @param ... further arguments passed on to underlying methods such as
#' height and width.
#'
#' @return
#' A \code{list} of HTML strings required to create popup graphs.
#'
#' @examples
#' \dontrun{
#' ## remote images -----
#' ### one image
#' library(sf)
#'
#' pnt <- st_as_sf(data.frame(x = 174.764474, y = -36.877245),
#'                 coords = c("x", "y"),
#'                 crs = 4326)
#'
#' img <- "http://bit.ly/1TVwRiR"
#'
#' mapview(pnt, popup = popupImage(img, src = "remote"))
#'
#' ### multiple file (types)
#' library(sp)
#' images <- c(img,
#'             "https://upload.wikimedia.org/wikipedia/commons/1/1b/R_logo.svg",
#'             "https://www.r-project.org/logo/Rlogo.png",
#'             "https://upload.wikimedia.org/wikipedia/commons/d/d6/MeanMonthlyP.gif")
#'
#' pt4 <- data.frame(x = jitter(rep(174.764474, 4), factor = 0.01),
#'                   y = jitter(rep(-36.877245, 4), factor = 0.01))
#' coordinates(pt4) <- ~ x + y
#' proj4string(pt4) <- "+init=epsg:4326"
#'
#' mapview(pt4, popup = lapply(images, popupImage)) # NOTE the gif animation
#'
#' ## local images -----
#' pnt <- st_as_sf(data.frame(x = 174.764474, y = -36.877245),
#'                 coords = c("x", "y"), crs = 4326)
#' img <- system.file("img","Rlogo.png",package="png")
#' mapview(pnt, popup = popupImage(img))
#' }
#'
#' @export popupImage
#' @name popupImage
#' @rdname popup
popupImage <- function(img, src = c("local", "remote"), ...) {

  src = ifelse(file.exists(img), "local", "remote")[1]
  pop <- switch(src,
                local = popupLocalImage(img = img, ...),
                remote = popupRemoteImage(img = img, ...))

  return(pop)

}


### local images -----
popupLocalImage <- function(img, width, height) {
  nm <- basename(img)
  drs <- file.path(tempdir(), "graphs")
  if (!dir.exists(drs)) dir.create(drs)
  fls <- file.path(drs, nm)
  invisible(file.copy(img, file.path(drs, nm)))
  rel_path <- file.path("..", basename(drs), basename(img))

  # info <- sapply(img, function(...) rgdal::GDALinfo(..., silent = TRUE))
  info = sapply(img, function(...) gdalUtils::gdalinfo(...))
  info = unlist(lapply(info, function(i) grep(glob2rx("Size is*"), i, value = TRUE)))
  cols = as.numeric(strsplit(gsub("Size is ", "", info), split = ", ")[[1]])[1]
  rows = as.numeric(strsplit(gsub("Size is ", "", info), split = ", ")[[1]])[2]
  yx_ratio <- rows / cols
  xy_ratio <- cols / rows

  if (missing(height) && missing(width)) {
    width <- 300
    height <- yx_ratio * width
  } else if (missing(height)) height <- yx_ratio * width else
    if (missing(width)) width <- xy_ratio * height

  # maxheight = 2000
  # width = width
  # height = height + 5
  pop = paste0("<image src='../graphs/",
               basename(img),
               "' width=",
               width,
               " height=",
               height,
               ">")

  popTemplate <- system.file("templates/popup-graph.brew", package = "mapview")
  myCon <- textConnection("outputObj", open = "w")
  brew::brew(popTemplate, output = myCon)
  outputObj <- outputObj
  close(myCon)

  return(paste(outputObj, collapse = ' '))

}


### remote images -----
popupRemoteImage <- function(img, width = 300, height = "100%") {
  pop = paste0("<image src='",
               img,
               "' width=",
               width,
               " height=",
               height,
               ">")
  maxheight = 2000
  popTemplate <- system.file("templates/popup-graph.brew", package = "mapview")
  myCon <- textConnection("outputObj", open = "w")
  brew::brew(popTemplate, output = myCon)
  outputObj <- outputObj
  close(myCon)

  return(paste(outputObj, collapse = ' '))
}



#' Create HTML strings for popups
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
#' @param width popup width in pixels.
#' @param height popup height in pixels.
#'
#' @return
#' A \code{list} of HTML strings required to create popup graphs.
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
#' mapview(meuse, popup = popupGraph(p, type = "svg"))
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
#' mapview(breweries[1, ], map.types = "Esri.WorldImagery",
#'         popup = popupGraph(mapview(breweries[1, ])@map,
#'                            type = "html",
#'                            width = 500,
#'                            height = 400))
#' }
#'
#' @export popupGraph
#' @name popupGraph
#' @rdname popup
popupGraph <- function(graphs, type = c("png", "svg", "html"),
                       width = 300, height = 300, ...) {

  ## if a single feature is provided, convert 'graphs' to list
  if (class(graphs)[1] != "list")
    graphs <- list(graphs)

  ## create target folder and filename
  drs <- file.path(tempdir(), "popup_graphs")
  if (!dir.exists(drs)) dir.create(drs)

  # type <- type[1]
  if (inherits(graphs[[1]], c("htmlwidget"))) {
    type <- "html"
  } else type <- type[1]

  pop <- switch(type,
                png = popupPNGraph(graphs = graphs, dsn = drs,
                                   width = width, height = height, ...),
                svg = popupSVGraph(graphs = graphs, dsn = drs,
                                   width = width, height = height, ...),
                html = popupHTMLGraph(graphs = graphs, dsn = drs,
                                      width = width, height = height, ...))

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
popupSVGraph <- function(graphs, dsn = tempdir(),
                         width = 300, height = 300, ...) {
  lapply(1:length(graphs), function(i) {
    nm <- paste0("tmp_", i, ".svg")
    fls <- file.path(dsn, nm)

    inch_wdth = width / 72
    inch_hght = height  / 72

    svg(filename = fls, width = inch_wdth, height = inch_hght, ...)
    print(graphs[[i]])
    dev.off()

    lns <- paste(readLines(fls), collapse = "")
    # file.remove(fls)
    return(lns)
  })
}


### png -----
popupPNGraph <- function(graphs, dsn = tempdir(),
                         width = 300, height = 300, ...) {
  lapply(1:length(graphs), function(i) {
    nm <- paste0("tmp_", i, ".png")
    fls <- file.path(dsn, nm)

    png(filename = fls, width = width, height = height, units = "px", ...)
    print(graphs[[i]])
    dev.off()

    rel_path <- file.path("..", basename(dsn))

    pop = paste0("<img src = ", file.path(rel_path, basename(fls)), ">")
    # maxheight = 2000
    # wdth = paste0(width, "px;")
    # hght = paste0(height, "px;")

    popTemplate <- system.file("templates/popup-graph.brew", package = "mapview")
    myCon <- textConnection("outputObj", open = "w")
    brew::brew(popTemplate, output = myCon)
    outputObj <- outputObj
    close(myCon)

    return(paste(outputObj, collapse = ' '))
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

    popupIframe(file.path(rel_path, basename(fls)), width + 5, height + 5)

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
  pop = paste0("<iframe src='",
               src,
               "' frameborder=0 width=",
               width,
               " height=",
               height,
               #" align=middle",
               "></iframe>")
  # wdth = paste0(width, "px;")
  # hght = paste0(height, "px;")
  # maxheight = 2000

  popTemplate <- system.file("templates/popup-graph.brew", package = "mapview")
  myCon <- textConnection("outputObj", open = "w")
  brew::brew(popTemplate, output = myCon)
  outputObj <- outputObj
  close(myCon)

  return(paste(outputObj, collapse = ' '))
}



### controls ==============================================================
# create popup table of attributes
brewPopupTable <- function(x, width = 300, height = 300, use_cpp = TRUE) {

  if (inherits(x, "Spatial")) x <- x@data
  if (inherits(x, "sf")) x <- sf2DataFrame(x)

  if (!use_cpp) {

    # data.frame with 1 column
    if (ncol(x) == 1) {
      df <- data.frame(as.character(x[, 1]))
      # data.frame with multiple columns
    } else {
      df <- data.frame(df2String(x), stringsAsFactors = FALSE)
    }

    names(df) <- names(x)

    if (nrow(x) == 1) df <- t(df)

    # df$x <- as.character(round(sp::coordinates(x)[, 1], 2))
    # df$y <- as.character(round(sp::coordinates(x)[, 2], 2))

    cols <- colnames(df)

    vals <- sapply(seq(nrow(x)), function(i) {
      df[i, ]
    })

    indodd <- seq(1, ncol(df), by = 2)
    indeven <- seq(2, ncol(df), by = 2)

    lst_html <- lapply(seq(nrow(df)), function(j) {
      odd <- sapply(indodd, function(i) {
        brewPopupRow(cols[i], df[j, i])
      })

      even <- sapply(indeven, function(i) {
        brewPopupRowAlt(cols[i], df[j, i])
      })

      pop <- vector("character", length(cols))

      pop[indodd] <- odd
      pop[indeven] <- even

      popTemplate <- system.file("templates/popup.brew", package = "mapview")
      myCon <- textConnection("outputObj", open = "w")
      brew::brew(popTemplate, output = myCon)
      outputObj <- outputObj
      close(myCon)

      return(paste(outputObj, collapse = ' '))
    })

  } else {

    cls <- class(x)[1]
    if (cls == "SpatialPoints") {
      mat <- NULL
    } else {

      #     if (cls == "SpatialLines") {
      #       x_pts <- sp::getSpatialLinesMidPoints(x)
      #       x <- SpatialLinesDataFrame(x, data = data.frame(x = coordinates(x_pts)[, 1],
      #                                                       y = coordinates(x_pts)[, 2]))
      #     }

      # data.frame with 1 column
      if (ncol(x) == 1) {
        mat <- matrix(as.character(x[, 1]))
        # data.frame with multiple columns
      } else {

        # check for list columns, if found supply suitable class info for printing
        ids <- sapply(x, function(i) is.list(i))

        if (any(ids)) {
          nms <- attr(ids, "names")[ids]
          for (i in nms) {
            x[, i] <- format(x[[i]]) #paste("object of class", class(x[[i]])[1])
          }
        }

        mat <- df2String(x)
      }

      colnames(mat) <- names(x)
      # if (nrow(x) == 1) mat <- t(mat)
    }

    # if (!class(x)[1] %in% c("SpatialLines",
    #                         "SpatialLinesDataFrame",
    #                         "SpatialPolygons",
    #                         "SpatialPolygonsDataFrame")) {
    #   mat <- cbind(Longitude = as.character(round(sp::coordinates(x)[, 1], 2)),
    #                Latitude = as.character(round(sp::coordinates(x)[, 2], 2)),
    #                mat)
    # }

    ## add 'feature id' in case of spydf, slndf
    #if (length(grep("DataFrame", class(x))) > 0) {
    fid <- rownames(x)
    mat <- cbind("Feature ID" = fid, mat)
    #}

    ## create list with row-specific html code
    cols <- colnames(mat)

    lst_html <- listPopupTemplates(mat, cols,
                                   system.file("templates/popup.brew",
                                               package = "mapview"))
  }

  return(lst_html)
}


# create popup table odd row for sp objects ---------------------------------------

brewPopupRow <- function(col.name, value) {

  paste0("<tr>",
         paste0("<td>",
                col.name,
                "</td>"),
         paste0("<td>",
                value,
                "</td>"),
         "</tr>")

}


# create popup table even row for sp objects ---------------------------------------

brewPopupRowAlt <- function(col.name, value) {

  paste0("<tr class='alt'>",
         paste0("<td>",
                col.name,
                "</td>"),
         paste0("<td>",
                value,
                "</td>"),
         "</tr>")

}

