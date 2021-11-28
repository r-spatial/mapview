printMapview = function (x) {

  ## normal htmlwidget printing for notebooks etc.
  ## set options fgb & georaster to FALSE!!
  if (!isTRUE(mapviewGetOption("fgb")) & !isTRUE(mapviewGetOption("georaster"))) {
    print(mapview2leaflet(x))
    # invisible(x)
    return(invisible())
  }

  ## convert to leaflet object
  x = mapview2leaflet(x)
  viewer = getOption("viewer")
  ide = get_ide()
  if (mapviewGetOption("viewer.suppress")) {
    viewer = NULL
  }
  if (!is.null(viewer)) {
    viewerFunc = function(url) {
      paneHeight = x$sizingPolicy$viewer$paneHeight
      if (identical(paneHeight, "maximize")) {
        paneHeight = -1
      }
      if (ide == "vscode") {
        # VSCode's viewer can't ignore cross-origin requests. Need to serve the
        # map so assests can be read, e.g. .fgb files.
        server <- servr::httd(
            dir = get_url_dir(url),
            verbose = FALSE,
            browser = FALSE
          )
        url <- server$url
        
      }
      viewer(url, height = paneHeight)
    }
  } else {
    viewerFunc = function(url) {
      dir = get_url_dir(url)
      switch(ide,
        "rstudio" = if (mapviewGetOption("viewer.suppress")) {
          fl = file.path(dir, "index.html")
          utils::browseURL(fl) 
          } else {
            servr::httd(
              dir = dir, 
              verbose = FALSE
            )
          },
          "vscode" = servr::httd(
            dir = dir,
            verbose = FALSE
          ),
          # default
          servr::httd(
            dir = dir,
            verbose = FALSE
          )
          )
    }
  }
  htmltools::html_print(
    htmltools::as.tags(x, standalone = TRUE)
    , viewer = if (interactive()) viewerFunc
  )
  invisible(x)
}

#' Method for printing mapview objects
#' @param x a mapview object
#'
setMethod('print', signature(x = "mapview"), printMapview)

#' Method for printing mapview objects (show)
#' @param object a mapview object
setMethod("show", signature(object = "mapview"),
          function(object) {
            print(object)
          }
)


#' Print functions for mapview objects used in knitr
#'
#' @param x A mapview object
#' @param ... further arguments passed on to \code{\link[knitr]{knit_print}}
#'
#' @rawNamespace
#' if(getRversion() >= "3.6.0") {
#'   S3method(knitr::knit_print, mapview)
#' } else {
#'   export(knit_print.mapview)
#' }
#'
knit_print.mapview = function(x, ...) {
  knitr::knit_print(mapview2leaflet(x), ...)
}

get_ide = function() {
  if (is_rstudio()) {
    return("rstudio") 
  } else if (is_vscode()) {
    return("vscode")
  } else {
    "other"
  }
}

is_rstudio = function() {
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    rstudioapi::isAvailable() && rstudioapi::versionInfo()$mode != "vscode"
  } else {
    FALSE
  }
}

is_vscode = function() {
    # can we find .vsc$attach() ?
    exists(".vsc") && exists("attach", envir = .vsc)
}

get_url_dir <- function(url) gsub("file://|/index.html", "", url)