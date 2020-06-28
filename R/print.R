#' Method for printing mapview objects
#' @param x a mapview object
setMethod('print', signature(x = "mapview"),
          function(x) {
            #print.mapview(x)
            print(mapview2leaflet(x))
          }
)

#' Method for printing mapview objects (show)
#' @param object a mapview object
setMethod("show", signature(object = "mapview"),
          function(object)
          {
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
knit_print.mapview <- function(x, ...) {
  knitr::knit_print(mapview2leaflet(x), ...)
}
