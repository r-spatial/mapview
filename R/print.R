#' Method for printing mapview objects
#'
#' @param x a mapview object
#'
#' @export
#'
setMethod('print', signature(x = "mapview"),
          function(x)
          {
            #print.mapview(x)
            print(mapview2leaflet(x))
          }
)
