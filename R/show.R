setMethod("show", signature(object = "mapview"),
          function(object)
          {
            print(object@map)
          }
)
