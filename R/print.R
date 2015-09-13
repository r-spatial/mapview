setMethod('print', signature(x = "mapview"),
          function(x)
          {
            print(x@map)
          }
)
