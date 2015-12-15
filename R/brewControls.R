# create popup table of attributes

brewPopupTable <- function(x, use_cpp = TRUE) {

  if (!use_cpp) {

    # data.frame with 1 column
    if (ncol(x@data) == 1) {
      df <- data.frame(as.character(x@data[, 1]))
    # data.frame with multiple columns
    } else {
      df <- data.frame(df2String(x@data), stringsAsFactors = FALSE)
    }

    names(df) <- names(x@data)

    if (nrow(x) == 1) df <- t(df)

    df$x <- as.character(round(sp::coordinates(x)[, 1], 2))
    df$y <- as.character(round(sp::coordinates(x)[, 2], 2))

    cols <- colnames(df)

    vals <- sapply(seq(nrow(x@data)), function(i) {
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
      if (ncol(x@data) == 1) {
        mat <- matrix(as.character(x@data[, 1]))
        # data.frame with multiple columns
      } else {
        mat <- df2String(x@data)
      }

      colnames(mat) <- names(x@data)
      # if (nrow(x) == 1) mat <- t(mat)
    }

    if (!class(x)[1] %in% c("SpatialLines", "SpatialLinesDataFrame")) {
      mat <- cbind(Longitude = as.character(round(sp::coordinates(x)[, 1], 2)),
                   Latitude = as.character(round(sp::coordinates(x)[, 2], 2)),
                   mat)
    }

    cols <- colnames(mat)

    ## create list with row-specific html code
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

