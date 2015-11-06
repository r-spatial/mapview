#' Convenience functions for brew plugin
#'
#' @author
#' Tim Appelhans, Florian Detsch
#'
#' @param x A 'SpatialPointsDataFrame'.
#' @param use_cpp Logical. If \code{TRUE}, the function makes use of
#' \strong{Rcpp} functionality which leads to a significant reduction in
#' computation time, particularly when processing large datasets.
#'
#' @name brewControls
NULL


# brew popup table for sp objects -----------------------------------------
#' @describeIn brewControls create popup table for sp objects
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

    df$x <- as.character(round(coordinates(x)[, 1], 2))
    df$y <- as.character(round(coordinates(x)[, 2], 2))

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

    mat <- df2String(x@data)
    colnames(mat) <- names(x@data)

    if (nrow(x) == 1) mat <- t(mat)

    mat <- cbind(mat, x = as.character(round(coordinates(x)[, 1], 2)))
    mat <- cbind(mat, y = as.character(round(coordinates(x)[, 2], 2)))

    cols <- colnames(mat)

    ## create list with row-specific html code
    lst_html <- listPopupTemplates(mat, cols,
                                   system.file("templates/popup.brew",
                                               package = "mapview"))
  }

  return(lst_html)
}


# create popup table odd row for sp objects ---------------------------------------
#' @describeIn brewControls create popup table odd row for sp objects
#'
#' @param col.name the name of the column of the attribute table
#' @param value the corresponding value from the attribute table
#'
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
#' @describeIn brewControls create popup table even row for sp objects
#'
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

