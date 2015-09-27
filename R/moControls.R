#' Convenience functions for whisker mustache plugin
#'
#' @author
#' Tim Appelhans
#'
#' @name moControls
NULL

# Create popup table for sp objects ---------------------------------------
#' @describeIn moControls create popup table for sp objects
# @export createPopupTable
#'
#' @param x a spatial object
#'
createPopupTable <- function(x) {

  if (class(x) %in% "SpatialPoints") {
    df <- data.frame(x = as.character(round(coordinates(x)[, 1], 2)),
                     y = as.character(round(coordinates(x)[, 2], 2)))
  } else {
    df <- as.data.frame(sapply(x@data, as.character),
                        stringsAsFactors = FALSE)

    if (nrow(x) == 1) df <- as.data.frame(t(df))
    if (!class(x) %in% c("SpatialLinesDataFrame", "SpatialLines")) {
      df$x <- as.character(round(coordinates(x)[, 1], 2))
      df$y <- as.character(round(coordinates(x)[, 2], 2))
    }
  }

  cols <- colnames(df)

  vals <- sapply(seq(nrow(df)), function(i) {
    df[i, ]
  })

  txt <- lapply(seq(nrow(df)), function(j) {

    pop <- sapply(seq(ncol(df)), function(i) {
      if (i%%2 == 0)
        createPopupRowAlt(cols[i], df[j, i])
      else
        createPopupRow(cols[i], df[j, i])
    })

    pop <- whisker::iteratelist(pop)

    popTemplate <- readLines(system.file("templates/popup.html",
                                         package = "mapview"))

    outputObj <- whisker::whisker.render(popTemplate)

    return(outputObj)
  })
  return(txt)
  print(txt)
}


# Create popup table odd row for sp objects -------------------------------
#' @describeIn moControls create popup table odd row for sp objects
# @export createPopupRow
#'
#' @param col.name the name of the column of the attribute table
#' @param value the corresponding value from the attribute table
#'
createPopupRow <- function(col.name, value) {

  paste0("<tr>",
         paste0("<td>",
                col.name,
                "</td>"),
         paste0("<td>",
                value,
                "</td>"),
         "</tr>")

}


# Create popup table even row for sp objects ------------------------------
#' @describeIn moControls create popup table even row for sp objects
# @export createPopupRowAlt
#'
createPopupRowAlt <- function(col.name, value) {

  paste0("<tr class='alt'>",
         paste0("<td>",
                col.name,
                "</td>"),
         paste0("<td>",
                value,
                "</td>"),
         "</tr>")

}


