#' Convenience functions for brew plugin
#'
#' @author
#' Tim Appelhans, Florian Detsch
#'
#' @name brewControls
NULL


# brew popup table for sp objects -----------------------------------------
#' @describeIn brewControls create popup table for sp objects
brewPopupTable <- function(x) {
  # df <- as.data.frame(sapply(x@data, as.character),
  #                     stringsAsFactors = FALSE)
  df <- data.frame(mapview:::df2String(x@data), stringsAsFactors = FALSE)
  names(df) <- names(x@data)

  if (nrow(x) == 1) df <- t(df)

  df$x <- as.character(round(coordinates(x)[, 1], 2))
  df$y <- as.character(round(coordinates(x)[, 2], 2))

  cols <- colnames(df)

  # vals <- sapply(seq(nrow(x@data)), function(i) {
  #   df[i, ]
  # })

  indodd <- seq(1, ncol(df), by = 2)
  indeven <- seq(2, ncol(df), by = 2)

  txt <- lapply(seq(nrow(df)), function(j) {
    #     odd <- sapply(indodd, function(i) {
    #       brewPopupRow(cols[i], df[j, i])
    #     })
    #
    #     even <- sapply(indeven, function(i) {
    #       brewPopupRowAlt(cols[i], df[j, i])
    #     })
    #
    #     pop <- vector("character", length(cols))
    #
    #     pop[indodd] <- odd
    #     pop[indeven] <- even
    #
    #     mapview:::mergePopupRows(cols, as.character(df[j, ]))
    #
    #     popTemplate <- system.file("templates/popup.brew", package = "mapview")
    #     myCon <- textConnection("outputObj", open = "w")
    #     brew::brew(popTemplate, output = myCon)
    #     outputObj <- outputObj
    #     close(myCon)
    #
    #     mapview:::htmlTemplate(cols, unlist(df[j, ]))
    #
    #     return(paste(outputObj, collapse = ' '))

    mapview:::htmlTemplate(cols, unlist(df[j, ]))
  })
  return(txt)
  print(txt)
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

