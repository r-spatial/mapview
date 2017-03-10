burst <- function(x,
                  zcol,
                  burst,
                  color,
                  popup,
                  ...) {

  if (is.character(burst)) {
    zcol <- burst
    burst <- TRUE
  }

  if (is.null(zcol) & !burst) {
    x
  } else if (is.null(zcol) & burst) {
    function() burstByColumn(x = x, color = color, popup = popup)
  } else {
    function() burstByRow(x = x, zcol = zcol, burst = burst, color = color)
  }

}


burstByColumn <- function(x,
                          color,
                          popup = popupTable(x),
                          ...) {

  nms <- colnames(sf2DataFrame(x, remove_sf_column = TRUE))
  x_lst <- lapply(nms, function(i) {
    x[, i, drop = FALSE]
  })
  names(x_lst) <- nms

  color_lst <- lapply(nms, function(i) {
    vectorColors(x, zcol = i, color = color)
  })

  labs <- lapply(nms, function(i) {
    makeLabels(x, zcol = i)
  })

  return(list(obj = x_lst,
              color = color_lst,
              popup = popup,
              labs = labs))

}

# implement as.factor!!!
# implement legend

burstByRow <- function(x,
                       zcol,
                       burst,
                       color,
                       ...) {

  popup <- popupTable(x)
  color <- as.list(vectorColors(x, zcol, color))

  x[[zcol]] <- as.character(x[[zcol]])
  x[[zcol]][is.na(x[[zcol]])] <- "NA"

  x <- x[, zcol, drop = FALSE]
  lst <- split(x, x[[zcol]])[as.character(x[[zcol]])]

  labs <- lapply(lst, makeLabels, zcol = zcol)

  return(list(obj = lst,
              color = color,
              popup = popup,
              labs = labs))
}

