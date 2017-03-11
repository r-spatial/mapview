burst <- function(x,
                  zcol,
                  burst,
                  color,
                  at,
                  na.color,
                  popup,
                  ...) {

  if (is.character(burst)) {
    zcol <- burst
    burst <- TRUE
  }

  if (is.null(zcol) & !burst) {
    x
  } else if (is.null(zcol) & burst) {
    function() burstByColumn(x = x,
                             color = color,
                             at = at,
                             na.color = na.color,
                             popup = popup)
  } else if (!is.null(zcol) & burst) {
    function() burstByRow(x = x,
                          zcol = zcol,
                          burst = burst,
                          color = color,
                          at = at,
                          na.color = na.color)
  }

}


burstByColumn <- function(x,
                          color,
                          at,
                          na.color,
                          popup = popupTable(x),
                          ...) {

  nms <- colnames(sf2DataFrame(x, remove_sf_column = TRUE))
  x_lst <- lapply(nms, function(i) {
    x[, i, drop = FALSE]
  })
  names(x_lst) <- nms

  popup <- rep(list(popup), length(x_lst))

  color_lst <- lapply(nms, function(i) {
    vectorColors(x, zcol = i, colors = color, at = at, na.color = na.color)
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
                       at,
                       na.color,
                       ...) {

  x[[zcol]] <- as.character(x[[zcol]])
  x[[zcol]][is.na(x[[zcol]])] <- "NA"

  lst <- split(x, x[[zcol]], drop = TRUE) #[as.character(x[[zcol]])]

  vec <- names(lst)
  vec[vec == "NA"] <- NA

  color <- as.list(zcolColors(x = vec,
                              colors = color,
                              at = at,
                              na.color = na.color))
  names(color) <- names(lst)

  popup <- popupTable(x)
  names(popup) <- as.character(x[[zcol]])
  popup <- lapply(names(lst), function(i) {
    tst <- popup[names(popup) %in% i]
    names(tst) <- NULL
    return(tst)
  })

  labs <- lapply(lst, makeLabels, zcol = zcol)

  # x <- x[, zcol, drop = FALSE]

  return(list(obj = lst,
              color = color,
              popup = popup,
              labs = labs))
}

