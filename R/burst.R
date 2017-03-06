prepareData <- function(x, zcol, burst, ...) {

  if (is.null(zcol) & !burst) {
    return(x)
  } else return(x)
  # } else if (burst) {
  #   return(burst(x, zcol, ...))
  # } else {
  #   lst <- lapply(zcol, function(i) {
  #     x[, i, drop = FALSE]
  #   })
  #   names(lst) <- zcol
  #   return(lst)
  # }

}


burst <- function(x, zcol = NULL, ...) {
  if (is.null(zcol)) {
    nms <- colnames(sf2DataFrame(x))
    lst <- lapply(nms, function(i) {
      x[, i, drop = FALSE]
    })
    names(lst) <- nms
  } else {
    f <- as.factor(x[[zcol]])
    lst <- split(x[, zcol, drop = FALSE], f = f, drop = TRUE)
  }
  return(lst)
}


burstByColumn <- function(x,
                          color,
                          popup,
                          ...) {

  nms <- colnames(sf2DataFrame(x))
  x_lst <- lapply(nms, function(i) {
    x[, i, drop = FALSE]
  })
  names(x_lst) <- nms

  color_lst <- lapply(nms, function(i) {
    zcolColors(x[[i]], color = color)
  })

  popup_lst <- lapply(seq(nms), function(i) {
    popupTable(x)
  })

  return(list(x = x_lst,
              color = color_lst,
              popup = popup_lst))

}
