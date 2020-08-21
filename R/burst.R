burst <- function(x,
                  zcol,
                  burst,
                  ...) {

  if (is.null(zcol) & !burst) {
    x
  } else if (is.null(zcol) & burst) {
    burstByColumn(x = x,
                  zcol = zcol)
  } else if (!is.null(zcol) & burst & length(zcol) == 1) {
    burstByRow(x = x,
               zcol = zcol)
  } else if (length(zcol) > 1) {
    burstByColumn(x = x,
                  zcol = zcol)
  }

}


burstByColumn <- function(x,
                          zcol,
                          ...) {
  if (is.null(zcol)) zcol <- colnames(sf2DataFrame(x, drop_sf_column = TRUE))
  # zcol = nms

  x_lst <- lapply(zcol, function(i) {
    x[, i, drop = FALSE]
  })
  names(x_lst) <- zcol

  return(x_lst)
}


burstByRow <- function(x,
                       zcol,
                       ...) {
  x[[zcol]] <- as.character(x[[zcol]])
  x[[zcol]][is.na(x[[zcol]])] <- "NA"

  lst <- split(x, x[[zcol]], drop = TRUE) #[as.character(x[[zcol]])]

  return(lst)
}

