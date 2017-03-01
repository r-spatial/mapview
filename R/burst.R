prepareData <- function(x, zcol, burst, ...) {

  if (is.null(zcol) & !burst) {
    return(x)
  } else if (burst) {
    return(burst(x, zcol, ...))
  } else {
    lst <- lapply(zcol, function(i) {
      x[, i, drop = FALSE]
    })
    names(lst) <- zcol
    return(lst)
  }

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
  print(inherits(lst, "list"))
  return(lst)
}

