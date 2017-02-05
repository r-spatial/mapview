burst <- function(x, ...) UseMethod("burst")

burst.sf <- function(x, zcol = NULL, ...) {
  if (is.null(zcol)) {
    nms <- colnames(sf2DataFrame(x))
    lst <- lapply(nms, function(i) {
      x[, i]
    })
    names(lst) <- nms
  } else {
    f <- as.factor(x[[zcol]])
    lst <- split(x[, zcol], f = f, drop = TRUE)
  }
  return(lst)
}

