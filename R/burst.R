burst <- function(x,
                  zcol,
                  burst,
                  color,
                  col.regions,
                  at,
                  na.color,
                  popup,
                  alpha,
                  alpha.regions,
                  na.alpha,
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
                             col.regions = col.regions,
                             at = at,
                             na.color = na.color,
                             popup = popup,
                             alpha = alpha,
                             alpha.regions = alpha.regions,
                             na.alpha = na.alpha)
  } else if (!is.null(zcol) & burst & length(zcol) == 1) {
    function() burstByRow(x = x,
                          zcol = zcol,
                          burst = burst,
                          color = color,
                          col.regions = col.regions,
                          at = at,
                          na.color = na.color)
  } else if (length(zcol) > 1) {
    nms = colnames(sf2DataFrame(x[, zcol], drop_sf_column = TRUE))
    function() burstByColumn(x = x,
                             color = color,
                             col.regions = col.regions,
                             at = at,
                             na.color = na.color,
                             popup = popup,
                             nms = nms,
                             alpha = alpha,
                             alpha.regions = alpha.regions,
                             na.alpha = na.alpha)
  }

}


burstByColumn <- function(x,
                          color,
                          col.regions,
                          at,
                          na.color,
                          popup = popupTable(x),
                          nms = NULL,
                          alpha,
                          alpha.regions,
                          na.alpha,
                          ...) {

  if (is.null(nms)) nms <- colnames(sf2DataFrame(x, drop_sf_column = TRUE))

  x_lst <- lapply(nms, function(i) {
    x[, i, drop = FALSE]
  })
  names(x_lst) <- nms

  popup <- rep(list(popup), length(x_lst))

  color_lst <- lapply(nms, function(i) {
    vectorColors(x, zcol = i, colors = color, at = at, na.color = na.color)
  })

  colregions_lst <- lapply(nms, function(i) {
    vectorColRegions(x, zcol = i, col.regions = col.regions,
                     at = at, na.color = na.color)
  })

  labs <- lapply(nms, function(i) {
    makeLabels(x, zcol = i)
  })

  alpha_lst = lapply(nms, function(i) {
    na.alpha = ifelse(na.alpha == 0, 0.001, na.alpha)
    alpha = rep(alpha, length(x[[i]]))
    alpha[is.na(x[[i]])] = na.alpha
    return(alpha)
  })

  alpharegions_lst = lapply(nms, function(i) {
    na.alpha = ifelse(na.alpha == 0, 0.001, na.alpha)
    alpha.regions = rep(alpha.regions, length(x[[i]]))
    alpha.regions[is.na(x[[i]])] = na.alpha
    return(alpha.regions)
  })

  return(list(obj = x_lst,
              color = color_lst,
              col.regions = colregions_lst,
              popup = popup,
              labs = labs,
              alpha = alpha_lst,
              alpha.regions = alpharegions_lst))

}


burstByRow <- function(x,
                       zcol,
                       burst,
                       color,
                       col.regions,
                       at,
                       na.color,
                       ...) {

  x[[zcol]] <- as.character(x[[zcol]])
  x[[zcol]][is.na(x[[zcol]])] <- "NA"

  lst <- split(x, x[[zcol]], drop = TRUE) #[as.character(x[[zcol]])]

  vec <- names(lst)
  vec[vec == "NA"] <- NA

  if (getGeometryType(x) == "ln") {
    color <- as.list(zcolColors(x = vec,
                                colors = color,
                                at = at,
                                na.color = na.color))
    names(color) <- names(lst)
  } else {
    color <- standardColor(x)
  }

  col.regions <- as.list(zcolColors(x = vec,
                                    colors = col.regions,
                                    at = at,
                                    na.color = na.color))

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
              col.regions = col.regions,
              popup = popup,
              labs = labs))
}

