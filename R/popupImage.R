#' Create HTML strings for popup graphs
#'
#' @description
#' Create HTML strings for \code{popup} graphs used as input for
#' \code{\link{mapview}} or \code{\link{leaflet}}.
#'
#' @param img A character \code{vector} of file path(s) or
#' web-URL(s) to any sort of image file(s).
#' @param src Whether the source is "local" (i.e. valid file path(s)) or
#' "remote" (i.e. valid URL(s)).
#' @param ... currently not used.
#'
#' @return
#' A \code{list} of HTML strings required to create popup graphs.
#'
#' @seealso \code{\link{popupTable}} and \code{\link{popupGraph}}.
#'
#' @examples
#' \dontrun{
#' ## remote images -----
#' ### one image
#' pt <- data.frame(x = 174.764474, y = -36.877245)
#' coordinates(pt) <- ~ x + y
#' proj4string(pt) <- "+init=epsg:4326"
#'
#' img <- "https://upload.wikimedia.org/wikipedia/commons/thumb/6/62/Mount_Eden.jpg/640px-Mount_Eden.jpg"
#'
#' mapview(pt, popup = popupImage(img, src = "remote"))
#'
#' ### multiple file (types)
#' images <- c(img,
#'             "https://upload.wikimedia.org/wikipedia/commons/1/1b/R_logo.svg",
#'             "https://www.r-project.org/logo/Rlogo.png",
#'             "https://upload.wikimedia.org/wikipedia/commons/d/d6/MeanMonthlyP.gif")
#'
#' pt4 <- data.frame(x = jitter(rep(174.764474, 4), factor = 0.01),
#'                   y = jitter(rep(-36.877245, 4), factor = 0.01))
#' coordinates(pt4) <- ~ x + y
#' proj4string(pt4) <- "+init=epsg:4326"
#'
#' mapview(pt4, popup = popupImage(images, src = "remote")) # NOTE the gif animation
#'
#' ## local images -----
#' img <- system.file("img","Rlogo.png",package="png")
#' mapview(pt, popup = popupImage(img))
#' }
#'
#' @export popupImage
#' @name popupImage
popupImage <- function(img, src = c("local", "remote"), ...) {

  src <- src[1]

  pop <- switch(src,
                local = popupLocalImage(img = img, ...),
                remote = popupRemoteImage(img = img, ...))

  return(pop)

}


### local images -----
popupLocalImage <- function(img, width, height) {
  nm <- basename(img)
  drs <- file.path(tempdir(), "graphs")
  if (!dir.exists(drs)) dir.create(drs)
  fls <- file.path(drs, nm)
  invisible(file.copy(img, file.path(drs, nm)))
  rel_path <- file.path("..", basename(drs), basename(img))

  info <- sapply(img, function(...) rgdal::GDALinfo(..., silent = TRUE))
  yx_ratio <- as.numeric(info["rows", ]) / as.numeric(info["columns", ])
  xy_ratio <- as.numeric(info["columns", ]) / as.numeric(info["rows", ])

  if (missing(height) && missing(width)) {
    width <- 300
    height <- yx_ratio * width
  } else if (missing(height)) height <- yx_ratio * width else
    if (missing(width)) width <- xy_ratio * height

  paste0("<image src='../graphs/",
         basename(img),
         "' width=",
         width,
         " height=",
         height,
         ">")

}


### remote images -----
popupRemoteImage <- function(img, width = 300, height = "100%") {
  paste0("<image src='",
         img,
         "' width=",
         width,
         " height=",
         height,
         ">")
}
