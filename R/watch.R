#' Start and/or stop automagic mapviewing of spatial objects in your workspace.
#'
#' @description
#'   Use these functions to enable automatic viewing of all spatial objects
#'   currently available in \code{env}. \code{mapviewWatcher} uses
#'   \link[later]{later} to set up a watcher function that continuously monitors
#'   \code{env} for spatial objects and refreshes the viewer/browser in case
#'   the list of spatial objects changes.
#'   \cr
#'   \cr
#'   \code{startWatching} and \code{stopWatching} are convenience functions to
#'   start and stop watching, respectively.
#'
#' @details
#'   \code{mapviewWatcher} uses \code{\link{identical}} and hence
#'   will redraw even if e.g. the attributes of a spatial object are changed only
#'   slightly. By default \code{mapviewWatcher} watches the \code{.GlobalEnv} but
#'   this can be changed to another environment. Whether watching is turned on is
#'   controlled by \code{mapviewGetOption("watch")}. In order to enable watching it
#'   needs to be set to \code{mapviewOptions(watch = TRUE)}
#'   (default is \code{FALSE}) and the watcher needs to be initiated by calling
#'   \code{mapviewWatcher()} once. To switch watching off it is sufficient to set
#'   \code{mapviewOptions(watch = FALSE)}.
#'
#' @param env the environment that is being watched (default is \code{.GlobalEnv}).
#' @param ... currently not used.
#'
#' @examples
#'   if (interactive()) {
#'     library(mapview)
#'
#'     ## start the watcher
#'     mapview::startWatching()
#'
#'     ## load some data and watch the automatic visualisation
#'     fran = mapview::franconia
#'     brew = mapview::breweries
#'
#'     ## stop the watcher
#'     mapview::stopWatching()
#'
#'     ## loading or removing things now will not trigger a view update
#'     rm(brew)
#'     trls = mapview::trails
#'
#'     ## re-starting the viewer will re-draw whatever is currently available
#'     mapview::startWatching()
#'
#'     ## watcher can also be stopped via mapviewOptions
#'     mapviewOptions(watch = FALSE)
#'
#'     rm(trls)
#'
#'   }
#'
#' @export
#' @rdname mapviewWatcher
mapviewWatcher = function(env = .GlobalEnv, ...) {

  if (!requireNamespace("later", quietly = TRUE)) {
    stop(
      "Please install.packages('later') to allow mapview to watch your workspace"
      , call. = FALSE
    )
  }

  last_value = NULL

  dir <- tempfile()
  dir.create(dir)
  htmlFile <- file.path(dir, "index.html")

  mv_watch = function() {
    if (!mapviewGetOption("watch")) return(invisible())
    spatdat_lst = getSpatialData(env = env)
    if (length(spatdat_lst) > 0 && !identical(spatdat_lst, last_value)) {
      m = mapview::mapView(spatdat_lst)
      mapview::mapshot(m, htmlFile, selfcontained = FALSE)

      # viewer <- getOption("viewer")
      # if (!is.null(viewer)) {
      #   viewer(htmlFile)
      # } else {
      #   utils::browseURL(htmlFile)
      # }
      last_value <<- spatdat_lst
    }

    later::later(mv_watch, 0.25)
  }

  ## initiate the watcher
  mv_watch()
  return(htmlFile)
}

#' @export
#' @describeIn mapviewWatcher start watching
startWatching = function(env = .GlobalEnv, ...) {
  mapviewOptions(watch = TRUE)
  fl = mapviewWatcher(env = env)
  if (!file.exists(fl)) {
    mapshot(mapview() + "egg", url = fl)
  }
  towatch = dirname(fl)
  servr::httw(
    dir = towatch
    , pattern = ".html"
    , verbose = FALSE
  )
}

#' @export
#' @describeIn mapviewWatcher stop watching
stopWatching = function(env = .GlobalEnv, ...) {
  mapviewOptions(watch = FALSE)
}


## helper
getSpatialData = function(env = .GlobalEnv) {
  dat = ls(envir = env, sorted = FALSE)
  cls = lapply(lapply(dat, get), class)

  ## sf
  sf_idx = grep("sf", cls)
  sf_dat = as.list(dat[sf_idx])
  names(sf_dat) = dat[sf_idx]

  ## sp
  sp_idx = grep("Spatial", cls)
  sp_dat = as.list(dat[sp_idx])
  names(sp_dat) = dat[sp_idx]

  # stars
  st_idx = grep("stars", cls)
  st_dat = as.list(dat[st_idx])
  names(st_dat) = dat[st_idx]

  ## raster - doesn't work currently! fix by converting to stars internally
  # rs_idx = grep("Raster", cls)
  # rs_dat = as.list(dat[rs_idx])
  # names(rs_dat) = dat[rs_idx]

  ## combine and get
  spatdat = Filter(Negate(is.null), c(sf_dat, sp_dat, st_dat)) #, rs_dat))

  lapply(spatdat, get)
}

