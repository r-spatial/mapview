# Start and/or stop automagic mapviewing of spatial objects in your workspace.

Use these functions to enable automatic viewing of all spatial objects
currently available in `env`. `mapviewWatcher` uses
[later](https://later.r-lib.org/reference/later.html) to set up a
watcher function that continuously monitors `env` for spatial objects
and refreshes the viewer/browser in case the list of spatial objects
changes.  
  
`startWatching` and `stopWatching` are convenience functions to start
and stop watching, respectively.

## Usage

``` r
mapviewWatcher(env = .GlobalEnv, ...)

startWatching(env = .GlobalEnv, ...)

stopWatching(env = .GlobalEnv, ...)
```

## Arguments

- env:

  the environment that is being watched (default is `.GlobalEnv`).

- ...:

  currently not used.

## Details

`mapviewWatcher` uses
[`identical`](https://rdrr.io/r/base/identical.html) and hence will
redraw even if e.g. the attributes of a spatial object are changed only
slightly. By default `mapviewWatcher` watches the `.GlobalEnv` but this
can be changed to another environment. Whether watching is turned on is
controlled by `mapviewGetOption("watch")`. In order to enable watching
it needs to be set to `mapviewOptions(watch = TRUE)` (default is
`FALSE`) and the watcher needs to be initiated by calling
`mapviewWatcher()` once. To switch watching off it is sufficient to set
`mapviewOptions(watch = FALSE)`.

## Functions

- `startWatching()`: start watching

- `stopWatching()`: stop watching

## Examples

``` r
  if (interactive()) {
    library(mapview)

    ## start the watcher
    mapview::startWatching()

    ## load some data and watch the automatic visualisation
    fran = mapview::franconia
    brew = mapview::breweries

    ## stop the watcher
    mapview::stopWatching()

    ## loading or removing things now will not trigger a view update
    rm(brew)
    trls = mapview::trails

    ## re-starting the viewer will re-draw whatever is currently available
    mapview::startWatching()

    ## watcher can also be stopped via mapviewOptions
    mapviewOptions(watch = FALSE)

    rm(trls)

  }
```
