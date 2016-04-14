### popups (Tim, Flo)

* add function to create popups including option `zcol = ...`
    * defaulting to all columns
    * symbols colored according to `zcol` argument
* add function to create popups using plots
    * see http://stackoverflow.com/questions/32352539/plotting-barchart-in-popup-using-leaflet-library/36237460#36237460
* include `popupOptions(maxWidth = "100%")` in mapviewOptions, see e.g. above example with `width = 400 * 0.01334` 
* implement `...` to be passed to `addPopups()` for each popup function

### spplot (Flo, Tim)

* implement webshot solution

### cubeView (Stefan, Tim)

* rotation issue

### xvecView (Chris, Tim)

* implement attribute colouring and sizing

### projView (Chris, Tim)

* function to create list objects easy

### mapView (Tim)

* add option to save html map to specified folder

### mapviewColors (Tim)

* integrate `viridis` package in `Enhances` so that `inferno` palette is used as default if viridis package is installed 

### miscellaneous (Tim)

* clean `leafletControls` -> `mapviewControls`
* add functionality to burst by factor levels of specific column
