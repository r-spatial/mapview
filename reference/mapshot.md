# Save mapview or leaflet map as HTML and/or image using webshot

Save a mapview or leaflet map as `.html` index file or `.png`, `.pdf`,
or `.jpeg` image.

## Usage

``` r
mapshot(
  x,
  url = NULL,
  file = NULL,
  remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar",
    "drawToolbar", "easyButton"),
  ...
)

mapshot2(
  x,
  url = NULL,
  file = NULL,
  remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar",
    "drawToolbar", "easyButton", "control"),
  ...
)
```

## Arguments

- x:

  `mapview` or `leaflet` object (or any other hmtlwidget).

- url:

  Output `.html` file. If not supplied and 'file' is specified, a
  temporary index file will be created.

- file:

  Output `.png`, `.pdf`, or `.jpeg` file.

- remove_controls:

  `character` vector of control buttons to be removed from the map when
  saving to file. Any combination of `"zoomControl"`, `"layersControl"`,
  `"homeButton"`, `"scaleBar"`, `"drawToolbar"`, `"easyButton"`. If set
  to `NULL` nothing will be removed. Ignored if `x` is not a mapview or
  leaflet map.

- ...:

  Further arguments passed on to
  [`saveWidget`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html)
  and/or
  [`webshot`](https://rstudio.github.io/webshot2/reference/webshot.html).

## Details

mapshot uses
[`webshot`](http://wch.github.io/webshot/reference/webshot.md) from the
webshot package. mapshot2 uses
[`webshot`](https://rstudio.github.io/webshot2/reference/webshot.html)
from the webshot2 package.

mapshot can be used to save both leaflet and mapview maps as html or png
files or both. In theory, it should also work for any and all other
htmlwidgets but has not been tested extensively for other htmlwidgets.

In case you want to save larger maps mapshot is likely to fail. You can
try setting `selfcontained = FALSE` to avoid errors and create a valid
local html file.

mapshot2 uses
[`saveWidget`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html) and
[`webshot`](https://rstudio.github.io/webshot2/reference/webshot.html)
to save maps as `.html` and/or `.png|.jpg` files, respectively.
[`webshot`](https://rstudio.github.io/webshot2/reference/webshot.html)
assumes a findable installation of some Chrome browser variant on your
system. If you see the the following error:

``  `google-chrome` and `chromium-browser` were not found. Try setting the CHROMOTE_CHROME environment variable or adding one of these executables to your PATH.  ``

it means that
[`find_chrome`](https://rstudio.github.io/chromote/reference/find_chrome.html)
cannot find a Chrome based browser in your system. Please see
<https://github.com/rstudio/chromote#specifying-which-browser-to-use>
for more details.

## Functions

- `mapshot()`: Save mapview or leaflet map as HTML and/or image using
  webshot

- `mapshot2()`: Save mapview or leaflet map as HTML and/or image using
  webshot2

## See also

[`webshot`](http://wch.github.io/webshot/reference/webshot.md),
[`saveWidget`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html).

[`webshot`](https://rstudio.github.io/webshot2/reference/webshot.html).

## Examples

``` r
if (FALSE) { # \dontrun{
  library(utils)

  m = mapview(breweries)
  html_fl = tempfile(fileext = ".html")
  png_fl = tempfile(fileext = ".png")

  ## create standalone .html
  mapshot(m, url = html_fl)
  browseURL(html_fl)

  ## create standalone .png; temporary .html is removed automatically unless
  ## 'remove_url = FALSE' is specified
  mapshot(m, file = png_fl)
  browseURL(png_fl)
  mapshot(m, file = png_fl,
          remove_controls = c("homeButton", "layersControl"))
  browseURL(png_fl)

  ## create .html and .png
  mapshot(m, url = html_fl, file = png_fl)
  browseURL(png_fl)
  browseURL(html_fl)
} # }

if (FALSE) { # \dontrun{
  library(utils)

  m = mapview(breweries)
  html_fl = tempfile(fileext = ".html")
  png_fl = tempfile(fileext = ".png")

  ## create standalone .html
  mapshot2(m, url = html_fl)
  browseURL(html_fl)

  ## create standalone .png; temporary .html is removed automatically unless
  ## 'remove_url = FALSE' is specified
  mapshot2(m, file = png_fl)
  browseURL(png_fl)
  mapshot2(m, file = png_fl,
           remove_controls = c("homeButton", "layersControl"))
  browseURL(png_fl)

  ## create .html and .png
  mapshot2(m, url = html_fl, file = png_fl)
  browseURL(png_fl)
  browseURL(html_fl)
} # }
```
