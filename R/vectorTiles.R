.vector_tile_cache <- new.env(parent = emptyenv())

getBasemapLabels <- function(map.types) {
  if (is.null(map.types)) return(character(0))
  nms <- names(map.types)
  vals <- unname(as.character(map.types))
  if (is.null(nms)) return(vals)
  ifelse(nzchar(nms), nms, vals)
}

isValidVectorTileStyle <- function(style) {
  is.list(style) &&
    !is.null(style$version) &&
    (is.list(style$sources) || is.list(style$layers))
}

fetchVectorTileStyle <- function(url_or_path) {
  if (!is.character(url_or_path) || length(url_or_path) != 1 || !nzchar(url_or_path)) {
    return(NULL)
  }

  is_url <- grepl("^https?://", url_or_path, ignore.case = TRUE)
  if (is_url && exists(url_or_path, envir = .vector_tile_cache, inherits = FALSE)) {
    return(get(url_or_path, envir = .vector_tile_cache, inherits = FALSE))
  }

  expanded_path <- if (!is_url) path.expand(url_or_path) else url_or_path
  if (!is_url && !file.exists(expanded_path)) {
    return(NULL)
  }

  style <- tryCatch(
    suppressWarnings(jsonlite::fromJSON(expanded_path, simplifyVector = FALSE)),
    error = function(e) NULL
  )

  if (isValidVectorTileStyle(style)) {
    if (is_url) {
      assign(url_or_path, url_or_path, envir = .vector_tile_cache)
      return(url_or_path)
    } else {
      return(style)
    }
  }

  NULL
}

resolveVectorTileStyle <- function(provider) {
  if (is.list(provider) && isValidVectorTileStyle(provider)) {
    return(provider)
  }
  if (!is.character(provider) || length(provider) != 1 || !nzchar(provider)) {
    return(NULL)
  }

  fetchVectorTileStyle(provider)
}

addVectorTiles <- function(map,
                           initialStyle = NULL,
                           styles = NULL,
                           pane = "tilePane") {

  if (is.character(initialStyle) && length(initialStyle) > 1) {
    initialStyle <- initialStyle[1]
  }

  style_map <- list()

  if (!is.null(styles)) {
    style_names <- names(styles)
    if (is.null(style_names)) {
      style_names <- as.character(styles)
    }
    for (i in seq_along(styles)) {
      nm <- if (nzchar(style_names[i])) style_names[i] else as.character(styles[[i]])
      resolved <- resolveVectorTileStyle(styles[[i]])
      if (!is.null(resolved)) {
        style_map[[nm]] <- resolved
      }
    }
  }

  if (!is.null(initialStyle) && !(initialStyle %in% names(style_map))) {
    resolved_init <- resolveVectorTileStyle(initialStyle)
    if (!is.null(resolved_init)) {
      style_map[[initialStyle]] <- resolved_init
    } else {
      initialStyle <- NULL
    }
  }

  if (length(style_map) == 0) {
    return(leaflet::addTiles(map))
  }

  if (missing(initialStyle)) {
    initialStyle <- names(style_map)[1]
  }

  deps <- list(
    htmltools::htmlDependency(
      name = "maplibre-gl",
      version = "5.0.0",
      src = c(href = "https://unpkg.com/maplibre-gl@5/dist/"),
      script = "maplibre-gl.js",
      stylesheet = "maplibre-gl.css"
    ),
    htmltools::htmlDependency(
      name = "leaflet-maplibre-gl",
      version = "0.1.4",
      src = c(href = "https://unpkg.com/@maplibre/maplibre-gl-leaflet/"),
      script = "leaflet-maplibre-gl.js"
    )
  )

  map$dependencies <- c(map$dependencies, deps)

  style_map_json <- jsonlite::toJSON(style_map, auto_unbox = TRUE)
  initial_style_json <- if (is.null(initialStyle)) "null" else jsonlite::toJSON(initialStyle, auto_unbox = TRUE)
  pane_json <- jsonlite::toJSON(pane, auto_unbox = TRUE)

  htmlwidgets::onRender(map, sprintf("
    function(el, x) {
      var map = this;
      var pane = %s;
      var incomingStyles = %s;
      map._vectorStyleMaps = map._vectorStyleMaps || {};
      map._vectorStyleMaps[pane] = Object.assign(map._vectorStyleMaps[pane] || {}, incomingStyles);
      if (pane === 'tilePane') {
        map._vectorStyleMap = map._vectorStyleMaps[pane];
      }

      map._vectorTilesInitializedPanes = map._vectorTilesInitializedPanes || {};
      if (map._vectorTilesInitializedPanes[pane]) return;
      map._vectorTilesInitializedPanes[pane] = true;
      if (pane === 'tilePane') {
        map._vectorTilesInitialized = true;
      }

      var styleMap = map._vectorStyleMaps[pane];
      var styleKeys = Object.keys(styleMap);
      if (styleKeys.length === 0) return;

      var initialLayer = %s;
      var hasInitial = initialLayer && Object.prototype.hasOwnProperty.call(styleMap, initialLayer);
      var activeStyle = hasInitial ? styleMap[initialLayer] : styleMap[styleKeys[0]];

      if (typeof L.maplibreGL !== 'function' && typeof L.MaplibreGL !== 'function') {
        console.error('MapLibre GL Leaflet plugin (L.maplibreGL) failed to load.');
        return;
      }

      var createGL = function(opts) {
        return (typeof L.maplibreGL === 'function') ? L.maplibreGL(opts) : new L.MaplibreGL(opts);
      };

      var glLayer = createGL({
        style: activeStyle,
        pane: pane
      });

      if (hasInitial) {
        glLayer.addTo(map);
      }
      map._vectorTileLayers = map._vectorTileLayers || {};
      map._vectorTileLayers[pane] = glLayer;
      if (pane === 'tilePane') {
        map._vectorTileLayer = glLayer;
      }

      if (pane === 'tilePane') {
        map.on('baselayerchange', function(e) {
          var layerName = e.name;
          var currentStyleMap = map._vectorStyleMaps[pane] || {};
          if (Object.prototype.hasOwnProperty.call(currentStyleMap, layerName)) {
            var targetStyle = currentStyleMap[layerName];
            glLayer.options.style = targetStyle;

            if (!map.hasLayer(glLayer)) {
              glLayer.addTo(map);
            }

            var glMap = glLayer.getMaplibreMap ? glLayer.getMaplibreMap() : (glLayer._glMap || null);
            if (glMap && typeof glMap.setStyle === 'function') {
              glMap.setStyle(targetStyle);
            }
          } else {
            if (map.hasLayer(glLayer)) {
              map.removeLayer(glLayer);
            }
          }
        });
      }
    }
  ", pane_json, style_map_json, initial_style_json))
}
