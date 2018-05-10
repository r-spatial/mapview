### leaflet w missing =====================================================

leafletMissing = function(map.types, ...) {

  lst = list(...)
  if (length(lst) == 0)
    easter.egg = FALSE else
      if (lst$easter.egg) easter.egg = TRUE

      if(easter.egg) {

        tilesets = list(
          ts = c(
            'http://{s}.tiles.mapbox.com/v3/gvenech.m13knc8e/{z}/{x}/{y}.png'
            # , 'http://{s}.tile.thunderforest.com/pioneer/{z}/{x}/{y}.png'
            # , 'http://a.gps-tile.openstreetmap.org/lines/{z}/{x}/{y}.png'
            # , 'http://a.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png'
            # , 'http://a.tiles.wmflabs.org/hikebike/{z}/{x}/{y}.png'
          ),
          attr = c(
            '&copy; <a href="https://www.mapbox.com/map-feedback/">Mapbox</a> &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> <strong><a href="https://www.mapbox.com/map-feedback/" target="_blank">Improve this map</a></strong>'
            # , '&copy; <a href="http://www.thunderforest.com/">Thunderforest</a>, &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
            # , '<a href="https://openstreetmap.org/"> &copy; OpenStreetMap contributors, CC-BY-SA</a>'
            # , '<a href="http://hot.openstreetmap.org/"> &copy; OpenStreetMap contributors, tiles courtesy of Humanitarian OpenStreetMap Team</a>'
            # , '<a href="http://hikebikemap.org/"> &copy; OpenStreetMap contributors, CC-BY-SA</a>'
          )
        )

        ind = sample(length(tilesets$ts), 1L)
        ts = tilesets$ts[ind]
        attr = tilesets$attr[ind]

        envinMR = data.frame(x = 8.771676,
                             y = 50.814891,
                             envinMR = "envinMR")
        sp::coordinates(envinMR) = ~x+y
        sp::proj4string(envinMR) = sp::CRS(llcrs)
        m = leaflet() %>%
          addTiles(urlTemplate = ts,
                   attribution = attr,
                   options = tileOptions(minZoom = 1, maxZoom = 18))

        fl = 'http://cdn.makeagif.com/media/8-11-2015/n2JwUG.gif'

        cit = unclass(utils::citation("mapview", auto = NULL))[[1]]
        cit = attr(cit, "textVersion")
        cit = paste(strsplit(cit, "\\. ")[[1]][1:3], collapse = ". ")

        pop = paste("<center>", "<b>",
                    '<a target="_blank" href="http://environmentalinformatics-marburg.github.io/mapview/introduction.html">mapview</a>',
                    "</b>", "<br>",
                    " was created at",
                    "<br>",
                    '<a target="_blank" href="http://environmentalinformatics-marburg.de/">Environmental Informatics Marburg</a>',
                    "<br>", "by ", "<br>",
                    '<a target="_blank" href="https://github.com/tim-salabim">Tim Appelhans</a>',
                    "<br>", "<br>",
                    '<hr width=50% style="border: none; height: 1px; color: #D8D8D8; background: #D8D8D8;"/>',
                    "<br>",
                    "Please cite as: ", "<br>",
                    cit,
                    "<br>", "<br>",
                    '<hr width=50% style="border: none; height: 1px; color: #D8D8D8; background: #D8D8D8;"/>',
                    "<br>",
                    "<b>", "mapview", "</b>", "is for quick visualisation of spatial data",
                    "<br>", "<br>",
                    paste('<img src =', fl, 'width="95%">'),
                    '<a target="_blank" href="http://makeagif.com/n2JwUG">Source: MakeAGIF.com</a>',
                    "</center>")
        m = leaflet::addCircleMarkers(data = envinMR, map = m,
                                      fillColor = "cyan",
                                      color = "black",
                                      radius = 15,
                                      weight = 4,
                                      opacity = 0.8,
                                      fillOpacity = 0.8,
                                      group = "envinMR",
                                      popup = pop)
        m = leaflet::addPopups(map = m,
                               lng = 8.771676,
                               lat = 50.814891,
                               popup = pop,
                               options = popupOptions(closeOnClick = TRUE))
        m = leaflet::addLayersControl(map = m,
                                      position = "topleft",
                                      overlayGroups = "envinMR")
        m = leaflet::setView(map = m, 8.771676, 50.814891, zoom = 4)
        if (isAvailableInLeaflet()$scl) m = leaflet::addScaleBar(map = m, position = "bottomleft")
        m = addMouseCoordinates(m) %>% addHomeButton(extent(envinMR),
                                                     "mapview home")
        out = new('mapview', object = list(NULL), map = m)
      } else {
        m = initBaseMaps(map.types)
        m = leaflet::setView(map = m, 8.770862, 50.814772, zoom = 18)
        m = leaflet::addLayersControl(map = m,
                                      baseGroups = map.types,
                                      position = mapviewGetOption(
                                        "layers.control.pos"))
        if (isAvailableInLeaflet()$scl) m = leaflet::addScaleBar(map = m, position = "bottomleft")
        m = addMouseCoordinates(m)
        out = new('mapview', object = list(NULL), map = m)
      }
      return(out)

}
