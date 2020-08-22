library(mapview)

### isAvailableInLeaflet() -----
avl = mapview:::isAvailableInLeaflet()

expect_true(avl$lab)
expect_true(avl$scl)


### mapview2leaflet -----
m = mapview(breweries)
expect_true(inherits(m, "mapview"))

l = mapview:::mapview2leaflet(m)
expect_true(inherits(l, c("leaflet", "htmlwidget")))


### getSimpleClass -----
library(plainview)

expect_equal(mapview:::getSimpleClass(leaflet::breweries91), "vec") # sf
expect_equal(mapview:::getSimpleClass(plainview::poppendorf), "rst") # raster
expect_equal(mapview:::getSimpleClass(franconia), "vec") # sp

### makeLabels -----
expect_equal(mapview:::makeLabels(sf::st_geometry(breweries)[[1]]), "1")
expect_equal(mapview:::makeLabels(head(sf::st_geometry(breweries))),
             as.character(1:6))
expect_equal(mapview:::makeLabels(franconia), rownames(franconia))
expect_equal(mapview:::makeLabels(franconia, zcol = "district"),
             franconia[["district"]])

### makeLabelsSP -----
expect_equal(length(mapview:::makeLabelsSP(leaflet::breweries91$brewery)),
              nrow(leaflet::breweries91@data))
expect_true(inherits(mapview:::makeLabelsSP(leaflet::breweries91$founded), "character"))

### getFeatureIds -----
expect_equal(mapview:::getFeatureIds(franconia), row.names(franconia))
expect_equal(mapview:::getFeatureIds(leaflet::gadmCHE), row.names(leaflet::gadmCHE))
expect_equal(mapview:::getFeatureIds(sf::st_geometry(breweries)),
             1:length(sf::st_geometry(breweries)))

### createExtent -----
expect_true(inherits(mapview:::createExtent(breweries), "Extent"))

### isMultiFeature -----
expect_equal(mapview:::isMultiFeature(sf::st_geometry(franconia)[[1]]), TRUE)
expect_equal(mapview:::isMultiFeature(sf::st_geometry(breweries)[[1]]), FALSE)
expect_false(mapview:::isMultiFeature("Test"))

### getSFClass -----
expect_equal(mapview:::getSFClass(breweries), "sf")
expect_equal(mapview:::getSFClass(sf::st_geometry(franconia)), "sfc_MULTIPOLYGON")
expect_equal(mapview:::getSFClass(sf::st_geometry(breweries)[[1]]), "POINT")

### getGeometryType -----
expect_true(inherits(mapview:::getGeometryType(franconia), "character"))
expect_equal(mapview:::getGeometryType(breweries), "pt")
expect_equal(mapview:::getGeometryType(sf::st_geometry(breweries)), "pt")
expect_equal(mapview:::getGeometryType(trails), "ln")
expect_equal(mapview:::getGeometryType(franconia), "pl")

### lineWidth -----
expect_equal(typeof(mapview:::lineWidth(franconia)), "double")
expect_equal(mapview:::lineWidth(trails), 2)
expect_equal(mapview:::lineWidth(franconia), 0.5)

### regionOpacity -----
expect_equal(typeof(mapview:::regionOpacity(franconia)), "double")
expect_equal(mapview:::regionOpacity(trails), 1)
expect_equal(mapview:::regionOpacity(franconia), 0.6)

### basemaps -----
expect_equal(typeof(mapview:::basemaps("#0000ff")), "character")
expect_equal(mapview:::basemaps("black")[1], "CartoDB.Positron")
expect_equal(mapview:::basemaps("white")[1], "CartoDB.DarkMatter")

### getProjection -----
expect_equal(typeof(mapview:::getProjection(franconia)), "character")

### createFileId -----
expect_equal(typeof(mapview:::createFileId()), "character")
expect_equal(nchar(mapview:::createFileId(8)), 8)

### extendLimits -----
expect_equal(length(mapview:::extendLimits(c(1, 10))), 2)
expect_equal(mapview:::extendLimits(c(1, 10)), c(0.37, 10.63))
expect_equal(typeof(mapview:::extendLimits(c(1, 10))), "double")

### circleRadius -----
expect_equal(length(mapview:::circleRadius(breweries)), 1)
expect_equal(length(mapview:::circleRadius(breweries, "founded")), nrow(breweries))
expect_equal(typeof(mapview:::circleRadius(breweries, "founded")), "double")
expect_equal(max(mapview:::circleRadius(breweries, "founded", max.rad = 22)), 22)
expect_equal(min(mapview:::circleRadius(breweries, "founded", min.rad = 2)), 2) # NAs will have radius 2!!

### extentOverlap -----
expect_true(suppressMessages(mapview:::extentOverlap(breweries, franconia)))
expect_false(suppressMessages(mapview:::extentOverlap(franconia[1, ], franconia[2, ])))

### makeLayerName -----
expect_equal(typeof(mapview:::makeLayerName(franconia)), "character")

### makeListLayerNames -----
expect_equal(typeof(mapview:::makeListLayerNames(list(franconia, breweries), "test")), "list")
expect_equal(mapview:::makeListLayerNames(list(f = franconia, b = breweries), "test"),
             list("f", "b"))

### paneName -----
expect_equal(mapview:::paneName(breweries), "point")

### zIndex -----
expect_equal(mapview:::zIndex(franconia), 420)

### useCanvas -----
expect_false(mapview:::useCanvas(breweries))

### is_literally_false ----
expect_true(mapview:::is_literally_false(FALSE))
tst = NULL
expect_false(mapview:::is_literally_false(tst))
