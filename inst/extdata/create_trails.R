library(sf)
library(mapview)

trails = st_read("/home/ede/software/data/Wanderwege_shape/Wanderweg_gesamt.shp")

# mapview(trails[1:500, ], zcol = "FTY")

tst = st_transform(subset(trails, trails$FTY == "Wanderweg"), crs = 4326)
# tst = st_transform(trails, crs = 4326)

# mapview(tst)

tst2 = st_intersection(tst, franconia)
trails = st_cast(tst2[, c(1, 2, 3, 11)])

# mapview(trails) + breweries

brewtst = st_transform(breweries, crs = 32632)
trailtst = st_transform(trails, crs = 32632)

bufftst = st_buffer(brewtst, dist = 3000)

indx = st_intersects(trailtst, bufftst)

subs = trailtst[lengths(indx) > 0, ]
subs = subs[, c(1, 2, 4, 5)]

mapview(subs)

subssim = st_simplify(subs, preserveTopology = TRUE, dTolerance = 15) %>% st_cast()
mapview(subssim)

trails = subssim

### replace äöüß!!!
tofix <- c("Ü", "Ä", "Ö", "ä", "ü", "ö", "ß")
fixwith <- c("Ue", "Ae", "Oe", "ae", "ue", "oe", "ss")

fun <- function(x, tofix, fix) gsub(tofix, fix, x)
# tst <- breweries

for (i in seq(tofix)) {
  curtofix <- tofix[i]
  curfix <- fixwith[i]
  for (j in c(1, 3)) {
    trails[[j]] <- fun(trails[[j]], curtofix, curfix)
  }
}


rownames(trails) <- as.character(seq(nrow(trails)))

save(trails, file = "/home/ede/software/Rpckgdev/mapview/data/trails.rda")
