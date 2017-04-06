library(sf)
library(mapview)
library(foreign)

tst <- read_sf("software/data/NUTS_2013_01M_SH/data/NUTS_RG_01M_2013.shp")
db <- read.dbf("software/data/NUTS_2013_01M_SH/data/NUTS_AT_2013.dbf")

tst_db <- merge(tst, db, by = "NUTS_ID")
tst_db5 <- tst_db[nchar(as.character(tst_db$NUTS_ID)) == 5, ]

mapview(tst_db5)

tst_fr <- subset(tst_db5, substr(tst_db5$NUTS_ID, 1, 4) %in% c("DE24", "DE25", "DE26"))

mapview(tst_fr)

franconia <- st_transform(tst_fr, crs = 4326)
franconia <- franconia[, c("NUTS_ID", "SHAPE_AREA", "SHAPE_LEN", "CNTR_CODE", "NAME_ASCI")]

mapview(franconia)

lst <- split(franconia, substr(franconia$NUTS_ID, 1, 4))
dists <- c("Oberfranken", "Mittelfranken", "Unterfranken")

franconia <- do.call(rbind, lapply(seq(lst), function(i) {
  lst[[i]]$district <- dists[i]
  return(lst[[i]])
}))

mapview(franconia)

str(franconia)
franconia$CNTR_CODE <- droplevels(franconia$CNTR_CODE)
franconia$NAME_ASCI <- droplevels(franconia$NAME_ASCI)

rownames(franconia) <- as.character(seq(nrow(franconia)))

save(franconia, file = "/home/ede/software/Rpckgdev/mapview/data/franconia.rda")
