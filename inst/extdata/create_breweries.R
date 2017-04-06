library(mapview)
library(sf)

data(breweries)
tmp <- st_as_sf(breweries91)
head(tmp)
head(breweries)

tmp <- tmp[, !colnames(tmp) %in% "web"]
tmp$number.of.types <- NA
tmp$number.seasonal.beers <- NA

breweries[breweries$brewery %in% tmp$brewery, ]$founded <-
  tmp[tmp$brewery %in% breweries$brewery, ]$founded

breweries
### take all breweries from tmp that are duplicated


tmp <- tmp[!tmp$brewery %in% breweries$brewery, ]
tmp <- tmp[, c(1:6, 8, 9, 7)]

breweries <- rbind(breweries, st_as_sf(tmp))
breweries$zipcode <- as.character(breweries$zipcode)

### replace äöüß!!!
tofix <- c("Ü", "Ä", "Ö", "ä", "ü", "ö", "ß")
fixwith <- c("Ue", "Ae", "Oe", "ae", "ue", "oe", "ss")

fun <- function(x, tofix, fix) gsub(tofix, fix, x)
# tst <- breweries

for (i in seq(tofix)) {
  curtofix <- tofix[i]
  curfix <- fixwith[i]
  for (j in c("brewery", "address", "village")) {
    breweries[[j]] <- fun(breweries[[j]], curtofix, curfix)
  }
}


rownames(breweries) <- as.character(seq(nrow(breweries)))
save(breweries, file = "data/breweries.rda")

