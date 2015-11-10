#library(mapview)
#slideView2internal(list(a="a",b="b"))

library(mapview)
### raster data ###
library(sp)
library(raster)

data(poppendorf)

stck1 <- subset(poppendorf, c(3, 4, 5))
stck2 <- subset(poppendorf, c(2, 3, 4))

slideView3(stck1, stck2)


#slideView2(stck1, stck2)

#slideView(stck1, stck2)
