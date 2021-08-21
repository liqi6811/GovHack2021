library(tidygeocoder)
library(sf)

sa1s = st_read("data/sa1/SA1_2021_AUST_GDA2020.shp")
sa1s = sa1s[sa1s$STE_NAME21 == "Victoria",]

address_to_sa1 = function(address) {
    out = geo(address=address, method="osm")
    x = c(out$long, out$lat)

    point = st_sfc(st_point(x), crs=7844)
    idx = st_intersects(point, sa1s)
    sa1s[idx[[1]],]$SA1_CODE21
}

address_to_sa1("361 Mount Dandenong Tourist Rd, Sassafras")