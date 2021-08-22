library(tidygeocoder)
library(sf)

sa1s = st_read("../data/vic_sa1/MB_2016_VIC.shp")
sa1s = sa1s[sa1s$STE_NAME16 == "Victoria",]

address_to_coords = function(address) {
    out = geo(address=address, method="osm")
    c(lon=out$long, lat=out$lat)
}

coords_to_sa1 = function(lat, lon) {
    x = c(lon, lat)
    point = st_sfc(st_point(x), crs=4283)
    idx = st_intersects(point, sa1s)
    sa1s[idx[[1]],]
}

address_to_sa1 = function(address) {
    out = geo(address=address, method="osm")
    x = c(out$long, out$lat)

    point = st_sfc(st_point(x), crs=4283)
    idx = st_intersects(point, sa1s)
    sa1s[idx[[1]],]
}

address_to_sa1("361 Mount Dandenong Tourist Rd, Sassafras")