library(stringr)
library(sp)
library(rgdal)
library(prevR)
library(tigris)
library(sf)
library(rgdal)
library(geosphere)

setwd("/Users/yroell/Documents/Classes/tdi/capstone/")
df = read.csv("alltrails.csv")
contact = read.csv("emergency.csv")

setwd("/Users/yroell/Documents/Classes/tdi/capstone/alltrails/")
coverage = readOGR(dsn = ".", layer = "coverage_hand")
river = readOGR(dsn = ".", layer = "river")

setwd("/Users/yroell/Documents/Classes/tdi/capstone/gpx/")
all_trails = list.files(pattern=".csv$")

c = counties(state = "Colorado")

gpx_trail_names = c()
for (i in df$websites) {
  j = str_match(i, "colorado/(.*)\\?")[1,2]
  k = str_to_title(str_replace_all(j, "-", " "))
  l = paste0(k,".csv")
  gpx_trail_names = c(gpx_trail_names, l)
}

gpx_list = list()
for (i in gpx_trail_names) {
    j = read.csv(i)
    j$color = point.in.SpatialPolygons(j$Longitude, j$Latitude, coverage)
    j[j == TRUE] = "blue"
    j[j == FALSE] = "orange"
    j$color[1] = "green"
    j$color[length(j$color)] = "green"
    j$rad = 2
    j$rad[1] = 10
    j$rad[length(j$rad)] = 5
    j$river = any(point.in.SpatialPolygons(j$Longitude, j$Latitude, river))
    ll = data.frame(long = j$Longitude[1], lat = j$Latitude[1])
    DT_sf = st_as_sf(ll, coords = c("long", "lat"), 
                     crs = 4269)
    j$county = st_intersection(DT_sf, c)["NAME"]$NAME
    j$phone = contact[contact$County == j$county[1], "Phone"]
    j$name = paste(contact[contact$County == j$county[1], "FName"], contact[contact$County == j$county[1], "LName"])
    for (k in 1:length(j$Latitude)){
      j[k,10] = distHaversine(c(j$Longitude[k], j$Latitude[k]), c(j$Longitude[k+1], j$Latitude[k+1]))
      j[k,11] = ((abs(j[k+1, 3] - j[k, 3])) / (j[k,10])) * 100
      j[k,12] = (j[k,10] + sum(j[1:k,10])) / 1000
    }
    colnames(j) = c(colnames(j)[1:9], "distance_waypoints", "slope", "distance_total")
    gpx_list[[i]] = j

}

setwd("/Users/yroell/Documents/Classes/tdi/capstone/alltrails/best_trail/")
save(gpx_trail_names, gpx_list, file = "gpxs.RData")
