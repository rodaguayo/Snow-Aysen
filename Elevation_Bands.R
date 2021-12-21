rm(list=ls())
cat("\014")  

library("raster")
source('ElevationZones.R')

basins<-shapefile("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/GIS/basin_q.shp")
nasadem_30m<-raster("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/GIS/dem_f.tif")

elev_zones_i<-elevationZones(x=basins[i,], dem=nasadem_30m, max.zones = 8, min.elevZ = 50, elev.thres = 500)
zone_i <- terra::rast(elev_zones_i$zonesRaster)

zone_i <- rbind(terra::as.polygons(zone_i[[1]], values = F),terra::as.polygons(zone_i[[2]], values = F),
                  terra::as.polygons(zone_i[[3]], values = F),terra::as.polygons(zone_i[[4]], values = F),
                  terra::as.polygons(zone_i[[5]], values = F),terra::as.polygons(zone_i[[6]], values = F),
                  terra::as.polygons(zone_i[[7]], values = F),terra::as.polygons(zone_i[[8]], values = F))                 )

plot(zone_i)
terra::writeVector(zone_i, "C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/GIS/Elevation_bands.shp", overwrite=TRUE)
