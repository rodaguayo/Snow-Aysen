rm(list=ls())
cat("\014")  

library("raster")
library("readxl")
library("Evapotranspiration")

period <- seq(from = as.POSIXct('2016-01-01', tz="UTC"), to = as.POSIXct("2021-12-10", tz="UTC"), by = "day")
t2m_max_c2<-rast("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/T2M_max_Corrected.nc")
t2m_min_c2<-rast("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/T2M_min_Corrected.nc")
dem_hr<-rast("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/GIS/dem_f.tif")

data("constants")
constants$lat_rad <- -37*3.14/180
constants$Elev <- mean(values(dem_hr))

PET_HS<-rast(t2m_max_c2, props=TRUE)
for (i in 1:nrow(t2m_max_c2)) {
  for (j in 1:ncol(t2m_max_c2)) {
  
  climatedata<-data.frame("Tmax" = as.numeric(t2m_max_c2[i,j]), 
                          "Tmin" = as.numeric(t2m_min_c2[i,j]), 
                          "Month" = as.numeric(format(period, "%m")),
                          "Day" = as.numeric(format(period, "%d")),
                          "Year" = as.numeric(format(period, "%Y")))
  
  climatedata<-ReadInputs(varnames= c("Tmax", "Tmin"), climatedata, constants, stopmissing =c(10,10,3), timestep = "daily", message = "no")
  PET_HS[i,j] <- t(as.data.frame(ET.HargreavesSamani(climatedata, constants, ts="daily", message="no", AdditionalStats="no", save.csv="no")$ET.Daily))
  print(i)
  }
}

time(PET_HS)<-period
PET_HS<-setNames(PET_HS, period)

PET_HS_m <- tapp(PET_HS, strftime(time(PET_HS),format="%Y"), fun = sum, na.rm = TRUE)
PET_HS_m <- mean(PET_HS_m[[-c(1,nlyr(PET_HS_m))]])

writeRaster(PET_HS_m, "C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/PET_mean.tif", overwrite = TRUE)
writeCDF(PET_HS, "C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/PET_Corrected.nc", 
         overwrite=TRUE, varname="pet", unit="mm", longname="Potential Evapotranspiration", zname="time", compression = 9)
