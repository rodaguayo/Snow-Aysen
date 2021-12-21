rm(list=ls())
cat("\014")  

library("hydroGOF")
library("terra")
library("qmap")
library("zoo")
library("sf")

## Spatial extent and period 
period  <- seq(from = as.Date("2016-01-01"), to = as.Date("2021-12-10"), by = "day")
sample <- terra::rast(matrix(rnorm(40*40),40,40), crs = "+init=epsg:4326")
ext(sample) <- c(-72.03,-71.98,-45.65, -45.60)

## ERA5 precipitation
pp_era5<-rast("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/ERA5_data.nc",  subds="tp")
pp_era5<-crop(pp_era5, ext(c(-72.5,-71.5,-46, -45)))*1000
pp_era5<-tapp(pp_era5, strftime(time(pp_era5),format="%Y-%m-%d"), fun = sum, na.rm = TRUE)[[-c(1)]]
time(pp_era5) <- period

#Observations for bias correction
pp_data <-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/PP_data_d.csv")
pp_shape<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/PP_metadata.csv")
pp_shape<-vect(pp_shape, geom=c("longitude", "latitude"), crs = "+init=epsg:4326")

pp_data$date<-as.Date(pp_data$date, format = "%d-%m-%y")
pp_data<-subset(pp_data, date >= min(period) & date <= max(period))
pp_data_mod <-as.data.frame(t(extract(disagg(pp_era5, 4, method = "bilinear"), pp_shape)))
colnames(pp_data_mod)<-as.data.frame(pp_shape)$name
pp_data_mod <- pp_data_mod[-1, ]
pp_data$date <-NULL

KGE(pp_data_mod$Teniente_Vidal_Coyhaique, pp_data$Teniente_Vidal_Coyhaique, method="2012", out.type = "full")

## Downscaling: Bilinear resampling
pp_era5<-resample(pp_era5, sample, method = "bilinear")

## Bias correction
qmap_power<-fitQmapPTF(pp_data$Teniente_Vidal_Coyhaique, pp_data_mod$Teniente_Vidal_Coyhaique, 
                       transfun="power", wet.day=TRUE, cost="RSS")

pp_era5c<-rast(pp_era5, props=TRUE)
for (i in 1:nrow(pp_era5)) {
  for (j in 1:ncol(pp_era5)) {
    pp_era5c[i,j] <- t(as.data.frame(doQmap(as.numeric(pp_era5[i,j]),qmap_power )))

  }
  print(i)
}

time(pp_era5c)<-period
pp_era5c_m <- mean(tapp(pp_era5c, strftime(time(pp_era5c),format="%Y"), fun = sum, na.rm = TRUE))

writeRaster(pp_era5c_m, "C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/PP_Corrected_mean.tif", overwrite = TRUE)
writeCDF(pp_era5c, "C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/PP_Corrected.nc", 
         overwrite=TRUE, varname="pp", unit="mm", longname="Precipitation", zname="time", compression = 9)
