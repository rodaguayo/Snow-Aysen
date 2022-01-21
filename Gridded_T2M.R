rm(list=ls())
cat("\014")  

library("hydroGOF")
library("terra")
library("zoo")
library("sf")

period  <- seq(from = as.Date("2016-01-01"), to = as.Date("2021-12-10"), by = "day")
sample <- terra::rast(matrix(rnorm(40*40),40,40), crs = "+init=epsg:4326")
ext(sample) <- c(-72.03,-71.98,-45.65, -45.60)

#Gridded products
t2m_max_era5<-rast("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/ERA5_data.nc",  subds="t2m")
t2m_max_era5<-crop(t2m_max_era5, ext(c(-72.5,-71.5,-46, -45)))-273.15
t2m_max_era5<-tapp(t2m_max_era5, strftime(time(t2m_max_era5),format="%Y-%m-%d"), fun = max, na.rm = TRUE)
t2m_max_era5<-resample(t2m_max_era5, sample, method = "bilinear")[[-c(1)]]
terra::time(t2m_max_era5) <- seq(from = as.POSIXct("2016-01-01", tz="UTC"), to = as.POSIXct("2021-12-10", tz="UTC"), by = "day")

t2m_min_era5<-rast("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/ERA5_data.nc",  subds="t2m")
t2m_min_era5<-crop(t2m_min_era5, ext(c(-72.5,-71.5,-46, -45)))-273.15
t2m_min_era5<-tapp(t2m_min_era5, strftime(time(t2m_min_era5),format="%Y-%m-%d"), fun = min, na.rm = TRUE)
t2m_min_era5<-resample(t2m_min_era5, sample, method = "bilinear")[[-c(1)]]
terra::time(t2m_min_era5) <- seq(from = as.POSIXct("2016-01-01", tz="UTC"), to = as.POSIXct("2021-12-10", tz="UTC"), by = "day")

#Downscaling
dem_hr<-project(rast("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/GIS/dem_f.tif"), sample, method="bilinear")
dem_lr<-project(rast("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/GIS/dem_f.tif"), rast("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/ERA5_data.nc"), method="bilinear")
dem_lr<-resample(dem_lr, dem_hr, method="bilinear")

#Try a better lapse rate (for Tmax and Tmin)
t2m_max_era5<-t2m_max_era5+(dem_lr-dem_hr)*0.0065
t2m_min_era5<-t2m_min_era5+(dem_lr-dem_hr)*0.0040
t2m_max_era5<-round(t2m_max_era5, 2)
t2m_min_era5<-round(t2m_min_era5, 2)

#Observations for bias correction:  Mean and variance scaling 
t2m_shape <-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/T2M_metadata.csv")
t2m_shape <-vect(t2m_shape, geom=c("longitude", "latitude"), crs = "+init=epsg:4326")
t2m_max  <-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/T2Mmax_data_d.csv")
t2m_min  <-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/T2Mmin_data_d.csv")
t2m_max$date<-as.Date(t2m_max$date, format = "%Y-%m-%d")
t2m_min$date<-as.Date(t2m_min$date, format = "%Y-%m-%d")
t2m_max<-subset(t2m_max, date >= min(period) & date <= max(period))
t2m_min<-subset(t2m_min, date >= min(period) & date <= max(period))

t2m_max_data_mod <-as.data.frame(t(extract(t2m_max_era5, t2m_shape)))
t2m_min_data_mod <-as.data.frame(t(extract(t2m_min_era5, t2m_shape)))
colnames(t2m_max_data_mod)<-as.data.frame(t2m_shape)$name
colnames(t2m_min_data_mod)<-as.data.frame(t2m_shape)$name
t2m_max_data_mod <- t2m_max_data_mod[-1, ]
t2m_min_data_mod <- t2m_min_data_mod[-1, ]
t2m_max$date <-NULL
t2m_min$date <-NULL

KGE(sim=t2m_max_data_mod, obs=t2m_max, method="2009", out.type="full",na.rm=TRUE)
KGE(sim=t2m_min_data_mod, obs=t2m_min, method="2009", out.type="full",na.rm=TRUE)

me_tmax<-me(sim=t2m_max_data_mod[,5], obs=t2m_max[,5], method="2009", out.type="full",na.rm=TRUE)
me_tmin<-me(sim=t2m_min_data_mod[,5], obs=t2m_min[,5], method="2009", out.type="full",na.rm=TRUE)
rSD_tmax<-1/rSD(sim=t2m_max_data_mod[,5]-me_tmax, obs=t2m_max[,5], na.rm=TRUE)
rSD_tmin<-1/rSD(sim=t2m_min_data_mod[,5]-me_tmin, obs=t2m_min[,5], na.rm=TRUE)

t2m_max_c1<-t2m_max_era5 - me_tmax
t2m_max_c2<-t2m_max_c1-mean(t2m_max_c1, na.rm =TRUE)
t2m_max_c2<-t2m_max_c2*rSD_tmax
t2m_max_c2<-t2m_max_c2 + mean(t2m_max_c1, na.rm =TRUE)

t2m_min_c1<-t2m_min_era5 - me_tmin
t2m_min_c2<-t2m_min_c1-mean(t2m_min_c1, na.rm =TRUE)
t2m_min_c2<-t2m_min_c2*rSD_tmin
t2m_min_c2<-t2m_min_c2 + mean(t2m_min_c1, na.rm =TRUE)

t2m_max_c2_m <- tapp(t2m_max_c2, strftime(time(t2m_max_c2),format="%Y"), fun = mean)
t2m_max_c2_m <- mean(t2m_max_c2_m[[-c(1,nlyr(t2m_max_c2_m))]])
t2m_min_c2_m <- tapp(t2m_min_c2, strftime(time(t2m_min_c2),format="%Y"), fun = mean)
t2m_min_c2_m <- mean(t2m_min_c2_m[[-c(1,nlyr(t2m_min_c2_m))]])
t2m_mean<-mean(c(t2m_max_c2_m, t2m_min_c2_m))

writeRaster(t2m_mean, "C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/T2M_Corrected_mean.tif", overwrite = TRUE)

writeCDF(t2m_max_c2, "C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/T2M_max_Corrected.nc", overwrite=TRUE, varname="t2m_max", unit="degC", 
         longname="Maximum temperature", zname="time", compression = 9)

writeCDF(t2m_min_c2, "C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/T2M_min_Corrected.nc", overwrite=TRUE, varname="t2m_min", unit="degC", 
         longname="Minimum temperature", zname="time", compression = 9)
