rm(list=ls())
cat("\014")  

library("terra")
library("hydroGOF")

## Spatial extent and period 
period  <- seq(from = as.Date("2016-01-01"), to = as.Date("2021-12-10"), by = "day")


## ERA5 precipitation and temperature
pp_era5<-rast("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/ERA5_data.nc",  subds="tp")
pp_era5<-crop(pp_era5, ext(c(-72.5,-71.5,-46, -45)))*1000
pp_era5<-tapp(pp_era5, strftime(time(pp_era5),format="%Y-%m-%d"), fun = sum, na.rm = TRUE)[[-c(1)]]
pp_era5<-disagg(pp_era5, 4, method = "bilinear")

t2m_era5<-rast("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/ERA5_data.nc",  subds="t2m")
t2m_era5<-crop(t2m_era5, ext(c(-72.5,-71.5,-46, -45)))-273.15
t2m_era5<-tapp(t2m_era5, strftime(time(t2m_era5),format="%Y-%m-%d"), fun = mean, na.rm = TRUE)[[-c(1)]]
t2m_era5<-disagg(t2m_era5, 4, method = "bilinear")


#Observations for comparison 
pp_shape<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/PP_metadata.csv")
pp_shape<-vect(pp_shape, geom=c("longitude", "latitude"), crs = "+init=epsg:4326")
pp_data <-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/PP_data_d.csv")
pp_data$date<-as.Date(pp_data$date, format = "%Y-%m-%d")
pp_data<-subset(pp_data, date >= min(period) & date <= max(period))

t2m_shape <-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/T2M_metadata.csv")
t2m_shape <-vect(t2m_shape, geom=c("longitude", "latitude"), crs = "+init=epsg:4326")
t2m_data <-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/T2Mmax_data_d.csv", colClasses = c("character",rep("numeric",6)))
t2m_min  <-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/T2Mmin_data_d.csv", colClasses = c("character",rep("numeric",6)))

for (i in 2:ncol(t2m_data)) {t2m_data[,i] <- rowMeans(cbind(t2m_data[,i],t2m_min[,i]))}
t2m_data$date<-as.Date(t2m_data$date, format = "%Y-%m-%d")
t2m_data<-subset(t2m_data, date >= min(period) & date <= max(period))


# Extract ERA5 values at specific locations
pp_era5 <-as.data.frame(t(extract(pp_era5, pp_shape)))
colnames(pp_era5)<-as.data.frame(pp_shape)$name
pp_era5 <- pp_era5[-1, ]
pp_data$date <-NULL

t2m_era5 <-as.data.frame(t(extract(t2m_era5, t2m_shape)))
colnames(t2m_era5)<-as.data.frame(t2m_shape)$name
t2m_era5 <- t2m_era5[-1, ]
t2m_data$date <-NULL


# Performance PP and T2M
KGE_pp<-KGE(sim=pp_era5, obs=pp_data, method="2012", out.type="full",na.rm=TRUE)
KGE_pp<-t(rbind(KGE_pp$KGE.value, KGE_pp$KGE.elements))

KGE2_t2m<-KGE(sim=t2m_era5, obs=t2m_data, method="2009", out.type="full",na.rm=TRUE)
KGE2_t2m<-t(rbind(KGE2_t2m$KGE.value, KGE2_t2m$KGE.elements))


