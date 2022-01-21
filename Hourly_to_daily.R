rm(list=ls())
cat("\014")  

library("lubridate")

############## Streamflow

# Barometric pressure 
pressure_raw<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/raw_data/Fraile_baro_hydro_v2.csv")
pressure_raw$DateTimeGMT0300<-as.POSIXct(pressure_raw$DateTimeGMT0300, format="%m/%d/%y %I:%M %p", tz = "America/Santarem", usetz=TRUE) #
pressure_raw$date_UTC <- lubridate::with_tz(pressure_raw$DateTimeGMT0300, "UTC")
pressure_raw$h<-as.numeric(pressure_raw$diff)*1000/(1000*9.81)*100

#Height to streamflow
model<-lm(pressure_raw$q ~ pressure_raw$h + 0)
pressure_raw$q_build<-predict(model, newdata = list(pressure_raw$h))
plot(pressure_raw$q_build)

#Hourly to date
pressure_raw_d<-aggregate(pressure_raw, FUN = mean, na.rm = TRUE, list(strftime(pressure_raw$date_UTC, format="%Y-%m-%d")))
pressure_raw_d<-pressure_raw_d[,c("Group.1", "q_build")]
colnames(pressure_raw_d)<-c("date", "q")
write.csv(pressure_raw_d, "C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/Q_data_d.csv", row.names = FALSE)


## Function to process hourly data 
hourly_to_day <- function(data) {
  data[,1] <- with_tz(data[,1], "UTC")
  data_max<-aggregate(data[,c(-1)], FUN = max, na.rm = F, list(strftime(data[,1], format="%Y-%m-%d")))
  data_min<-aggregate(data[,c(-1)], FUN = min, na.rm = F, list(strftime(data[,1], format="%Y-%m-%d")))
  data_mean<-aggregate(data[,c(-1)], FUN = mean, na.rm = F, list(strftime(data[,1], format="%Y-%m-%d")))
  data_sum<-aggregate(data[,c(-1)], FUN = sum, na.rm = F, list(strftime(data[,1], format="%Y-%m-%d")))
  data<-cbind(unique(strftime(data[,1], format="%Y-%m-%d")), data_mean, data_max, data_min, data_sum)
  return(data)
}

data_INIA<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/raw_data/El_Claro_INIA.csv")
data_INIA[,1]<-as.POSIXct(data_INIA[,1], format="%d-%m-%Y %H:%M", tz = "America/Santarem", usetz=TRUE) 
data_INIA<-hourly_to_day(data_INIA)
write.csv(data_INIA, "C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/raw_data/El_Claro_INIAc.csv", row.names = FALSE)

data_DGA<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/raw_data/Cerro_Divisadero_DGA.csv")
data_DGA[,1]<-as.POSIXct(data_DGA[,1], format="%d/%m/%Y %H:%M", tz = "America/Santarem", usetz=TRUE) 
data_DGA<-hourly_to_day(data_DGA)
write.csv(data_DGA, "C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/raw_data/Cerro_Divisadero_DGAc.csv", na = "", row.names = FALSE)

data_CIEP<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/raw_data/El_Fraile_CIEP.csv")
data_CIEP[,1]<-as.POSIXct(data_CIEP[,1], format="%Y.%m.%d %H:%M", tz = "America/Santarem", usetz=TRUE) 
data_CIEP<-hourly_to_day(data_CIEP)
write.csv(data_CIEP, "C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/raw_data/data_CIEPc.csv", na = "", row.names = FALSE)

data_DGA<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/raw_data/El_Fraile_DGA.csv")
data_DGA[,1]<-as.POSIXct(data_DGA[,1], format="%Y-%m-%d %H:%M", tz = "America/Santarem", usetz=TRUE) 
data_DGA<-hourly_to_day(data_DGA)
write.csv(data_DGA, "C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/raw_data/El_Fraile_DGAc.csv", na = "", row.names = FALSE)


