rm(list=ls())
cat("\014")  

library("dplyr")
library("exactextractr")
library("TUWmodel")
library("hydroGOF")
library("hydroPSO")
library("terra")
library("sf")

## PP, PET and T2M
date  <- seq(from = as.Date("2016-01-01"), to = as.Date("2021-12-09", tz="UTC"), by = "day")
basins  <- st_read("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/GIS/Elevation_bands.shp")
areas<-as.numeric(sf::st_area(basins)/sum(sf::st_area(basins)))

pp  <- rast("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/PP_Corrected.nc")
pet <- rast("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/PET_Corrected.nc")
t2m <- (rast("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/T2M_max_Corrected.nc")+rast("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/T2M_min_Corrected.nc"))/2
pp  <- t(exact_extract(pp,  basins , 'mean'))[-nlyr(pp), ] #Problem in last day 
pet <- t(exact_extract(pet, basins , 'mean'))[-nlyr(pet), ]
t2m <- t(exact_extract(t2m, basins , 'mean'))[-nlyr(t2m), ]
pet[is.na(pet)]<-0

set.seed(5)
q_obs <-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/Q_data_d.csv")
q_obs$q <-q_obs$q*1000*84600/as.numeric(sum(st_area(basins)))
q_obs$date <-as.Date(q_obs$date, format ="%Y-%m-%d") 
q_obs  <- subset(q_obs, date >= min(date) & date <= max(date))
q_obs$r<- q_obs$q
q_obs$r[match(sample(q_obs$r, length(q_obs$r)*0.95), q_obs$r)] <- NA
q_obs  <- merge(q_obs, data.frame(date), all = TRUE)

# Snow scale to Banda 5(6)
swe_route<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/SNOW_data_p.csv")
swe_route<-aggregate(swe_route, FUN = mean, na.rm = TRUE, list(swe_route$Date, swe_route$stake))
swe_route<-swe_route[,c("Group.1","Group.2","swe_mm")]
colnames(swe_route)<-c("date","stake","swe")
swe_route$date<-as.Date(swe_route$date)
swe_route_p <-data.frame(date)
swe_route_p$RN1 <- merge(swe_route_p, subset(swe_route, stake == "RN1"), all = TRUE)$swe
swe_route_p$RN2 <- merge(swe_route_p, subset(swe_route, stake == "RN2"), all = TRUE)$swe
swe_route_p$RN3 <- merge(swe_route_p, subset(swe_route, stake == "RN3"), all = TRUE)$swe
swe_route_p$RN4 <- merge(swe_route_p, subset(swe_route, stake == "RN4"), all = TRUE)$swe
swe_route_p$RN7 <- merge(swe_route_p, subset(swe_route, stake == "RN7"), all = TRUE)$swe

## Perfomance metric (KGE 2012)
KGE_df<-data.frame(matrix(ncol = 9, nrow = 5))
colnames(KGE_df)<-c("Name","KGE_Q", "r_Q", "Beta_Q", "Gamma_Q", "KGE_SWE", "r_SWE", "Beta_SWE", "Gamma_SWE")

## Parameters: Lower and upper bound
lower_param        <-  c(0.9,  0.1,   1.0,  -3.0, -2.0,   0.1,      10,   0.1,    0,    2,    30,      1,     0,      0,       0)
upper_param        <-  c(1.5,    5,   3.0,   1.0,  2.0,   1.0,     600,    20,    2,   30,   250,    100,     8,     30,      50)
names(upper_param) <-  c("SCF", "DDF", "Tr",  "Ts", "Tm", "LPrat",  "FC","Beta", "k0", "k1", "k2", "lsuz", "cperc", "bmax", "croute")
names(lower_param) <-  c("SCF", "DDF", "Tr",  "Ts", "Tm", "LPrat",  "FC","Beta", "k0", "k1", "k2", "lsuz", "cperc", "bmax", "croute")


## Case N°1: Without calibration
ave_params    <-  (lower_param + upper_param)/2
model_1       <- TUWmodel(param=ave_params, prec=pp, airt=t2m, ep=pet, area=areas)
q_obs$case_1  <- model_1$q
swe_route_p$RN2_case_1<-model_1$swe[,2]

KGE_q         <-KGE(sim=q_obs$case_1, obs=q_obs$q, method="2012", out.type="full", na.rm=TRUE)
KGE_swe       <-KGE(sim=c(model_1$swe[,1], model_1$swe[,2], model_1$swe[,3], model_1$swe[,8]), 
                    obs=c(swe_route_p$RN1, swe_route_p$RN2, swe_route_p$RN4, swe_route_p$RN7), method="2012", out.type="full", na.rm=TRUE)
KGE_df[1,]     <-c("Case N1", KGE_q$KGE.value, KGE_q$KGE.elements, KGE_swe$KGE.value, KGE_swe$KGE.elements)


## Case N°2: Random Q values (N = X)
TUWhydromod <- function(param.values, obs=q_obs$r, prec=pp, airt=t2m, ep=pet, area=areas) {
  
  model_2   <- TUWmodel(param=param.values, prec=pp, airt=t2m, ep=pet, area=areas)
  gof       <- KGE(model_2$q, obs, method="2012", na.rm=TRUE)
  
  out       <- vector("list", 2)   # Creating the output of the R function
  out[[1]]  <- gof
  out[[2]]  <- model_2$q
  names(out) <- c("GoF", "sim")  # Mandatory names for the elements of the output 
  
  return(out)}

out <- hydroPSO(fn="hydromodInR", lower=lower_param,  upper=upper_param, method="spso2011", model.FUN="TUWhydromod",
                control=list(write2disk=FALSE, MinMax="max", npart=80, maxit=100, normalise=TRUE, REPORT=5, reltol=1E-6),
                model.FUN.args= list(obs=q_obs$q, prec=pp, airt=t2m, ep=pet, area=areas))

model_2<-TUWmodel(param=out$par, prec=pp, airt=t2m, ep=pet, area=areas)
q_obs$case_2  <- model_2$q
swe_route_p$RN2_case_2<-model_2$swe[,2]

KGE_q         <-KGE(sim=q_obs$case_2, obs=q_obs$q, method="2012", out.type="full", na.rm=TRUE)
KGE_swe       <-KGE(sim=c(model_2$swe[,1], model_2$swe[,2], model_2$swe[,3], model_2$swe[,8]), 
                    obs=c(swe_route_p$RN1, swe_route_p$RN2, swe_route_p$RN4, swe_route_p$RN7), method="2012", out.type="full", na.rm=TRUE)
KGE_df[2,]        <-c("Case N2", KGE_q$KGE.value, KGE_q$KGE.elements, KGE_swe$KGE.value, KGE_swe$KGE.elements)



## Case N°3: All Q values
TUWhydromod <- function(param.values, obs=q_obs$q, prec=pp, airt=t2m, ep=pet, area=areas) {
  
  model_3   <- TUWmodel(param=param.values, prec=pp, airt=t2m, ep=pet, area=areas)
  gof       <-KGE(model_3$q, obs, method="2012", na.rm=TRUE)
  
  out       <- vector("list", 2)   # Creating the output of the R function
  out[[1]]  <- gof
  out[[2]]  <- model_3$q
  names(out) <- c("GoF", "sim")  # Mandatory names for the elements of the output 
  
  return(out)}

out <- hydroPSO(fn="hydromodInR", lower=lower_param,  upper=upper_param, method="spso2011", model.FUN="TUWhydromod",
                control=list(write2disk=FALSE, MinMax="max", npart=80, maxit=100, normalise=TRUE, REPORT=5, reltol=1E-6),
                model.FUN.args= list(obs=q_obs$q, prec=pp, airt=t2m, ep=pet, area=areas))

model_3<-TUWmodel(param=out$par, prec=pp, airt=t2m, ep=pet, area=areas)
q_obs$case_3  <- model_3$q
swe_route_p$RN2_case_3<-model_3$swe[,2]

KGE_q         <-KGE(sim=q_obs$case_3, obs=q_obs$q, method="2012", out.type="full", na.rm=TRUE)
KGE_swe       <-KGE(sim=c(model_3$swe[,1], model_3$swe[,2], model_3$swe[,3], model_3$swe[,8]), 
                    obs=c(swe_route_p$RN1, swe_route_p$RN2, swe_route_p$RN4, swe_route_p$RN7), method="2012", out.type="full", na.rm=TRUE)
KGE_df[3,]    <-c("Case N3", KGE_q$KGE.value, KGE_q$KGE.elements, KGE_swe$KGE.value, KGE_swe$KGE.elements)



## Case N°4: All SWE values from the snow route
TUWhydromod <- function(param.values, obs=q_obs$q, prec=pp, airt=t2m, ep=pet, area=areas) {
  
  model_4   <- TUWmodel(param=param.values, prec=pp, airt=t2m, ep=pet, area=areas)
  gof       <- KGE(sim=c(model_4$swe[,1], model_4$swe[,2], model_4$swe[,3], model_4$swe[,8]), 
                   obs=c(swe_route_p$RN1, swe_route_p$RN2, swe_route_p$RN4, swe_route_p$RN7), method="2012", na.rm=TRUE)
  
  out       <- vector("list", 2)   # Creating the output of the R function
  out[[1]]  <- gof
  out[[2]]  <- model_4$q
  names(out) <- c("GoF", "sim")  # Mandatory names for the elements of the output 
  
  return(out)}
  
out <- hydroPSO(fn="hydromodInR", lower=lower_param,  upper=upper_param, method="spso2011", model.FUN="TUWhydromod",
                control=list(write2disk=FALSE, MinMax="max", npart=80, maxit=100, normalise=TRUE, REPORT=5, reltol=1E-6),
                model.FUN.args= list(obs=q_obs$q, prec=pp, airt=t2m, ep=pet, area=areas))

model_4<-TUWmodel(param=out$par, prec=pp, airt=t2m, ep=pet, area=areas)
q_obs$case_4  <- model_4$q
swe_route_p$RN2_case_4<-model_4$swe[,2]

KGE_q         <-KGE(sim=q_obs$case_4, obs=q_obs$q, method="2012", out.type="full", na.rm=TRUE)
KGE_swe       <-KGE(sim=c(model_4$swe[,1], model_4$swe[,2], model_4$swe[,3], model_4$swe[,8]), 
                    obs=c(swe_route_p$RN1, swe_route_p$RN2, swe_route_p$RN4, swe_route_p$RN7), method="2012", out.type="full", na.rm=TRUE)
KGE_df[4,]    <-c("Case N4", KGE_q$KGE.value, KGE_q$KGE.elements, KGE_swe$KGE.value, KGE_swe$KGE.elements)



## Case N°5: Q and SWE values
TUWhydromod <- function(param.values, obs=q_obs$q, prec=pp, airt=t2m, ep=pet, area=areas) {
  
  model_5   <- TUWmodel(param=param.values, prec=pp, airt=t2m, ep=pet, area=areas)
  gof1      <- KGE(sim=c(model_5$swe[,1], model_5$swe[,2], model_5$swe[,3], model_5$swe[,8]), 
                   obs=c(swe_route_p$RN1, swe_route_p$RN2, swe_route_p$RN4, swe_route_p$RN7), method="2012", na.rm=TRUE)
  gof2      <- KGE(model_5$q, obs, method="2012", na.rm=TRUE)
  
  out       <- vector("list", 2)   # Creating the output of the R function
  out[[1]]  <- gof1*0.2 + gof2*0.8
  out[[2]]  <- model_5$q
  names(out) <- c("GoF", "sim")  # Mandatory names for the elements of the output 
  
  return(out)}

out <- hydroPSO(fn="hydromodInR", lower=lower_param,  upper=upper_param, method="spso2011", model.FUN="TUWhydromod",
                control=list(write2disk=FALSE, MinMax="max", npart=80, maxit=100, normalise=TRUE, REPORT=5, reltol=1E-6),
                model.FUN.args= list(obs=q_obs$q, prec=pp, airt=t2m, ep=pet, area=areas))

model_5<-TUWmodel(param=out$par, prec=pp, airt=t2m, ep=pet, area=areas)
q_obs$case_5  <- model_5$q
swe_route_p$RN2_case_5<-model_5$swe[,2]

KGE_q         <-KGE(sim=q_obs$case_5, obs=q_obs$q, method="2012", out.type="full", na.rm=TRUE)
KGE_swe       <-KGE(sim=c(model_5$swe[,1], model_5$swe[,2], model_5$swe[,3], model_5$swe[,8]), 
                    obs=c(swe_route_p$RN1, swe_route_p$RN2, swe_route_p$RN4, swe_route_p$RN7), method="2012", out.type="full", na.rm=TRUE)
KGE_df[5,]    <-c("Case N5", KGE_q$KGE.value, KGE_q$KGE.elements, KGE_swe$KGE.value, KGE_swe$KGE.elements)


write.csv(q_obs,  "C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/q_results.csv", row.names = FALSE)
write.csv(swe_route_p,  "C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/swe_results.csv")
write.csv(KGE_df,  "C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/KGE_results.csv")




      

