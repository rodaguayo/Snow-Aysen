rm(list=ls())
cat("\014")  

library("exactextractr")
library("TUWmodel")
library("hydroGOF")
library("hydroPSO")
library("terra")

## PP, PET and T2M
period  <- seq(from = as.Date("2016-01-01"), to = as.Date("2021-12-10", tz="UTC"), by = "day")
basins <-sf::st_read("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/GIS/Elevation_bands.shp")
areas<-as.numeric(sf::st_area(basins)/sum(sf::st_area(basins)))

pp  <- rast("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/PP_Corrected.nc")
pet <- rast("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/PET_Corrected.nc")
t2m <- (rast("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/T2M_max_Corrected.nc")+rast("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/T2M_min_Corrected.nc"))/2
pp  <- t(exact_extract(pp,  basins , 'mean'))
pet <- t(exact_extract(pet, basins , 'mean'))
t2m <- t(exact_extract(t2m, basins , 'mean'))

q_obs <-as.data.frame(read_xlsx("/home/rooda/Dropbox/Patagonia/Data/Streamflow/Data_Streamflow_v10.xlsx", sheet = "data_monthly"))
q_obs <-q_obs*1000*24*3600*c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)/area_m2
q_obs <-subset(q_obs, Date >= "1989-12-31")

## Perfomance metric (KGE 2012)
KGE_df<-data.frame(matrix(ncol = 10, nrow = 5))
colnames(KGE_df)<-c("Name","KGE_Q", "r_Q", "Beta_Q", "Gamma_Q", "KGE_SWE", "r_SWE", "Beta_SWE", "Gamma_SWE")

## Parameters: Lower and upper bound
names_param        <-  c("SCF", "DDF", "Tr",  "Ts", "Tm", "LPrat",  "FC","Beta", "k0", "k1", "k2", "lsuz", "cperc", "bmax", "croute")
lower_param        <-  c(0.9,    0,   1.0,  -3.0, -2.0,   0.1,       0,     0,    0,    2,    30,      1,     0,      0,       0)
upper_param        <-  c(1.5,    5,   3.0,   1.0,  2.0,   1.0,     600,    20,    2,   30,   250,    100,     8,     30,      50)
names(upper_param) <-  names_param
names(lower_param) <-  names_param


## Case N°1: Without calibration
ave_params    <-  (lower_param + upper_param)/2
model   <- TUWmodel(param=ave_params, prec=pp, airt=t2m, ep=pet, area=areas)
q       <- model$q*(1/86400)*sum(sf::st_area(basins))
swe     <- model$swe[,8]

q_sim   <- subset(as.numeric(model$q),   dates >= cal_period[1] & dates < cal_period[2])
swe_sim <- subset(as.numeric(model$swe), dates >= cal_period[1] & dates < cal_period[2])

KGE_q   <-KGE(sim=as.vector(q_sim_cal), obs=q_obsc_i, method="2012", out.type="full", na.rm=TRUE)
KGE_swe <-KGE(sim=as.vector(q_sim_cal), obs=q_obsc_i, method="2012", out.type="full", na.rm=TRUE)
KGE_df  <-rbind(KGE_df, c("Case NX", KGE_q$KGE.value, KGE_q$KGE.elements, KGE_swe$KGE.value, KGE_swe$KGE.value))


## Case N°2: Random Q values (N = X)
TUWhydromod <- function(param.values, obs=q_obsc_i, PP=pp_i, T2M=t2m_i, PET=pet_i, AREA=area_m2, INITIAL= t0_cond) {
  
  simLump   <- TUWmodel(param=param.values, prec=as.matrix(PP), airt=as.matrix(T2M), ep=as.matrix(PET), area=AREA, incon=INITIAL)
  q_sim     <- subset(as.numeric(simLump$q), dates >= cal_period[1] & dates < cal_period[2])
  gof       <- KGE(sim=q_sim, obs=obs, method="2012", na.rm=TRUE)
  
  out       <- vector("list", 2)   # Creating the output of the R function
  out[[1]]  <- gof
  out[[2]]  <- q_sim
  names(out) <- c("GoF", "sim")  # Mandatory names for the elements of the output
  
  return(out)}

out <- hydroPSO(fn="hydromodInR", lower=lower_param,  upper=upper_param, method="spso2011", model.FUN="TUWhydromod",
                control=list(write2disk=FALSE, MinMax="max", npart=100, maxit=100, normalise=TRUE, REPORT=5, reltol=1E-6, parallel = "parallel"),
                model.FUN.args= list(obs=q_obsc_i, PP=pp_i, T2M=t2m_i, PET=pet_i, AREA=area_m2, INITIAL= t0_cond))

tuw_model_q<-TUWmodel(prec = as.matrix(pp_i), airt= as.matrix(t2m_i), ep =  as.matrix(pet_i), 
                      area= area_m2, param=out$par, itsteps=NULL, incon=t0_cond)

KGE_q   <-KGE(sim=as.vector(q_sim_cal), obs=q_obsc_i, method="2012", out.type="full", na.rm=TRUE)
KGE_swe <-KGE(sim=as.vector(q_sim_cal), obs=q_obsc_i, method="2012", out.type="full", na.rm=TRUE)
KGE_df  <-rbind(KGE_df, c("Case NX", KGE_q$KGE.value, KGE_q$KGE.elements, KGE_swe$KGE.value, KGE_swe$KGE.value))


## Case N°3: All Q values
TUWhydromod <- function(param.values, obs=q_obsc_i, PP=pp_i, T2M=t2m_i, PET=pet_i, AREA=area_m2, INITIAL= t0_cond) {
  
  simLump   <- TUWmodel::TUWmodel(param=param.values, prec=as.matrix(PP), airt=as.matrix(T2M), ep=as.matrix(PET), area=AREA, incon=INITIAL)
  q_sim     <- subset(as.numeric(simLump$q), dates >= cal_period[1] & dates < cal_period[2])
  gof       <- KGE(sim=q_sim, obs=obs, method="2012", na.rm=TRUE)
  
  out       <- vector("list", 2)   # Creating the output of the R function
  out[[1]]  <- gof
  out[[2]]  <- q_sim
  names(out) <- c("GoF", "sim")  # Mandatory names for the elements of the output
  
  return(out)}

out <- hydroPSO(fn="hydromodInR", lower=lower_param,  upper=upper_param, method="spso2011", model.FUN="TUWhydromod",
                control=list(write2disk=FALSE, MinMax="max", npart=100, maxit=100, normalise=TRUE, REPORT=5, reltol=1E-6, parallel = "parallel"),
                model.FUN.args= list(obs=q_obsc_i, PP=pp_i, T2M=t2m_i, PET=pet_i, AREA=area_m2, INITIAL= t0_cond))

tuw_model_q<-TUWmodel(prec = as.matrix(pp_i), airt= as.matrix(t2m_i), ep =  as.matrix(pet_i), 
                      area= area_m2, param=out$par, itsteps=NULL, incon=t0_cond)

KGE_q   <-KGE(sim=as.vector(q_sim_cal), obs=q_obsc_i, method="2012", out.type="full", na.rm=TRUE)
KGE_swe <-KGE(sim=as.vector(q_sim_cal), obs=q_obsc_i, method="2012", out.type="full", na.rm=TRUE)
KGE_df  <-rbind(KGE_df, c("Case NX", KGE_q$KGE.value, KGE_q$KGE.elements, KGE_swe$KGE.value, KGE_swe$KGE.value))


## Case N°4: All SWE values
TUWhydromod <- function(param.values, obs=q_obsc_i, PP=pp_i, T2M=t2m_i, PET=pet_i, AREA=area_m2, INITIAL= t0_cond) {
  
  simLump   <- TUWmodel::TUWmodel(param=param.values, prec=as.matrix(PP), airt=as.matrix(T2M), ep=as.matrix(PET), area=AREA, incon=INITIAL)
  q_sim     <- subset(as.numeric(simLump$q), dates >= cal_period[1] & dates < cal_period[2])
  gof       <- KGE(sim=q_sim, obs=obs, method="2012", na.rm=TRUE)
  
  out       <- vector("list", 2)   # Creating the output of the R function
  out[[1]]  <- gof
  out[[2]]  <- q_sim
  names(out) <- c("GoF", "sim")  # Mandatory names for the elements of the output
  
  return(out)}

out <- hydroPSO(fn="hydromodInR", lower=lower_param,  upper=upper_param, method="spso2011", model.FUN="TUWhydromod",
                control=list(write2disk=FALSE, MinMax="max", npart=100, maxit=100, normalise=TRUE, REPORT=5, reltol=1E-6, parallel = "parallel"),
                model.FUN.args= list(obs=q_obsc_i, PP=pp_i, T2M=t2m_i, PET=pet_i, AREA=area_m2, INITIAL= t0_cond))

tuw_model_q<-TUWmodel(prec = as.matrix(pp_i), airt= as.matrix(t2m_i), ep =  as.matrix(pet_i), 
                      area= area_m2, param=out$par, itsteps=NULL, incon=t0_cond)

KGE_q   <-KGE(sim=as.vector(q_sim_cal), obs=q_obsc_i, method="2012", out.type="full", na.rm=TRUE)
KGE_swe <-KGE(sim=as.vector(q_sim_cal), obs=q_obsc_i, method="2012", out.type="full", na.rm=TRUE)
KGE_df  <-rbind(KGE_df, c("Case NX", KGE_q$KGE.value, KGE_q$KGE.elements, KGE_swe$KGE.value, KGE_swe$KGE.value))


## Case N°5: Q and SWE values
TUWhydromod <- function(param.values, obs=q_obsc_i, PP=pp_i, T2M=t2m_i, PET=pet_i, AREA=area_m2, INITIAL= t0_cond) {
  
  simLump   <- TUWmodel::TUWmodel(param=param.values, prec=as.matrix(PP), airt=as.matrix(T2M), ep=as.matrix(PET), area=AREA, incon=INITIAL)
  q_sim     <- subset(as.numeric(simLump$q), dates >= cal_period[1] & dates < cal_period[2])
  gof       <- KGE(sim=q_sim, obs=obs, method="2012", na.rm=TRUE)
  
  out       <- vector("list", 2)   # Creating the output of the R function
  out[[1]]  <- gof
  out[[2]]  <- q_sim
  names(out) <- c("GoF", "sim")  # Mandatory names for the elements of the output
  
  return(out)}

out <- hydroPSO(fn="hydromodInR", lower=lower_param,  upper=upper_param, method="spso2011", model.FUN="TUWhydromod",
                control=list(write2disk=FALSE, MinMax="max", npart=100, maxit=100, normalise=TRUE, REPORT=5, reltol=1E-6, parallel = "parallel"),
                model.FUN.args= list(obs=q_obsc_i, PP=pp_i, T2M=t2m_i, PET=pet_i, AREA=area_m2, INITIAL= t0_cond))

tuw_model_q<-TUWmodel(prec = as.matrix(pp_i), airt= as.matrix(t2m_i), ep =  as.matrix(pet_i), 
                      area= area_m2, param=out$par, itsteps=NULL, incon=t0_cond)

KGE_q   <-KGE(sim=as.vector(q_sim_cal), obs=q_obsc_i, method="2012", out.type="full", na.rm=TRUE)
KGE_swe <-KGE(sim=as.vector(q_sim_cal), obs=q_obsc_i, method="2012", out.type="full", na.rm=TRUE)
KGE_df  <-rbind(KGE_df, c("Case NX", KGE_q$KGE.value, KGE_q$KGE.elements, KGE_swe$KGE.value, KGE_swe$KGE.value))


## Case N°6: Q, SWE and Snow height
TUWhydromod <- function(param.values, obs=q_obsc_i, PP=pp_i, T2M=t2m_i, PET=pet_i, AREA=area_m2, INITIAL= t0_cond) {
  
  simLump   <- TUWmodel::TUWmodel(param=param.values, prec=as.matrix(PP), airt=as.matrix(T2M), ep=as.matrix(PET), area=AREA, incon=INITIAL)
  q_sim     <- subset(as.numeric(simLump$q), dates >= cal_period[1] & dates < cal_period[2])
  gof       <- KGE(sim=q_sim, obs=obs, method="2012", na.rm=TRUE)
  
  out       <- vector("list", 2)   # Creating the output of the R function
  out[[1]]  <- gof
  out[[2]]  <- q_sim
  names(out) <- c("GoF", "sim")  # Mandatory names for the elements of the output
  
  return(out)}

out <- hydroPSO(fn="hydromodInR", lower=lower_param,  upper=upper_param, method="spso2011", model.FUN="TUWhydromod",
                control=list(write2disk=FALSE, MinMax="max", npart=100, maxit=100, normalise=TRUE, REPORT=5, reltol=1E-6, parallel = "parallel"),
                model.FUN.args= list(obs=q_obsc_i, PP=pp_i, T2M=t2m_i, PET=pet_i, AREA=area_m2, INITIAL= t0_cond))

tuw_model_q<-TUWmodel(prec = as.matrix(pp_i), airt= as.matrix(t2m_i), ep =  as.matrix(pet_i), 
                      area= area_m2, param=out$par, itsteps=NULL, incon=t0_cond)

KGE_q   <-KGE(sim=as.vector(q_sim_cal), obs=q_obsc_i, method="2012", out.type="full", na.rm=TRUE)
KGE_swe <-KGE(sim=as.vector(q_sim_cal), obs=q_obsc_i, method="2012", out.type="full", na.rm=TRUE)
KGE_df  <-rbind(KGE_df, c("Case NX", KGE_q$KGE.value, KGE_q$KGE.elements, KGE_swe$KGE.value, KGE_swe$KGE.value))
      

