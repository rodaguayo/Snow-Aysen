rm(list=ls())
cat("\014")  

library("plotly")
library("RColorBrewer")
library("climdex.pcic")
library("TTAinterfaceTrendAnalysis")


## PP, Tmax, Tmin data
pp_data<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/PP_data_d.csv")
tmax_data<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/T2Mmax_data_d.csv")
tmin_data<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/T2Mmin_data_d.csv")
pp_data<-fillGap(pp_data, corPeriod = "daily")

dates <- as.PCICt("1950-01-01", "gregorian") + (1:length(pp_data$date) * 86400)

ci<-climdexInput.raw(tmax = as.numeric(tmax_data$Teniente_Vidal_Coyhaique), 
                     tmin = as.numeric(tmin_data$Teniente_Vidal_Coyhaique), 
                     prec = as.numeric(pp_data$Teniente_Vidal_Coyhaique), 
                     tmax.dates = dates, tmin.dates = dates, prec.dates = dates,
                     base.range = c(1960, 1990), n = 5, northern.hemisphere = FALSE,
                     tavg = NULL, tavg.dates = NULL, quantiles = NULL, temp.qtiles = c(0.1,0.9), 
                     prec.qtiles = c(0.95, 0.99), max.missing.days = c(annual = 30,monthly = 7), 
                     min.base.data.fraction.present = 0.1)

ci_values <- as.data.frame(matrix(nrow=73, ncol=27, byrow = TRUE))
colnames(ci_values)<-climdex.get.available.indices(ci, function.names = FALSE)
rownames(ci_values)<-1950:2022

ci_values[,1]<-climdex.su(ci)
ci_values[,2]<-climdex.id(ci)
ci_values[,3]<-climdex.txx(ci, freq = "annual")
ci_values[,4]<-climdex.txn(ci, freq = "annual")
ci_values[,5]<-climdex.tx10p(ci, freq = "annual")
ci_values[,6]<-climdex.tx90p(ci, freq = "annual")
ci_values[,7]<-climdex.fd(ci)
ci_values[,8]<-climdex.su(ci)
ci_values[,9]<-climdex.tr(ci)
ci_values[,10]<-climdex.tnx(ci, freq = "annual")
ci_values[,11]<-climdex.tnn(ci, freq = "annual")
ci_values[,12]<-climdex.tn10p(ci, freq = "annual")
ci_values[,13]<-climdex.tn90p(ci, freq = "annual")
ci_values[,14]<-climdex.csdi(ci)
ci_values[,15]<-climdex.rx1day(ci, freq = "annual")
ci_values[,16]<-climdex.rx5day(ci, freq = "annual")
ci_values[,17]<-climdex.sdii(ci)
ci_values[,18]<-climdex.r10mm(ci)
ci_values[,19]<-climdex.r20mm(ci)
ci_values[,20]<-climdex.rnnmm(ci, threshold = 1)
ci_values[,21]<-climdex.cdd(ci)
ci_values[,22]<-climdex.cwd(ci)
ci_values[,23]<-climdex.r95ptot(ci)
ci_values[,24]<-climdex.r99ptot(ci)
ci_values[,25]<-climdex.prcptot(ci)
ci_values[,26]<-climdex.gsl(ci)
ci_values[,27]<-climdex.dtr(ci, freq = "annual")

mk_values <- as.data.frame(matrix(nrow=27, ncol=2, byrow = TRUE))
rownames(mk_values)<-climdex.get.available.indices(ci, function.names = FALSE)
colnames(mk_values)<-c("sen_slope","p_value")

for (i in 1:27) {
  mk_serie<-mannKen(ci_values[,i])
  mk_values[i,1]<-mk_serie$sen.slope
  mk_values[i,2]<-mk_serie$p.value
}

mk_values$p_value[mk_values$p_value > 0.05] <- NA

write.csv(mk_values, "C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/mk_values.csv")
write.csv(ci_values, "C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/ci_values.csv")
