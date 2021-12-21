rm(list=ls())
cat("\014")  

library("plotly")
library("RColorBrewer")

Sys.setenv(PATH=paste0("C:/Users/rooda/AppData/Local/Programs/orca;", Sys.getenv("PATH")))

data_pp  <-read.csv("C:/Users/rooda/Dropbox/Patagonia/Trends/Trends_Precipitation.csv")
data_t2m <-read.csv("C:/Users/rooda/Dropbox/Patagonia/Trends/Trends_Temperature.csv")
data_q   <-read.csv("C:/Users/rooda/Dropbox/Patagonia/Trends/Trends_Streamflow.csv")
data_pp$Zone  <- factor(data_pp$Zone, levels = c("Northern", "Center", "Southern"))
data_t2m$Zone <- factor(data_t2m$Zone, levels = c("Northern", "Center", "Southern"))
data_q$Zone   <- factor(data_q$Zone, levels = c("Northern", "Center", "Southern"))

f <- list(family = "Verdana", size = 16)
f2 <- list(family = "Verdana", size = 12)
marker <- list(color = ~Zone, size = 5)

x <- list(titlefont = f, tickfont = f2, ticks = "outside")
title <-list(text = "a) Precipitation", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0)
y <- list(title = "Sen's slope (% per decade)", titlefont = f, tickfont = f2, ticks = "outside", zeroline = F, 
          range = c(-0.25,0.25), tickformat  = ".0%")
legend <- list(orientation = "h", x = 0.65, y = 0.97)


fig1 <- plot_ly(data_pp, y = ~sen_slope, x = ~season, boxpoints = "all",  pointpos = 0, marker = marker, type = "box", split = ~Zone)
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y, showlegend = TRUE)
fig1 <- fig1 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig1 <- fig1 %>% layout(annotations = title, legend = legend, boxmode = "group")
fig1

title2 <-list(text = "b) Temperature", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0)
y2 <- list(title = "Sen's slope (ºC per decade)", titlefont = f, range = c(-0.5,0.5), 
           tickfont = f2, dtick = 0.2, ticks = "outside", zeroline = FALSE)

fig2 <- plot_ly(data_t2m, y = ~sen_slope, x = ~season, boxpoints = "all",  pointpos = 0, marker = marker, type = "box", split = ~Zone)
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2, showlegend = FALSE)
fig2 <- fig2 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig2 <- fig2 %>% layout(annotations = title2, boxmode = "group")
fig2

title3 <-list(text = "c) Streamflow", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0)
y3 <- list(title = "Sen's slope (% per decade)", titlefont = f, tickfont = f2, ticks = "outside", zeroline = F, 
           range = c(-0.25,0.25), tickformat  = ".0%")

fig3 <- plot_ly(data_q, y = ~sen_slope, x = ~season, boxpoints = "all",  pointpos = 0, marker = marker, type = "box", split = ~Zone)
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y3, showlegend = FALSE)
fig3 <- fig3 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig3 <- fig3 %>% layout(annotations = title3, boxmode = "group")
fig3

server <- orca_serve()
setwd("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Figures/")
server$export(fig1, file = "Figure1a_trend.png", width = 1000, height = 300, scale = 3)
server$export(fig2, file = "Figure1b_trend.png", width = 1000, height = 300, scale = 3)
server$export(fig3, file = "Figure1c_trend.png", width = 1000, height = 300, scale = 3)
server$close()

