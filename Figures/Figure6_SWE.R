Sys.setenv(PATH=paste0("C:/Users/rooda/AppData/Local/Programs/orca;", Sys.getenv("PATH")))
rm(list=ls())
cat("\014")  

library("plotly")
library("RColorBrewer")

f <- list(family = "Verdana", size = 22)
f2 <- list(family = "Verdana", size = 18)

data<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Data/SNOW_data_p.csv")
data$stake   <- as.factor(data$stake)

f <- list(family = "Verdana", size = 16)
f2 <- list(family = "Verdana", size = 12)

x <- list(titlefont = f, tickfont = f2, ticks = "outside")
y <- list(title = "Snow depth (m)", titlefont = f, tickfont = f2, ticks = "outside", zeroline = F)
legend <- list(orientation = "h", x = 0.05, y = 1.15)

fig1 <- plot_ly(data, y = ~snow_depth_m, x = ~stake, type = "box", color = ~Date, colors = brewer.pal(10, "YlGnBu")[3:9])
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y, showlegend = TRUE)
fig1 <- fig1 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig1 <- fig1 %>% layout(legend = legend, boxmode = "group")
fig1

y2 <- list(title = "SWE (mm)", titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE)

fig2 <- plot_ly(data, y = ~swe_mm, x = ~stake, type = "box", color = ~Date, colors = brewer.pal(10, "YlGnBu")[3:9])
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2, showlegend = FALSE)
fig2 <- fig2 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig2 <- fig2 %>% layout(legend = legend, boxmode = "group")
fig2

server <- orca_serve()
setwd("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Figures/")
server$export(fig1, file = "Figure7a_snow.png", width = 950, height = 400, scale = 3)
server$export(fig2, file = "Figure7b_swe.png", width = 950, height = 400, scale = 3)
server$close()