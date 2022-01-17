Sys.setenv(PATH=paste0("C:/Users/rooda/AppData/Local/Programs/orca;", Sys.getenv("PATH")))
rm(list=ls())
cat("\014")  

library("readxl")
library("plotly")
library("RColorBrewer")

f <- list(family = "Verdana", size = 22)
f2 <- list(family = "Verdana", size = 18)

data<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/ci_values.csv")
x <- list(titlefont = f, tickfont = f2, ticks = "outside", type = 'date')
y1 <- list(titlefont = f, title = "% of days T2m > 90th", tickfont = f2, ticks = "outside", zeroline = FALSE)

fig1 <- plot_ly(data, y = ~tx90p, x = ~date, type = 'scatter', mode = 'lines+markers',  
                marker = list(size = 10), line = list(width = 0.5, dash = 'dot'))
fig1 <- fig1 %>% add_trace(x= as.numeric(unlist(data %>% filter(!is.na(tx90p)) %>% select(date))), y=predict(lm(tx90p~date, data)), 
                           marker = list(opacity=0), line = list(width = 5, dash = "solid", color = "rgba(0,0,0,0.5)"))
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y1, showlegend = FALSE)

y2 <- list(titlefont = f, title = "Max 5-day PP (mm)", tickfont = f2, ticks = "outside", zeroline = FALSE, side = "right", dtick = 100)
fig2 <- plot_ly(data, y = ~rx5day, x = ~date, type = 'scatter', mode = 'lines+markers',  
                marker = list(size = 10), line = list(width = 0.5, dash = 'dot'))
fig2 <- fig2 %>% add_trace(x= as.numeric(unlist(data %>% filter(!is.na(rx5day)) %>% select(date))), y=predict(lm(rx5day~date, data)), 
                           marker = list(opacity=0), line = list(width = 5, dash = "solid", color = "rgba(0,0,0,0.5)"))
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2, showlegend = FALSE)


y3 <- list(titlefont = f, title = "Simple PP intensity", tickfont = f2, ticks = "outside", zeroline = FALSE, dtick = 3)
fig3 <- plot_ly(data, y = ~sdii, x = ~date, type = 'scatter', mode = 'lines+markers',  
                marker = list(size = 10), line = list(width = 0.5, dash = 'dot'))
fig3 <- fig3 %>% add_trace(x= as.numeric(unlist(data %>% filter(!is.na(sdii)) %>% select(date))), y=predict(lm(sdii~date, data)), 
                           marker = list(opacity=0), line = list(width = 5, dash = "solid", color = "rgba(0,0,0,0.5)"))
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y3, showlegend = FALSE)

y4 <- list(titlefont = f, title = "Total annual PP (mm)", tickfont = f2, ticks = "outside", zeroline = FALSE, side = "right")
fig4 <- plot_ly(data, y = ~prcptot, x = ~date, type = 'scatter', mode = 'lines+markers',  
                marker = list(size = 10), line = list(width = 0.5, dash = 'dot'))
fig4 <- fig4 %>% add_trace(x= as.numeric(unlist(data %>% filter(!is.na(prcptot)) %>% select(date))), y=predict(lm(prcptot~date, data)), 
                           marker = list(opacity=0), line = list(width = 5, dash = "solid", color = "rgba(0,0,0,0.5)"))
fig4 <- fig4 %>% layout(xaxis = x, yaxis = y4, showlegend = FALSE)

y5 <- list(titlefont = f, title = "Daily T2M range (ºC)", tickfont = f2, ticks = "outside", zeroline = FALSE)
fig5 <- plot_ly(data, y = ~dtr, x = ~date, type = 'scatter', mode = 'lines+markers',  
                marker = list(size = 10), line = list(width = 0.5, dash = 'dot'))
fig5 <- fig5 %>% add_trace(x= as.numeric(unlist(data %>% filter(!is.na(dtr)) %>% select(date))), y=predict(lm(dtr~date, data)), 
                           marker = list(opacity=0), line = list(width = 5, dash = "solid", color = "rgba(0,0,0,0.5)"))
fig5 <- fig5 %>% layout(xaxis = x, yaxis = y5, showlegend = FALSE)


fig <- subplot(fig1, fig2, fig3, fig4, fig5, nrows = 5, shareY = T, shareX = T, margin = c(-0.04, -0.04, 0, 0))
fig

server <- orca_serve()
server$export(fig, file = "C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Figures/Figure4_Climate_trends.png", width = 900, height = 1200, scale = 3)
server$close()
