Sys.setenv(PATH=paste0("C:/Users/rooda/AppData/Local/Programs/orca;", Sys.getenv("PATH")))
rm(list=ls())
cat("\014")  

library("readxl")
library("plotly")
library("RColorBrewer")

f <- list(family = "Verdana", size = 22)
f2 <- list(family = "Verdana", size = 18)

#Observed data
data_swe<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/swe_results.csv")
data_swe$date<-as.Date(data_swe$date)
data_swe<-subset(data_swe, date > as.Date("2017-01-01"))

data_q<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Results/q_results.csv")
data_q$date<-as.Date(data_q$date)
data_q<-subset(data_q, date > as.Date("2017-01-01"))

x <- list(titlefont = f, tickfont = f2, ticks = "outside", type = 'date', tickformat = "%Y")
y1 <- list(titlefont = f, title = "Q (mm/day)", tickfont = f2, ticks = "outside", zeroline = FALSE)
colors<-brewer.pal(10, 'Paired')

fig1 <- plot_ly(data_q, showlegend = T, y = ~case_1, x = ~date, type = 'scatter', mode = 'lines', name = "Case 1",  opacity  = 1, line = list(color = colors[2], width = 1))
fig1 <- fig1 %>% add_trace(y = ~case_2, mode = 'lines', name = "Case 2",  opacity  = 1, line = list(color = colors[4], width = 1))
fig1 <- fig1 %>% add_trace(y = ~case_3, mode = 'lines', name = "Case 3",  opacity  = 1, line = list(color = colors[6], width = 1))
fig1 <- fig1 %>% add_trace(y = ~case_4, mode = 'lines', name = "Case 4",  opacity  = 1, line = list(color = colors[8], width = 1))
fig1 <- fig1 %>% add_trace(y = ~case_5, mode = 'lines', name = "Case 5",  opacity  = 1, line = list(color = colors[10], width = 1))
fig1 <- fig1 %>% add_trace(y = ~q, mode = 'lines', name = "Observed", opacity  = 1, line = list(color = "black", width = 1))
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y1, legend = list(orientation = "h", font = f2))

x <- list(titlefont = f, tickfont = f2, ticks = "outside", type = 'date', tickformat = "%Y")
y2 <- list(titlefont = f, title = "SWE - R1 (mm)", tickfont = f2, ticks = "outside", zeroline = FALSE)

fig2 <- plot_ly(data_swe, showlegend = FALSE, y = ~RN1_case_1, x = ~date, type = 'scatter', mode = 'lines', name = "Case 1",  opacity  = 1, line = list(color = colors[2], width = 1))
fig2 <- fig2 %>% add_trace(y = ~RN1_case_2, mode = 'lines', name = "Case 2",  opacity  = 1, line = list(color = colors[4], width = 1))
fig2 <- fig2 %>% add_trace(y = ~RN1_case_3, mode = 'lines', name = "Case 3",  opacity  = 1, line = list(color = colors[6], width = 1))
fig2 <- fig2 %>% add_trace(y = ~RN1_case_4, mode = 'lines', name = "Case 4",  opacity  = 1, line = list(color = colors[8], width = 1))
fig2 <- fig2 %>% add_trace(y = ~RN1_case_5, mode = 'lines', name = "Case 5",  opacity  = 1, line = list(color = colors[10], width = 1))
fig2 <- fig2 %>% add_trace(y = ~RN1, mode = 'scatter', name = "Observed",  opacity  = 1, line = list(color = "black", width = 2))
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2)

data_q2<-as.data.frame(t(hydroTSM::monthlyfunction(data_q, FUN = mean)))
data_q2$month<- factor(rownames(data_q2), levels = c(rownames(data_q2)))

x <- list(titlefont = f, tickfont = f2, ticks = "outside")
y3 <- list(title = "Streamflow (mm/month)", titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE)

fig3 <- plot_ly(data_q2, y = ~case_1*30, showlegend = FALSE, x = ~month, type = 'scatter', mode = 'lines+markers', name = "GR4J",  marker = list(size = 3, color = colors[2]), line = list(color = colors[2], width = 1))
fig3 <- fig3 %>% add_trace(y = ~case_2*30, mode = 'lines+markers', name = "GR5J", marker = list(size = 3, color = colors[4]), line = list(color = colors[4], width = 1))
fig3 <- fig3 %>% add_trace(y = ~case_3*30, mode = 'lines+markers', name = "GR6J", marker = list(size = 3, color = colors[6]), line = list(color = colors[6], width = 1))
fig3 <- fig3 %>% add_trace(y = ~case_4*30, mode = 'lines+markers', name = "Case 4",  marker = list(size = 3, color = colors[8]), line = list(color = colors[8], width = 1))
fig3 <- fig3 %>% add_trace(y = ~case_5*30, mode = 'lines+markers', name = "Case 5",  marker = list(size = 3, color = colors[10]), line = list(color = colors[10], width = 1))
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y3, showlegend = FALSE)

data_q3<-as.data.frame(hydroTSM::daily2monthly(data_q, FUN = sum))
data_q3f<-as.data.frame(hydroTSM::fdc(data_q3, plot =FALSE))

x <- list(title = "Probability (%)", titlefont = f, tickfont = f2, ticks = "outside")
y4 <- list(title = "Streamflow (mm/month)", titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE)

fig4 <- plot_ly(showlegend = FALSE, y = sort(data_q3$case_1), x =  sort(data_q3f$case_1)*100, type = 'scatter', mode = 'lines', name = "Case 1",  line = list(size = 3, color = colors[2]))
fig4 <- fig4 %>% add_trace(y = sort(data_q3$case_2), x =  sort(data_q3f$case_2)*100, mode = 'lines', name = "Case 2", line = list(size = 3, color = colors[4]))
fig4 <- fig4 %>% add_trace(y = sort(data_q3$case_3), x =  sort(data_q3f$case_3)*100, mode = 'lines', name = "Case 3", line = list(size = 3, color = colors[6]))
fig4 <- fig4 %>% add_trace(y = sort(data_q3$case_4), x =  sort(data_q3f$case_4)*100, mode = 'lines', name = "Case 4", line = list(size = 3, color = colors[8]))
fig4 <- fig4 %>% add_trace(y =sort(data_q3$case_5),  x =  sort(data_q3f$case_5)*100, mode = 'lines', name = "Case 5", line = list(size = 3, color = colors[10]))
fig4 <- fig4 %>% layout(xaxis = x, yaxis = y4)

s1  <- subplot(fig1, fig2, nrows = 2, shareX = T, shareY= F, titleY = T, titleX = T, margin = c(0.04, 0.04, 0.04, 0.04))
s2  <- subplot(fig3, fig4, shareX = F, shareY= T, titleY = T, titleX = T, margin = c(0.04, 0.04, 0.04, 0.04))
fig <- subplot(s1, s2, nrows = 2,  titleY = T, titleX = T, margin = c(0.04, 0.04, 0.04, 0.04))
fig

htmlwidgets::saveWidget(as_widget(fig), "C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Figures/Figure_Hydro.html")

server <- orca_serve()
server$export(fig, file = "C:/Users/rooda/Dropbox/Proyectos/Semilla-Aysen/Figures/Figure_Hydro.png", width = 1000, height = 1000, scale = 3)
server$close()
