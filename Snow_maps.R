rm(list=ls())
cat("\014")  

library("sen2r")

# Set paths
out_dir_1  <- tempfile("C:/Users/rooda/Desktop/test3", pattern = "sen2r_out_1_") # output folder
safe_dir <- tempfile("C:/Users/rooda/Desktop/test4", pattern = "sen2r_safe_")  # folder to store downloaded SAFE

myextent_1 <- sf::st_read("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Cuenca_Puelo.shp")

out_paths_1 <- sen2r(
  gui = FALSE,
  step_atmcorr = "l2a",
  extent = myextent_1,
  extent_name = "Aysen",
  timewindow = c(as.Date("2020-09-13"), as.Date("2020-11-25")),
  server = "gcloud",
  list_indices = c("NDSI"),
  max_mask = 10, 
  path_l2a = safe_dir,
  path_out = out_dir_1
)


s2_translate("C:/Users/rooda/Dropbox/Coding/Snow-Aysen/S2A_MSIL2A_20210502T141731_N0300_R010_T18GYQ_20210502T183146.SAFE")
x_out <- s2_calcindices(infiles = "C:/Users/rooda/Dropbox/Coding/Snow-Aysen/S2A2A_20210502_010_18GYQ_BOA_10.vrt",
  indices = "NDSI",
  dataType = "Float32"
)