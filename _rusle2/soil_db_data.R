#================================ Setup ================================

# libraries needed
libs <- c("RODBC", "aqp", "soilDB", "plyr", "lidR", "shapefiles", "sf", "terra", "raster", "tidyr", "dplyr", "ggplot2", "easypackages", "spatialEco", "rasterVis", "raster", "elevatr", "mapview", "leaflet", "tmap", "RColorBrewer", "exactextractr", "emmeans", "lidR", "shapefiles", "sf", "terra", "raster", "tidyr", "dplyr", "ggplot2", "easypackages", "spatialEco","here", "performance", "see", "RColorBrewer", "lme4", "nlme", "readxl", "writexl", "emmeans", "splines", "lspline", "ggeffects", "lubridate", "cowplot", "gridGraphics", "broom", "DT")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs], dep = TRUE)
}

# load libraries
lapply(libs, library, character.only = T)

# enter the file path for the highest level folder you're working in 
data_folder <- "C:/Users/natha/Box/"

# when a file is needed, call the hert() function
# for example; data_frame = read.csv(hert("more_data/measurements_data.csv"))
hert <- function(file) {
  file_path = paste(data_folder, file, sep = "")
  return(file_path)
}

#================================ Pulling SURGO data ================================

# get component-level data for a specific soil survey area (Yolo county, CA)
q <- "SELECT 
component.mukey, cokey, comppct_r, compname, taxclname, 
taxorder, taxsuborder, taxgrtgroup, taxsubgrp
FROM legend
INNER JOIN mapunit ON mapunit.lkey = legend.lkey
LEFT OUTER JOIN component ON component.mukey = mapunit.mukey
WHERE legend.areasymbol = 'CA113'"

# run the query
res <- tibble(SDA_query(q))

datatable(res)


# define bounding box
b <- c(-120.54,38.61,-120.41,38.70)

# fetch the results, may take about 10 seconds
x <- mapunit_geom_by_ll_bbox(b)



# define bounding box
b <- c(-120.54,38.61,-120.41,38.70)
# query geometry
x <- mapunit_geom_by_ll_bbox(b) # about 20 seconds

# reset margins, and plot the results
par(mar=c(0,0,0,0))
plot(x)
# add our original bounding box in red
rect(b[1], b[2], b[3], b[4], border='red', lwd=2)
# add a title at the bottom of the figure
title(sub='SSURGO Map Unit Delineations', line=-2, font=2)

