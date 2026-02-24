#================================ Setup ================================

# libraries needed
libs <- c("lidR", "shapefiles", "sf", "terra", "raster", "tidyr", "dplyr", "ggplot2", "easypackages", "spatialEco","here", "performance", "see", "RColorBrewer", "lme4", "nlme", "readxl", "writexl", "emmeans", "splines", "lspline", "ggeffects", "lubridate", "cowplot", "gridGraphics", "broom", "DT", "flextable")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
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

#================================ Site Descriptions ================================

# Create the data frame
site_table <- tribble(
  ~"Location", ~"Site", ~"Earthworms", ~"Par-Sci", ~"Soil Series", ~"Total Rainfall (cm)",
  
  "Lake Rebecca Park Reserve", "LRW", "European", "N", "Lester-Kilkenny complex", "4.96",
  
  "", "LRJ", "Jumping worm", "N", "Lester loam", "4.96",
  
  "", "LRE", "European", "N", "Lester loam", "4.96",
  
  "Minnesota Landscape Arboretum", "MAG", "Jumping worm", "N", "Lester-Kilkenny complex", "4.82",
  
  "", "ASH", "European", "N", "Lester-Kilkenny complex", "4.82",
  
  "", "WD", "Jumping worm", "N", "Lester-Kilkenny complex", "4.82",
  
  "Plummer House", "PLH", "Jumping worm", "Y", "Marlean silty clay loam", "5.33",
  
  "Indian Heights Park", "IH", "European", "Y", "Marlean silty clay loam", "5.33",
  
  "Northern Heights Park", "NH", "Jumping worm", "Y", "Marlean silty clay loam", "4.90",
  
  "Lake Minnetonka Regional Park", "LME", "European", "Y", "Lester loam", "4.69",
  
  "", "LMJ", "Jumping worm", "Y", "Lester-Malardi complex", "4.69",
  
  "Riley Creek Conservation Area", "RCE", "European", "Y", "Lester-Ridgeton complex", "5.39",
  
  "", "RCJ", "Jumping worm", "Y", "Suckercreek fine sandy loam", "5.39"
)

# Build the flextable
ft <- flextable(site_table) %>%
  autofit() %>%
  font(part = "all", fontname = "Calibri") %>% 
  fontsize(part = "all", size = 11) %>% 
  
  align(align = "left", part = "all") %>% 
  valign(valign = "center", part = "header")
  
ft
