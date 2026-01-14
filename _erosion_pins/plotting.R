#================================ Setup ================================

# libraries needed
libs <- c("devtools", "lidR", "shapefiles", "sf", "terra", "raster", "tidyr", "dplyr", "ggplot2", "easypackages", "spatialEco", "rasterVis", "raster", "elevatr", "mapview", "leaflet", "tmap", "RColorBrewer", "exactextractr", "emmeans", "lidR", "shapefiles", "sf", "terra", "raster", "tidyr", "dplyr", "ggplot2", "easypackages", "spatialEco","here", "performance", "see", "RColorBrewer", "lme4", "nlme", "readxl", "writexl", "emmeans", "splines", "lspline", "ggeffects", "lubridate", "cowplot", "gridGraphics", "broom")


# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
lapply(libs, library, character.only = T)

# enter the file path for the highest level folder you're working in 
data_folder <- "C:/Users/natha/Box/box_data/_data/_mapping/_topography/"

# when a file is needed, call the hert() function
# for example; data_frame = read.csv(hert("more_data/measurements_data.csv"))
hert <- function(file) {
  file_path = paste(data_folder, file, sep = "")
  return(file_path)
}

#================================ Leaflet Plotting ================================

# Input data
arb_dem <- rast(hert("ARB_topo/lidar.gdb"), "dem_1m_m")
lr_dem <- rast(hert("LR_topo/lidar.gdb"), "dem_1m_m")


# Create color palette
arb_dem.values = values(arb_dem)

pal <- colorNumeric(palette = c("transparent", "black"),
                          domain = arb_dem.values,
                          na.color = "transparent") # pallet for slope

# Plot
leaflet() %>% addTiles() %>% 
  addRasterImage(arb_dem,
                 colors = pal) %>% 
  addLegend("topright",
            pal = pal, 
            values = arb_dem.values,
            title = "values")


#================================ Clip Rastrers to AOI ================================
# 15T 451247 4967387 
# Input coordinates from the top left point 
point_one = 451247
point_two = 4967387
 
xmin <- point_one; xmax <- point_one + 150
ymin <- point_two - 150; ymax <- point_two

new_ext <- ext(xmin, xmax,
               ymin, ymax)

arb_clip <- crop(arb_dem, new_ext, snap = "out")

# Plot
leaflet() %>% addTiles() %>% 
  addRasterImage(arb_clip,
                 colors = pal,
                 opacity = .95) %>% 
  addLegend("topright",
            pal = pal, 
            values = arb_dem.values,
            title = "values")



#================================ Clip Rastrers to AOI ================================


# Install special libraries
devtools::install_github("tylermorganwall/rayshader") # require RTools to be installed
library(rayshader)


#And convert it to a matrix:
elmat = raster_to_matrix(arb_dem)

#We use another one of rayshader's built-in textures:
elmat |>
  sphere_shade(texture = "desert") |>
  plot_map()

#detect_water and add_water adds a water layer to the map:
elmat |>
  sphere_shade(texture = "desert") |>
  add_water(detect_water(elmat), color = "desert") |>
  plot_map()

#And here we add an ambient occlusion shadow layer, which models
#lighting from atmospheric scattering:
elmat |>
  sphere_shade(texture = "imhof1") |>
  add_shadow(ray_shade(elmat), 0.5) |>
  add_shadow(ambient_shade(elmat, maxsearch = 30), 0) |>
  plot_map()





# Plot 3D
elmat |>
  sphere_shade(sunangle = 20, texture = "imhof1") |>
  add_shadow(ray_shade(elmat, zscale = 0.2), 0.3) |>
  add_shadow(ambient_shade(elmat), 0) |>
  plot_3d(
    elmat,
    zscale = 0.4,
    fov = 0,
    theta = 60, # spins model
    zoom = 0.75,
    phi = 45,
    windowsize = c(1000, 800)
  )

# Changes camera direction
#render_camera(fov = 0, theta = 60, zoom = 0.75, phi = 45) # compass

# Render scalebar
# render_scalebar(
#   limits = c(0, 5, 10),
#   label_unit = "m",
#   position = "W",
#   #y = 50,
#   scale_length = c(0.33, 1)
# )

render_compass(position = "N")

render_points(
  extent = attr(elmat, "extent"),
  lat = 44.858068,
  long = -93.616433,
  altitude = 273.410807,
  zscale = 50,
  size = 3,
  color = "red",
  clear_previous = TRUE
)

render_snapshot()
