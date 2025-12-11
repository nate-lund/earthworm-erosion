#'############################### [setup] ################################

# libraries needed
libs <- c("lidR", "shapefiles", "sf", "terra", "raster", "tidyr", "dplyr", "ggplot2", "easypackages", "spatialEco", "rasterVis", "raster", "elevatr", "mapview", "leaflet", "tmap", "RColorBrewer", "exactextractr")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
lapply(libs, library, character.only = T)

# enter the file path for the highest level folder you're working in 
data_folder <- "C:/Users/natha/Box/box_data/_data/_forest-landcover-erosion/"

# when a file is needed, call the hert() function
# for example; data_frame = read.csv(hert("more_data/measurements_data.csv"))
hert <- function(file) {
  file_path = paste(data_folder, file, sep = "")
  return(file_path)
}

#'############################### [Resources] ################################

# Technical resources
# sf manual: <https://cran.r-project.org/web/packages/sf/sf.pdf>
# lidR manual: <https://r-lidar.github.io/lidRbook/>
# shapefiles manual: <https://cran.r-project.org/web/packages/shapefiles/shapefiles.pdf>
# MNtopo <https://files.dnr.state.mn.us/aboutdnr/gis/mntopo/mntopo_help_document.pdf>
# EPSG info: <https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf>
# SF: https://r-spatial.github.io/sf/articles/sf1.html#reading-and-writing

# Elevation, slope, and curvature can be extracted from Minnesota LiDAR data found at MNTopo (<http://arcgis.dnr.state.mn.us/maps/mntopo/>). The raw LiDAR data can be downloaded directly from the website. LiDAR data is in NAD83 w/ elevation in meters.

#'############################### [importing spatial data] ################################

# import rasters
dem <- rast(hert("_spatial/Great_Lakes_datasets.gdb"), "GL_USGS30m_DEM_r")
slope <- rast(hert("_spatial/Great_Lakes_datasets.gdb"), "GL_USGS30m_slope_r")
rusle2 <- rast(hert("_spatial/Great_Lakes_datasets.gdb"), "GL_RUSLE2_30m_raster_r")
landcover <- rast(hert("_spatial/Great_Lakes_datasets.gdb"), "GL_LandCover_30m_2020_raster")

#' [temporary change to make data processing faster]
ext <- ext(landcover)
xmin <- ext[1]; xmax <- ext[2]
ymin <- ext[3]; ymax <- ext[4]

new_ext <- ext(1100000, 1200000,
             5150000, 5200000)

landcover <- crop(landcover, new_ext, snap = "out") 

plot(landcover)

# import feature classes
huc12 <- vect(hert("_spatial/Great_Lakes_datasets.gdb"), layer = "GL_HUC12")
states <- vect(hert("_spatial/Great_Lakes_datasets.gdb"), layer = "GL_state_boundaries")


#'############################### [data cleanup] ################################

#' reproject, resample, and trim extent to match [landcover]
slope <- project(slope, landcover) %>% # reproject slope to match landcover
  resample(landcover, method = "bilinear") # resample slope to match resolution

rusle2 <- project(rusle2, landcover) %>% # reproject slope to match landcover
  resample(landcover, method = "bilinear") # resample slope to match resolution

# RUSLE2 data needs to be divieded by 10,000 to be accurate
rusle2 <- rusle2 / 10000

# assigning proper landcover types
categories <- data.frame(
  ID = c(0, 1, 5, 6, 8, 10, 14, 15, 16, 17, 18),
  land_cover = c("NoData",
                 "1 Temperate or Subpolar Needleaf Forest",
                 "5 Temperate or Subpolar Broadleaf Deciduous Forest",
                 "6 Mixed Forest",
                 "8 Temperate or Subpolar Shrubland",
                 "10 Temperate or Subpolar Grassland",
                 "14 Wetland",
                 "15 Cropland",
                 "16 Barren Land",
                 "17 Urban and Built-up",
                 "18 Water")
)

levels(landcover) <- categories # Associate the data frame with the raster

# visualize to verify

plot(landcover)
plot(slope)
plot(rusle2)


# change format of feature classes for plotting and project to WGS 84 (EPSG:4326) assuming current CRS is EPSG:26918 (NAD83 / UTM zone 18N)
huc12.sf <- st_as_sf(huc12)
huc12.84 <- st_as_sf(huc12) %>% st_transform(crs = 4326)

states.sf <- st_as_sf(states)
states.84 <- st_as_sf(states) %>% st_transform(crs = 4326)



#'############################### [slope per land cover] ################################

log.slope <- log(slope) # make log(slope) raster

#' [simple] ---------------------------------------------------------------
# calculate a quick mean slope for each land cover
landcover.int <- as.int(landcover) # land cover needs to be an integer, not categorical

zonal_stats <- terra::zonal(slope, landcover.int,
                            "mean", na.rm = TRUE)

print(tibble(zonal_stats))


#' [complicated stats] ---------------------------------------------------------------
# getting mean, confidence intervals, and other stats for each land cover

lc.slope <- data.frame(landcover = values(landcover),
                          log.slope = values(log.slope))

stats <- lc.slope %>%
  group_by(land_cover) %>%
  summarise(mean = mean(GL_USGS30m_slope_r, na.rm = TRUE),
            sd = sd(GL_USGS30m_slope_r, na.rm = TRUE),
            count = length(GL_USGS30m_slope_r)) %>% 
  mutate(se = sd / (count - 1),
         crit = qt(p = 0.975, df = count - 1),
         me = crit * se)


#' [plotting and for tables] ---------------------------------------------------------------
# more detailed stats require additional functions

# define slope bins (0–5, 5–15, 15–30, 30-100)
slope.bins <- matrix(c(0 , 2, 1,
                       3 , 5, 2,
                       6 , 10, 3,
                       11 , 20, 4,
                       21 , 30, 5,
                       30, 50, 6,
                       50, 100, 7), ncol=3, byrow = TRUE)

slope.binned <- classify(slope, slope.bins) # apply bins to slope raster, convert to integer


# tabulate area - how many unique combinations are there, and how many cells are in each?
stack <- c(landcover, slope.binned) # stack land cover and binned slope raseters
stack_area <- terra::crosstab(stack, long = TRUE, useNA = FALSE) # tabulate

stack_area.t <- stack_area %>%
  group_by(land_cover) %>% 
  mutate(nsum = sum(n)) %>% 
  ungroup() %>% 
  mutate(frac = n / nsum)

# plot bins for each land cover
ggplot(data = stack_area.t, mapping = aes(y = frac, x = GL_USGS30m_slope_r)) +
  geom_col() +
  facet_wrap(~land_cover, ncol = 2, scales = "free_y")


#' [plotting and for tables] ---------------------------------------------------------------
# below is the same as above but for log(slope)


# define slope bins
l.bins <- data.frame(A = seq(from = -13, to = 4.5, by = 0.5),
                     B = seq(from = -12.5, to = 5, by = 0.5),
                     "name" = seq(from = -13, to = 4.5, by = 0.5)) # this col is the name of each bin
l.slope.bins <- as.matrix(l.bins)


l.slope.binned <- classify(log.slope, l.slope.bins) # apply bins to slope raster, convert to integer


# tabulate area - how many unique combinations are there, and how many cells are in each?
l.stack <- c(landcover, l.slope.binned) # stack land cover and binned slope raseters
l.stack_area <- terra::crosstab(l.stack, long = TRUE, useNA = TRUE) # tabulate

l.stack_area.t <- l.stack_area %>%
  group_by(land_cover) %>% 
  mutate(nsum = sum(n)) %>% 
  ungroup() %>% 
  mutate(frac = n / nsum)


# plot bins for each land cover
ggplot(data = l.stack_area.t, mapping = aes(y = frac, x = GL_USGS30m_slope_r)) +
  geom_col() +
  facet_wrap(~land_cover, ncol = 2, scales = "free_y") +
  geom_vline(xintercept = 1)









#'############################### [plotting by HUC8] ################################

#' [learning masks etc.]

# LANDCOVER

# filter landcover for temperate braodleaf and mixed forests  
all.forestcover <- classify(landcover, rbind(c(5,1), c(6,1)), others = 0) # creates a binary mask for landcover data with classes 5 & 6

# rename filtered class to...
categories2 <- data.frame(
  ID = c(0, 1),
  land_cover = c("Other",
                 "Temperate or Subpolar Broadleaf Deciduous Forest & Mixed Forest")
)

levels(all.forestcover) <- categories2 # Associate the data frame with the raster

plot(all.forestcover)
plot(huc12)

# extract raster data to hucs, summing all 1's.
fc.hucs <- terra::extract(forestcover.mask, huc12, fun = sum, method="simple", na.rm = TRUE) # this takes a very long time

huc12$forest_sum <- fc.hucs[,2] # Merge back into sf

# plotting
pal = colorNumeric("Greens", huc12$forest_sum)

# Map
leaflet((st_as_sf(huc12) %>% st_transform(crs = 4326))) %>%
  addPolygons(color = "white",
              fillColor = ~pal,
              fillOpacity = 0.7,
              weight = 1) 
  addLegend(pal = pal, values = values(forest_sum), title = "Landcovers") 


# calculate polygone area
huc12$area_m2 <- expanse(huc12, unit = "m")
huc12$area_km2 <- expanse(huc12, unit = "km")

# normalize forest sums
# Assuming hhucs has a column 'forestcover' with summed values
huc12$forest_sum <- fc.hucs$forestcover

# Normalize by area (density per km²)
huc12$forest_density <- huc12$forest_sum / huc12$area_km2



# map 2
leaflet(huc12.sf) %>%
  addPolygons(color = "white",
              fillColor = ~colorNumeric("Greens", forest_density)(forest_density),
              fillOpacity = 0.7,
              weight = 1,
              popup = ~paste0("Density: ", round(forest_density, 2), " per km²"))


head(hhucs)







# SLOPE / RUSLE

# use this to summarize slope... or sum sedimetn delivery
# zonal(forestcover, states, fun = mean, as.raster=TRUE)

?

?zonal

#' [plotting]

plot.fc <- aggregate(forestcover, fact = 9, fun = "mean")

pal <- colorNumeric(c("blue","red"), values(forestcover),
                    na.color = "transparent")

leaflet() %>% addTiles() %>% 
  addRasterImage(colors = pal, 
                 plot.fc, 
                 opacity = 0.9) %>% 
  addPolygons(data = states.sf,
              color = "white", weight = 1.0, smoothFactor = 0.5,
              opacity = 0.25, fillOpacity = 0.15,) %>% 
  addLegend(pal = pal, values = values(plot.fc), title = "Landcovers") 
  





# creating a mask 
mask_tf <- slope > 20 # creates a logical mask
mask_num <- classify(mask_tf, rbind(c(0,0), c(1,1))) # creates a numerical (0/1) mask

plot(mask_tf)
plot(mask_num)

high_slope <- mask(slope, mask_num, maskvalue = 0) # take the values of slope, except for the values that are maskvalue = in mask_num, replaced with NA (or updatevalue = )

plot(high_slope)

hist(high_slope)

?mask

summary(high_slope)



#'############################### [old things to reference] ################################




# load laz fliles from arb (in NAD83). filter for only ground-classified points (-keep_class 2)
arb_las1 <- readLAS("C:/Users/natha/Documents/local_data/_data/_mapping/_arb_topo/_lidar/4342-05-13.laz", filter = "-keep_class 2")
arb_las2 <- readLAS("C:/Users/natha/Documents/local_data/_data/_mapping/_arb_topo/_lidar/4342-05-12.laz", filter = "-keep_class 2")

# load laz files from lr
lr_las1 <- readLAS("C:/Users/natha/Documents/local_data/_data/_mapping/_lake_rebecca_topo/_lidar/3542-30-08.laz", filter = "-keep_class 2")
lr_las2 <- readLAS("C:/Users/natha/Documents/local_data/_data/_mapping/_lake_rebecca_topo/_lidar/3542-30-09.laz", filter = "-keep_class 2")
lr_las3 <- readLAS("C:/Users/natha/Documents/local_data/_data/_mapping/_lake_rebecca_topo/_lidar/3542-31-08.laz", filter = "-keep_class 2")
lr_las4 <- readLAS("C:/Users/natha/Documents/local_data/_data/_mapping/_lake_rebecca_topo/_lidar/3542-31-09.laz", filter = "-keep_class 2")

# Transform coordinate reference system to NAD83 zone 15N (ESPG 26915). Fit a digital terrain model to the data using the triangular irregular network algorithm (not as computationally intense, default settings). Terrain function (<https://www.rdocumentation.org/packages/raster/versions/3.6-32/topics/terrain>)

# transform to NAD83 zone 15N (ESPG 26915)
epsg(arb_las1) # report the current coordinate system
st_crs(arb_las1) <- 26915 # set to  NAD83 zone 15N
st_crs(arb_las2) <- 26915
st_crs(lr_las1) <- 26915
st_crs(lr_las2) <- 26915
st_crs(lr_las3) <- 26915
st_crs(lr_las4) <- 26915

# fit DEM using triangular irregular network, this is similar to LAStools stuff too
arb_dem1 <- rasterize_terrain(arb_las1, res = 1, algorithm = tin())
arb_dem2 <- rasterize_terrain(arb_las2, res = 1, algorithm = tin())

lr_dem1 <- rasterize_terrain(lr_las1, res = 1, algorithm = tin())
lr_dem2 <- rasterize_terrain(lr_las2, res = 1, algorithm = tin())
lr_dem3 <- rasterize_terrain(lr_las3, res = 1, algorithm = tin())
lr_dem4 <- rasterize_terrain(lr_las4, res = 1, algorithm = tin())


# merge DEMS
arb_dem <- mosaic(arb_dem1, arb_dem2)

lr_dem <- mosaic(lr_dem1, lr_dem2, lr_dem3, lr_dem4)

# Create new rasters using dems for slope and curvature. Slope is in degrees. Curvature is calculated in the direction of maximum slope (profile). Positive values indicate upward covex.

# slope (in degrees)
arb_slope <- terrain(arb_dem, "slope", unit = "degrees")
lr_slope <- terrain(lr_dem, "slope", unit = "degrees")

# curvature
arb_curvature <- curvature(arb_dem, type = "profile")
lr_curvature <- curvature(lr_dem, type = "profile")


# export raster arb
terra::writeRaster(arb_dem, filetype = "GTiff", filename = file.path("C:/Users/natha/Documents/local_data/_data/_mapping/_arb_topo/arb_1m_dem"), overwrite=TRUE)

# import
lr_1m_dem <- rast("C:/Users/natha/Documents/local_data/_data/_mapping/_lake_rebecca_topo/lr_1m_dem")

#Crop the raster
cropped_raster <- crop(arb_1m_dem, extent(c(min(arb_geo_data$latitude) + 500,
                                            max(arb_geo_data$latitude) + 500,
                                            min(arb_geo_data$longitude) + 500,
                                            max(arb_geo_data$longitude + 500))))


# plotting


# format dem
arb_dem_map <- as.data.frame(arb_1m_dem, xy = TRUE) %>% 
  rename("elevation" = "Z")

ggplot(data = arb_dem_map) +
  geom_tile(mapping = aes(x = x, y = y, fill = elevation)) +
  scale_fill_viridis_c() +
  labs(title = "Digital Elevation Model", fill = "Elevation") +
  coord_equal() +
  theme_minimal()

