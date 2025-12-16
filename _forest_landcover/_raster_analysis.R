#'############################### [setup] ################################

# libraries needed
libs <- c("lidR", "shapefiles", "sf", "terra", "raster", "tidyr", "dplyr", "ggplot2", "easypackages", "spatialEco", "rasterVis", "raster", "elevatr", "mapview", "leaflet", "tmap", "RColorBrewer", "exactextractr", "emmeans", "lidR", "shapefiles", "sf", "terra", "raster", "tidyr", "dplyr", "ggplot2", "easypackages", "spatialEco","here", "performance", "see", "RColorBrewer", "lme4", "nlme", "readxl", "writexl", "emmeans", "splines", "lspline", "ggeffects", "lubridate", "cowplot", "gridGraphics", "broom")

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
#  ----
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

landcover <- classify(landcover, cbind(255, 0)) # remove weird 255 land cover classes made by cropping

plot(landcover)

# import feature classes
huc12 <- vect(hert("_spatial/Great_Lakes_datasets.gdb"), layer = "GL_HUC12")
states <- vect(hert("_spatial/Great_Lakes_datasets.gdb"), layer = "GL_state_boundaries")


#'############################### [data cleanup] ################################

#' reproject, resample, and trim extent to match [landcover]
slope <- project(slope, landcover) %>% # reproject slope to match landcover
  resample(landcover, method = "bilinear")  # resample slope to match resolution

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

#'############################### [rusle2 per land cover] ################################
# Section 1 -------------------



# input the raster needed for statics here
landcover <- landcover # call landcover
raster.in <- rusle2 # call slope raster

#' [TERRA][simple] ---------------------------------------------------------------
# calculate a quick mean, median, max, and min for each land cover

landcover.int <- as.int(landcover) # land cover needs to be an integer, not categorical

zonal.stats <- data.frame("landcover" = c("NoData",
                                          "1 Temperate or Subpolar Needleaf Forest",
                                          "5 Temperate or Subpolar Broadleaf Deciduous Forest",
                                          "6 Mixed Forest",
                                          "8 Temperate or Subpolar Shrubland",
                                          "10 Temperate or Subpolar Grassland",
                                          "14 Wetland",
                                          "15 Cropland",
                                          "16 Barren Land",
                                          "17 Urban and Built-up",
                                          "18 Water"),
                          "count" = freq(landcover.int)[,3],
                          "mean" = terra::zonal(raster.in, landcover.int, "mean", na.rm = TRUE)[,2],
                          "median" = terra::zonal(raster.in, landcover.int, "median", na.rm = TRUE)[,2],
                          "max" = terra::zonal(raster.in, landcover.int, "max", na.rm = TRUE)[,2],
                          "min" = terra::zonal(raster.in, landcover.int, "min", na.rm = TRUE)[,2],
                          "sum" = terra::zonal(raster.in, landcover.int, "sum", na.rm = TRUE)[,2]) %>% 
  drop_na()


tibble(zonal.stats) # print

write.csv(zonal.stats, hert("_analysis/rusle2-landcover-stats.csv")) # export to excel



#' [TERRA][histograms] ---------------------------------------------------------------
# generate histograms for each landcover ----

# define rusle2 bins
start = -1 # start of bins
end = 10 # end of bins
width = .1 # bin spacing

# create a matrix of the bin distributions
bins <- data.frame(A = seq(from = start, to = end - width, by = width),
                   B = seq(from = start - -width, to = end, by = width), #check the sign in "from"
                   "bin" = seq(from = start, to = end - width, by = width))
slope.bins <- as.matrix(bins)


slope.binned <- classify(raster.in, slope.bins) # apply bins to slope raster

# tabulate area - how many unique combinations are there, and how many cells are in each?
stack <- c(landcover, slope.binned) # stack land cover and binned slope raseters
stack.area <- terra::crosstab(stack, long = TRUE, useNA = TRUE) # tabulate

stack.area.t <- stack.area %>%
  drop_na() %>% 
  group_by(land_cover) %>% 
  mutate(nsum = sum(n)) %>% 
  ungroup() %>% 
  mutate(frac = n / nsum)


# plot bins for each land cover
ggplot(data = stack.area.t, mapping = aes(y = frac, x = GL_RUSLE2_30m_raster_r)) +
  geom_col() +
  scale_x_continuous(limits = c(-1, 10)) +
  facet_wrap(~land_cover, ncol = 2, scales = "free_y") +
  geom_vline(xintercept = -0.534) # mean of... cropland


# export
write.csv(stack.area, hert("_analysis/rusle2-landcover-hist.csv")) # export df to plot later





#'############################### [slope per land cover] ################################

# input the raster needed for statics here
landcover <- landcover # landcover is also needed
raster.in <- slope # call slope raster


#' [TERRA][simple] ---------------------------------------------------------------
# calculate a quick mean, median, max, and min for each land cover

landcover.int <- as.int(landcover) # land cover needs to be an integer, not categorical

zonal.stats <- data.frame("landcover" = c("NoData",
                                          "1 Temperate or Subpolar Needleaf Forest",
                                          "5 Temperate or Subpolar Broadleaf Deciduous Forest",
                                          "6 Mixed Forest",
                                          "8 Temperate or Subpolar Shrubland",
                                          "10 Temperate or Subpolar Grassland",
                                          "14 Wetland",
                                          "15 Cropland",
                                          "16 Barren Land",
                                          "17 Urban and Built-up",
                                          "18 Water",
                                          "NA"),
                          "count" = freq(landcover.int)[,3],
                          "mean" = terra::zonal(raster.in, landcover.int, "mean", na.rm = TRUE)[,2],
                          "median" = terra::zonal(raster.in, landcover.int, "median", na.rm = TRUE)[,2],
                          "max" = terra::zonal(raster.in, landcover.int, "max", na.rm = TRUE)[,2],
                          "min" = terra::zonal(raster.in, landcover.int, "min", na.rm = TRUE)[,2]) %>% 
  drop_na()


tibble(zonal.stats) # print

write.csv(zonal.stats, hert("_analysis/slope-landcover-stats.csv")) # export to excel




#' [TERRA][histograms] ---------------------------------------------------------------
# generate histograms for each landcover

# define slope bins
start = 0 # start of bins
end = 100 # end of bins
width = 1 # bin spacing

# create a matrix of the bin distributions
bins <- data.frame(A = seq(from = start, to = end - width, by = width),
                     B = seq(from = start - -width, to = end, by = width), #check the sign in "from"
                     "bin" = seq(from = start, to = end - width, by = width))
slope.bins <- as.matrix(bins)


slope.binned <- classify(raster.in, slope.bins) # apply bins to slope raster

# tabulate area - how many unique combinations are there, and how many cells are in each?
stack <- c(landcover, slope.binned) # stack land cover and binned slope raseters
stack.area <- terra::crosstab(stack, long = TRUE, useNA = TRUE) # tabulate

stack.area.t <- stack.area %>%
  drop_na() %>% 
  group_by(land_cover) %>% 
  mutate(nsum = sum(n)) %>% 
  ungroup() %>% 
  mutate(frac = n / nsum)


# plot bins for each land cover
ggplot(data = stack.area.t, mapping = aes(y = frac, x = GL_USGS30m_slope_r)) +
  geom_col() +
  scale_x_continuous(limits = c(0, 50)) +
  facet_wrap(~land_cover, ncol = 2, scales = "free_y") +
  geom_vline(xintercept = -0.534) # mean of... cropland


# export
write.csv(stack.area, hert("_analysis/slope-landcover-hist.csv")) # export df to plot later


#' [TERRA][slope bins table] ---------------------------------------------------------------
# generate tables and  for each landcover


# define slope bins & create a maxtrix of the bin distribtions
bins <- data.frame(A = c(0, 2, 5, 15, 30, 50), # "left" bin side
                   B = c(2, 5, 15, 30, 50, 100), # "right" bin side
                   C = c(1, 2, 3, 4, 5, 6))
slope.bins <- as.matrix(bins)

slope.binned <- classify(raster.in, slope.bins) # apply bins to slope raster

# apply proper slope bin titles
slope.cats <- data.frame(
  ID = c(1, 2, 3, 4, 5, 6),
  slope_class = c("0 - 2", "02  - 5", "05 - 15", "15 - 30", "30 - 50", "50 - 100")
)
levels(slope.binned) <- slope.cats # Associate the data frame with the raster



# tabulate area - how many unique combinations are there, and how many cells are in each?
stack <- c(landcover, slope.binned) # stack land cover and binned slope raseters
stack.area <- terra::crosstab(stack, long = TRUE, useNA = TRUE) # tabulate

stack.sum <- stack.area %>%
  drop_na() %>% 
  group_by(land_cover) %>% 
  mutate(nsum = sum(n)) %>% 
  ungroup() %>% 
  mutate(frac = n / nsum * 100)

stack.vis <- stack.sum %>%
  select(-n, -nsum) %>% 
  pivot_wider(names_from = slope_class,
              values_from = frac)

# export
write.csv(stack.vis, hert("_analysis/slope-landcover-bins.csv")) # export clean table for manuscript
write.csv(stack.sum, hert("_analysis/slope-landcover-bins-plots.csv")) # export df to plot later


# plot bins for each land cover
ggplot(data = stack.sum, mapping = aes(y = frac, x = slope_class)) +
  geom_col() +
  facet_wrap(~land_cover, ncol = 2, scales = "free_y") +
  geom_vline(xintercept = -0.534) # mean of... cropland




#' [TERRA / LEAFLET][plotting / visualization] ---------------------------------------------------------------
# for slope. this can be run after either the histogram or table chunk above

# sample the raster to reduce size
factor = 1000000 # factor to sample down to

lc.sample <- spatSample(landcover, size = factor, method = "regular", as.raster = TRUE)
slope.sample <- spatSample(slope, size = factor, method = "regular", as.raster = TRUE)

# establish parameters 
landcover.classes <- levels(landcover)[[1]]$land_cover
landcover.values  <- levels(landcover)[[1]]$ID
landcover.colors <- c("grey","darkgreen", "palegreen4", "lightgreen", "cadetblue3", "lightcyan", "darkblue", "lightgoldenrod", "brown", "grey42", "blue")

slope.values <- values(slope.sample) # slope sample to improve performance

# #normalize slope to [0,1] for alpha
alpha.values <- scales::rescale(slope.values, to = c(0.2, 1.0)) # Scale from 0.2 to 1.0
# alpha.values <- (slope.values - min(slope.values, na.rm = TRUE)) /
#   (max(slope.values, na.rm = TRUE) - min(slope.values, na.rm = TRUE)) # idk if this is needed

#--------------------

r = slope.sample
values = values(slope.sample)
pal_func <- colorNumeric(c("red", "blue"), values(r), na.color = "transparent")

opacity_values <- scales::rescale(values(r), to = c(0.2, 1.0)) # Scale from 0.2 to 1.0
color_vector <- pal_func(values(r))



#---------------------

# create color pallets
pal <- colorFactor(palette = landcover.colors,
                   domain  = landcover.values,
                   na.color = "transparent") # pallet for landcover


pal.slope <- colorNumeric(palette = c("transparent", "black"),
                          domain = slope.values,
                          na.color = "transparent") # pallet for slope

#' [plot only landcover]
leaflet() %>% addTiles() %>% 
  addRasterImage(lc.sample,
                 colors = pal,
                 opacity = alpha.values) %>% 
  addLegend("topright",
            pal = pal, 
            values = landcover.values,
            title = "Landcover",
            labFormat = labelFormat(transform = function(x) {
              landcover.classes[match(x, landcover.values)]
            }))


#' [plot landcover, opacity for slope]
leaflet() %>% addTiles() %>% 
  addRasterImage(lc.sample,
                 colors = pal,
                 opacity = 0.7) %>% 
  addRasterImage(slope.sample,
                 colors = pal.slope,
                 opacity = slope.values) %>% 
  addLegend("topright",
            pal = pal, 
            values = landcover.values,
            title = "Landcover",
            labFormat = labelFormat(transform = function(x) {
              landcover.classes[match(x, landcover.values)]
            }))

# ploting messing

lc.sl <- c(lc.sample, slope.sample)

leaflet() %>% addTiles() %>% 
  addRasterImage(lc.sl,
                 opacity = slope.sample)

# #' [plotting in ggplot]

# aggregate
#aggregate(l, fact = 9, fun = "mean")

# # build a df for ggplot 
# lc.slope <- as.data.frame(c(landcover, slope), xy = TRUE)
# 
# tibble(lc.slope)
# 
# ggplot(data = lc.slope, mapping = aes(x, y, fill = land_cover, alpha = GL_USGS30m_slope_r)) +
#   geom_raster()



# # create a combined raster where the thousands and hundreds place represent land cover and the tens and ones rep slope
# combo <- landcover * 100 + slope.binned
# 
# # # replace some NAs
# # na.remov <- cbind(NA, 999)
# # combo <- classify(combo, na.remov)
# 
# # create a data frame of unique combinations
# cats <- data.frame(landcover = stack.area[, 1],
#                    slopeclass = stack.area[, 2]) %>% 
#   unique() %>% 
#   drop_na() %>% 
#   mutate(id = landcover * 100 + slopeclass) %>% 
#   select(id, landcover, slopeclass)
# 
# # essentially provide categories
# levels(combo) <- cats

#' [DPLYR][complicated stats] ---------------------------------------------------------------
# stats per landcover

# getting mean, confidence intervals, and other stats for each land cover
lc.slope <- data.frame(landcover = values(landcover),
                          raster.in = values(raster.in)) %>%
  mutate(.keep = "unused",
         raster.in = GL_USGS30m_slope_r,
         landcover = case_when(
           land_cover == 0 ~ NA,
           land_cover == 1 ~ "1 Temperate or Subpolar Needleaf Forest",
           land_cover == 5 ~ "5 Temperate or Subpolar Broadleaf Deciduous Forest",
           land_cover == 6 ~ "6 Mixed Forest",
           land_cover == 8 ~ "8 Temperate or Subpolar Shrubland",
           land_cover == 10 ~ "10 Temperate or Subpolar Grassland",
           land_cover == 14 ~ "14 Wetland",
           land_cover == 15 ~ "15 Cropland",
           land_cover == 16 ~ "16 Barren Land",
           land_cover == 17 ~ "17 Urban and Built-up",
           land_cover == 18 ~ NA, # removes "18 Water"
           TRUE ~ NA
         )) %>% 
  drop_na() # drops rows where any column has NA


# fitting a lm using means coding. each estimate is the mean of that landcover's log(slope)
lm <- lm(data  = lc.slope, raster.in ~ landcover - 1)
lm.stats <- tidy(lm)


# getting other stats, mean etc
slope.stats <- lc.slope %>% 
  group_by(landcover) %>% 
  summarise(mean = mean(raster.in),
            median = median(raster.in),
            min = min(raster.in),
            max = max(raster.in),
            sd = sd(raster.in))

#' [products]
slope.stats; lm.stats

#' plotting histograms from tables [unsure if this will work with large data sizes]
ggplot(data = lc.slope, aes(x = raster.in)) +
  geom_histogram(binwidth =  0.5) +
  facet_wrap(~landcover, scale = "free_y", ncol = 2) +
  geom_vline(xintercept = mean.raster.in) # mean of total pop

#' binning slope data
lc.slope.binned <- lc.slope %>% 
  mutate(slope.class = case_when( # bottom is inclusive top is not
    raster.in < 2 ~ "<2%",
    raster.in >= 2 & raster.in < 5 ~ "2 - 5",
    raster.in >= 5 & raster.in < 10 ~ "5 - 10",
    raster.in >= 10 & raster.in < 25 ~ "10 - 25",
    raster.in >= 25 & raster.in < 50 ~ "25 - 50",
    raster.in >= 50 ~ ">50",
    TRUE ~ NA
  )) %>% 
  mutate(slope.class = factor(slope.class, levels=c("<2%", "2 - 5", "5 - 10", "10 - 25", "25 - 50", ">50")))


binned.slope.stats <- lc.slope.binned %>% 
  group_by(landcover, slope.class) %>% 
  summarise(count = n()) %>% 
  mutate(total = sum(count),
         frac = count / total * 100,
         .keep = "unused") %>% 
  pivot_wider(
    names_from = slope.class,
    values_from = frac
  )



#pairs(emmeans(lm, "landcover"), adjust = "none") # this provides hugely inflated p-values because of high degrees of spatial autocorrelation.
# # fitting a lm, estimates are difference between the overall mean and the landcover class mean 
# mean.raster.in <- mean(lc.slope$raster.in)
# lm.sum <- lm(data  = lc.slope, (raster.in - mean.raster.in) ~ landcover - 1) # omits last catagory
# tidy(lm.sum)




#'############################### [RUSLE2 per land cover] ################################












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

