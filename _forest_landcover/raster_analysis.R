#================================ Setup ================================

# libraries needed
libs <- c("tidyverse", "lidR", "shapefiles", "sf", "terra", "raster", "tidyr", "dplyr", "ggplot2", "easypackages", "spatialEco", "rasterVis", "raster", "elevatr", "mapview", "leaflet", "tmap", "RColorBrewer", "exactextractr", "emmeans", "lidR", "shapefiles", "sf", "terra", "raster", "tidyr", "dplyr", "ggplot2", "easypackages", "spatialEco","here", "performance", "see", "RColorBrewer", "lme4", "nlme", "readxl", "writexl", "emmeans", "splines", "lspline", "ggeffects", "lubridate", "cowplot", "gridGraphics", "broom", "flextable", "officer")

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

#================================ Resources ================================

# Technical resources
# sf manual: <https://cran.r-project.org/web/packages/sf/sf.pdf>
# lidR manual: <https://r-lidar.github.io/lidRbook/>
# shapefiles manual: <https://cran.r-project.org/web/packages/shapefiles/shapefiles.pdf>
# MNtopo <https://files.dnr.state.mn.us/aboutdnr/gis/mntopo/mntopo_help_document.pdf>
# EPSG info: <https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf>
# SF: https://r-spatial.github.io/sf/articles/sf1.html#reading-and-writing

# Elevation, slope, and curvature can be extracted from Minnesota LiDAR data found at MNTopo (<http://arcgis.dnr.state.mn.us/maps/mntopo/>). The raw LiDAR data can be downloaded directly from the website. LiDAR data is in NAD83 w/ elevation in meters.


#================================ Import Data  ================================

# import rasters
dem <- rast(hert("_spatial/Great_Lakes_datasets.gdb"), "GL_USGS30m_DEM_r")
slope <- rast(hert("_spatial/Great_Lakes_datasets.gdb"), "GL_USGS30m_slope_r_land_less100")
rusle2 <- rast(hert("_spatial/Great_Lakes_datasets.gdb"), "GL_RUSLE2_30m_raster_r")
landcover <- rast(hert("_spatial/Great_Lakes_datasets.gdb"), "GL_LandCover_30m_2020_raster")

#' #' [subset landcover for QAQC]
#' ext <- ext(landcover)
#' xmin <- ext[1]; xmax <- ext[2]
#' ymin <- ext[3]; ymax <- ext[4]
#' 
#' new_ext <- ext(1100000, 1200000,
#'              5150000, 5200000)
#' 
#' landcover <- crop(landcover, new_ext, snap = "out")
#' 
#' landcover <- classify(landcover, cbind(255, 0)) # remove weird 255 land cover classes made by cropping

# import feature classes
#huc12 <- vect(hert("_spatial/Great_Lakes_datasets.gdb"), layer = "GL_HUC12")
#states <- vect(hert("_spatial/Great_Lakes_datasets.gdb"), layer = "GL_state_boundaries")


#================================ Cleanup Data ================================

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
# huc12.sf <- st_as_sf(huc12)
# huc12.84 <- st_as_sf(huc12) %>% st_transform(crs = 4326)
# 
# states.sf <- st_as_sf(states)
# states.84 <- st_as_sf(states) %>% st_transform(crs = 4326)




#================================ Processing ================================


##================================ Build Slope Bins ================================

# input the raster needed
landcover.int <- landcover # landcover is also needed
raster.in <- slope # call slope raster



# define slope bins & create a maxtrix of the bin distribtions
bins <- data.frame(A = c(0, 2, 5, 15, 30, 50), # "left" bin side
                   B = c(2, 5, 15, 30, 50, 100), # "right" bin side
                   C = c(1, 2, 3, 4, 5, 6))  # bind IDs
slope.bins <- as.matrix(bins)

slope.binned <- classify(raster.in, slope.bins) # apply bins to slope raster



# apply proper slope bin titles
slope.cats <- data.frame(
  ID = c(1, 2, 3, 4, 5, 6),
  slope_class = c("0_2", "02_5", "05_15", "15_30", "30_50", "50_100")
)
levels(slope.binned) <- slope.cats # Associate the data frame with the raster

# tabulate area - how many unique combinations are there, and how many cells are in each
stack <- c(landcover, slope.binned) # stack land cover and binned slope raseters
stack.area <- terra::crosstab(stack, long = TRUE, useNA = TRUE) # tabulate

# For totals
slope_totals <- freq(slope.binned) %>% 
  mutate(frac = count / sum(count) * 100)


stack.sum <- stack.area %>%
  drop_na() %>% 
  group_by(land_cover) %>% 
  mutate(nsum = sum(n)) %>% 
  ungroup() %>% 
  mutate(lc_frac = n / nsum * 100)


# Pivot df for slope percent breakdown 
stack_slope <- stack.sum %>% 
  group_by(slope_class) %>% 
  mutate(slope_frac = lc_frac / sum(lc_frac, na.rm = TRUE) * 100) %>% 
  ungroup() %>% 
  arrange(slope_class) %>% 
  select(-n, -nsum, -lc_frac) %>% 
  pivot_wider(names_from = slope_class,
              values_from = slope_frac)


#' [compute mean slope for each landcover ]
slope_mean <- terra::zonal(raster.in, landcover.int, "mean", na.rm = TRUE)[,2]


# Pivot df for landcover percent breakdown
stack_lc <- stack.sum %>%
  select(-n, -nsum) %>% 
  pivot_wider(names_from = slope_class,
              values_from = lc_frac) %>% 
  mutate(mean_slope = slope_mean[2:10]) %>% 

    add_row(land_cover = "Overall",
          `0_2` = slope_totals[1,4],
          `02_5` = slope_totals[2,4],
          `05_15` = slope_totals[3,4],
          `15_30` = slope_totals[4,4],
          `30_50` = slope_totals[5,4],
          `50_100` = slope_totals[6,4],) # Add total row




# Visualize
tibble(stack_slope)
tibble(stack_lc)


# export
write.csv(stack_slope, hert("_analysis/02-26_stack_slopeBylandcover.csv")) # export clean table for manuscript
write.csv(stack_lc, hert("_analysis/02-26_stack_landcoverBySlope.csv")) # export df to plot later




##================================ Two-way Mean ================================

# Import data
slopebins <- slope.binned
lc <- landcover
erosion <- rusle2

# Create a combined category raster. This encodes each landcover × slope combination uniquely.
multiplex <- lc * 1000 + slopebins # Creat a raster with a uniqe ID for each slope class - landcover combo
names(multiplex) <- "multiplex"


# Compute MEAN erosion value of each slope-landcover combo
two_way_mean <- zonal(erosion, multiplex, fun = "mean", na.rm = TRUE) # I think the na.rm here drops the NA landcover and water

# Seperate out multiplex
mtwo_way <- two_way_mean %>% 
  mutate(
    landcover   = multiplex %/% 1000,
    slope_class = multiplex %% 1000
  ) %>% 
  rename(mean_erosion = GL_RUSLE2_30m_raster_r)

# Pivot
mtwo_way_wide <- mtwo_way %>% 
  select(-multiplex) %>% 
  arrange(landcover, slope_class) %>%
  pivot_wider(
    names_from = slope_class,
    values_from = mean_erosion,
    names_prefix = "slope_"
  )

# Calculate the overall mean for each slope class 
rs_mean <- terra::zonal(erosion, slopebins, "mean", na.rm = TRUE)[,2]

# Calculate the overall mean for each land cover class
rl_mean <- terra::zonal(erosion, lc, "mean", na.rm = TRUE)[,2]

# Combine two way table with means
final_mtwoway <- mtwo_way_wide %>% 
  mutate(overall = rl_mean[2:10]) %>% 
  add_row(landcover = 99, slope_1 = rs_mean[1], slope_2 = rs_mean[2], slope_3 = rs_mean[3], slope_4 = rs_mean[4], slope_5 = rs_mean[5], slope_6 = rs_mean[6])



tibble(final_mtwoway)


write.csv(final_mtwoway, hert("_analysis/02-26_twoWay_mean.csv")) # export clean table for manuscript


##================================ Two-way Sum ================================


# Import data
slopebins <- slope.binned
lc <- landcover
erosion <- rusle2

# Create a combined category raster. This encodes each landcover × slope combination uniquely.
multiplex <- lc * 1000 + slopebins # Creat a raster with a uniqe ID for each slope class - landcover combo
names(multiplex) <- "multiplex"


# Compute MEAN erosion value of each slope-landcover combo
two_way_sum <- zonal(erosion, multiplex, fun = "sum", na.rm = TRUE) # I think the na.rm here drops the NA landcover and water

# Seperate out multiplex
stwo_way <- two_way_sum %>% 
  mutate(
    landcover   = multiplex %/% 1000,
    slope_class = multiplex %% 1000
  ) %>% 
  rename(sum_erosion = GL_RUSLE2_30m_raster_r)

# Pivot
stwo_way_wide <- stwo_way %>% 
  select(-multiplex) %>% 
  arrange(landcover, slope_class) %>%
  pivot_wider(
    names_from = slope_class,
    values_from = sum_erosion,
    names_prefix = "slope_"
  )


# Calculate the overall sum for each slope class 
rs_sum <- terra::zonal(erosion, slopebins, "sum", na.rm = TRUE)[,2]

# Calculate the overall sum for each land cover class
rl_sum <- terra::zonal(erosion, lc, "sum", na.rm = TRUE)[,2]

# Calcualte area of each landcover
lc_area_km <- terra::zonal(cellSize(landcover.int, unit="km"),
                           landcover.int, fun="sum")[,2]


# Combine two way table with means
final_stwoway <- stwo_way_wide %>% 
  mutate(
    overall = rl_sum[2:10],
    area_km = lc_area_km[2:10],
    erosion_perc = overall / sum(overall) * 100,
    area_perc = area_km / sum(area_km) * 100
    ) %>% 
  add_row(landcover = 99, slope_1 = rs_sum[1], slope_2 = rs_sum[2], slope_3 = rs_sum[3], slope_4 = rs_sum[4], slope_5 = rs_sum[5], slope_6 = rs_sum[6])


tibble(final_stwoway)


write.csv(final_stwoway, hert("_analysis/02-26_twoWay_sum.csv")) # export clean table for manuscript






#================================ Pub Tables ================================

# Pull data
stack_slopeBylandcover_df <- read.csv(hert("_analysis/02-26_stack_slopeBylandcover.csv"))
stack_landcoverBySlope_df <- read.csv(hert("_analysis/02-26_stack_landcoverBySlope.csv"))
twoWay_mean_df <- read.csv(hert("_analysis/02-26_twoWay_mean.csv"))
twoWay_sum <- read.csv(hert("_analysis/02-26_twoWay_sum.csv")) 



##================================ Slope Bins ================================


slope_bins <- left_join(stack_landcoverBySlope_df, stack_slopeBylandcover_df, by = "land_cover")

slope_bins <- slope_bins %>% 
  # Round and rename
  mutate("mean_slope" = round(mean_slope, digits = 2),
         "X0_2.x" = round(X0_2.x, digits = 1),
         "X02_5.x" = round(X02_5.x, digits = 1),
         "X05_15.x" = round(X05_15.x, digits = 1),
         "X15_30.x" = round(X15_30.x, digits = 1),
         "X30_50.x" = round(X30_50.x, digits = 1),
         "X50_100.x" = round(X50_100.x, digits = 1),
         "X0_2.y" = round(X0_2.y, digits = 1),
         "X02_5.y" = round(X02_5.y, digits = 1),
         "X05_15.y" = round(X05_15.y, digits = 1),
         "X15_30.y" = round(X15_30.y, digits = 1),
         "X30_50.y" = round(X30_50.y, digits = 1),
         "X50_100.y" = round(X50_100.y, digits = 1)
  ) %>% 
  mutate(landcover = case_when(
    land_cover == "1 Temperate or Subpolar Needleaf Forest" ~ "Needleaf Forest",
    land_cover == "5 Temperate or Subpolar Broadleaf Deciduous Forest" ~ "Broadleaf Deciduous Forest",
    land_cover == "6 Mixed Forest" ~ "Mixed Forest",
    land_cover == "8 Temperate or Subpolar Shrubland" ~ "Shrubland",
    land_cover == "10 Temperate or Subpolar Grassland" ~ "Grassland",
    land_cover == "14 Wetland" ~ "Wetland",
    land_cover == "15 Cropland" ~ "Cropland",
    land_cover == "16 Barren Land" ~ "Barren Land",
    land_cover == "17 Urban and Built-up" ~ "Urban and Built-up",
    land_cover == "Overall" ~ "Overall",
    land_cover == "Area" ~ "Area"
  )) %>% 
  arrange(land_cover)



# Add data and format table
slope_bins.ft <- flextable(slope_bins,
                                col_keys = c("landcover",
                                             "blank",
                                             "mean_slope",
                                             "blank3",
                                             "X0_2.x",
                                             "X02_5.x",
                                             "X05_15.x",
                                             "X15_30.x",
                                             "X30_50.x",
                                             "X50_100.x",
                                             "blank2",
                                             "X0_2.y",
                                             "X02_5.y",
                                             "X05_15.y",
                                             "X15_30.y",
                                             "X30_50.y",
                                             "X50_100.y"
                                )) %>% 
  empty_blanks() %>%
  
  font(part = "all", fontname = "Calibri") %>% 
  fontsize(part = "all", size = 11) %>% 
  
  align(align = "right", part = "all") %>%
  
  align(align = "center", part = "header") %>%
  valign(valign = "center", part = "header") %>% 
  
  align(align = "left", part = "header", j = "landcover") %>% 
  align(align = "left", j = "landcover") %>% 
  
  width(j = c("mean_slope",
              "X0_2.x",
              "X02_5.x",
              "X05_15.x",
              "X15_30.x",
              "X30_50.x",
              "X50_100.x",
              "X02_5.y",
              "X05_15.y",
              "X0_2.y",
              "X15_30.y",
              "X30_50.y",
              "X50_100.y"), width = 0.75) %>% 
  width(j = "landcover", width = 2) %>% 
  
  #  padding(padding = 6, part = "header") %>% 
  line_spacing(space = 1.8, part = "header") %>% 
  
  bold(i = ~ landcover == "Broadleaf Deciduous Forest" |
         landcover == "Mixed Forest") %>% 
  bold(i = ~ landcover == "Overall") %>% 
  
  #bold(~ X02_5.y > 0, ~ ) %>% 
  
  # Add percents to bins
  set_formatter(
    `X0_2.x`  = function(x) paste0(x, "%"),
    `X02_5.x`  = function(x) paste0(x, "%"),
    `X05_15.x`  = function(x) paste0(x, "%"),
    `X15_30.x`  = function(x) paste0(x, "%"),
    `X30_50.x`  = function(x) paste0(x, "%"),
    `X50_100.x` = function(x) paste0(x, "%"),
    `X0_2.y`  = function(x) ifelse(x != 0, paste0(x, "%"), ""),
    `X02_5.y`  = function(x) ifelse(x != 0, paste0(x, "%"), ""),
    `X05_15.y`  = function(x) ifelse(x != 0, paste0(x, "%"), ""),
    `X15_30.y`  = function(x) ifelse(x != 0, paste0(x, "%"), ""),
    `X30_50.y`  = function(x) ifelse(x != 0, paste0(x, "%"), ""),
    `X50_100.y` = function(x) ifelse(x != 0, paste0(x, "%"), "")
  ) %>% 
  
  
  # Add headers
  add_header_row(values = c("",
                            "a) Landcover by Slope Class",
                            "b) Slope Class by Landcover"),
                 colwidths = c(4, # adds up to total number of cols
                               7,
                               6)
  ) %>% 
  
  # Rename columns
  labelizor(
  part = "header", 
  labels = c("landcover" = "Landcover",
             "mean_slope" = "Mean Slope (%)",
             "X0_2.x" = "0 - 2%",
             "X02_5.x" = "2 - 5%",
             "X05_15.x" = "5 - 15%",
             "X15_30.x" = "15 - 30%",
             "X30_50.x" = "30 - 50%",
             "X50_100.x" = ">50%",
             "X0_2.y" = "0 - 2%",
             "X02_5.y" = "2 - 5%",
             "X05_15.y" = "5 - 15%",
             "X15_30.y" = "15 - 30%",
             "X30_50.y" = "30 - 50%",
             "X50_100.y" = "> 50%"
  ))

slope_bins.ft






save_as_image(slope_bins.ft, path = "C:/Users/natha/OneDrive/Onedrive Documents/01_Projects/P01_MS1/Figures/slope_bins.ft.svg")




##================================ Two-way tables ================================

twoWay_mean_df
twoWay_sum

two_way <- left_join(twoWay_mean_df, twoWay_sum, by = "landcover") %>% 
 
  # Round and rename
  mutate("slope_1.x" = round(slope_1.x, digits = 2),
         "slope_2.x" = round(slope_2.x, digits = 2),
         "slope_3.x" = round(slope_3.x, digits = 2),
         "slope_4.x" = round(slope_4.x, digits = 2),
         "slope_5.x" = round(slope_5.x, digits = 2),
         "slope_6.x" = round(slope_6.x, digits = 3),
         
         "overall.x" = round(overall.x, digits = 2),
         
         
         "slope_1.y" = signif(slope_1.y, digits = 3)/1000,
         "slope_2.y" = signif(slope_2.y, digits = 3)/1000,
         "slope_3.y" = signif(slope_3.y, digits = 3)/1000,
         "slope_4.y" = signif(slope_4.y, digits = 3)/1000,
         "slope_5.y" = signif(slope_5.y, digits = 3)/1000,
         "slope_6.y" = signif(slope_6.y, digits = 3)/1000,
         
         "overall.y" = signif(overall.y, digits = 3)/1000,
         
         "area_km" = round(area_km, digits = 0),
         "erosion_perc" = round(erosion_perc, digits = 1),
         "area_perc" = round(area_perc, digits = 1),
  ) %>% 
  
  mutate(landcover = case_when(
    landcover == "1" ~ "Needleaf Forest",
    landcover == "5" ~ "Broadleaf Deciduous Forest",
    landcover == "6" ~ "Mixed Forest",
    landcover == "8" ~ "Shrubland",
    landcover == "10" ~ "Grassland",
    landcover == "14" ~ "Wetland",
    landcover == "15" ~ "Cropland",
    landcover == "16" ~ "Barren Land",
    landcover == "17" ~ "Urban and Built-up",
    landcover == "99" ~ "Overall"
  ))







# Add data and format table
two_way.ft <- flextable(two_way,
                            col_keys = c("landcover",
                                         "blank",
                                         
                                         "slope_1.x",
                                         "slope_2.x",
                                         "slope_3.x",
                                         "slope_4.x",
                                         "slope_5.x",
                                         "slope_6.x",
                                         "overall.x",
                                         
                                         "blank2",
                                         "slope_1.y",
                                         "slope_2.y",
                                         "slope_3.y",
                                         "slope_4.y",
                                         "slope_5.y",
                                         "slope_6.y",
                                         "overall.y",
                                         
                                         "blank3",
                                         "area_km",
                                         "erosion_perc",
                                         "area_perc"
                            )) %>% 
  empty_blanks() %>% 
  
  # All
  font(part = "all", fontname = "Calibri") %>% 
  fontsize(part = "all", size = 11) %>% 
  align(align = "right", part = "all") %>%
  
  # Header
  align(align = "center", part = "header") %>%
  valign(valign = "center", part = "header") %>% 
  
  # Landcover
  align(align = "left", part = "header", j = "landcover") %>% 
  align(align = "left", j = "landcover") %>% 
  
  width(j = c("landcover",
              
              "slope_1.x",
              "slope_2.x",
              "slope_3.x",
              "slope_4.x",
              "slope_5.x",
              "slope_6.x",
              "overall.x",
              
              "slope_1.y",
              "slope_2.y",
              "slope_3.y",
              "slope_4.y",
              "slope_5.y",
              "slope_6.y",
              "overall.y",
              
              "area_km",
              "erosion_perc",
              "area_perc"), width = 0.75) %>% 
  width(j = "landcover", width = 2) %>% 
  
  #  padding(padding = 6, part = "header") %>% 
  line_spacing(space = 1.8, part = "header") %>% 
  
  bold(i = ~ landcover == "Broadleaf Deciduous Forest" |
         landcover == "Mixed Forest") %>% 
  
  bold(i = ~ landcover == "Overall") %>% 
  
  # Add headers
  add_header_row(values = c("",
                            "a) Erosion Rate (t/ha/yr)",
                            "b) Total Erosion (1000*t)",
                            "c) Summary"),
                 colwidths = c(2, # adds up to total number of cols
                               8,
                               8,
                               3)
  ) %>%

  
  
  labelizor(
    part = "header", 
    labels = c("landcover" = "Landcover",
               
               "slope_1.x" = "0 - 2%",
               "slope_2.x" = "2 - 5%",
               "slope_3.x" = "5 - 15%",
               "slope_4.x" = "15 - 30%",
               "slope_5.x" = "30 - 50%",
               "slope_6.x" = "> 50%",
               "overall.x" = "Average",
               
               "slope_1.y" = "0 - 2%",
               "slope_2.y" = "2 - 5%",
               "slope_3.y" = "5 - 15%",
               "slope_4.y" = "15 - 30%",
               "slope_5.y" = "30 - 50%",
               "slope_6.y" = "> 50%",
               "overall.y" = "Average",
               
               "area_km" = "Area (km²)",
               "erosion_perc" = "% Total Erosion",
               "area_perc" = "% Total Area"
    ))



two_way.ft



save_as_image(two_way.ft, path = "C:/Users/natha/OneDrive/Onedrive Documents/01_Projects/P01_MS1/Figures/two_way.ft.svg")







##================================ RUSLE2-Slope Stats ================================

twoWay_mean_df
twoWay_sum

two_way <- left_join(twoWay_mean_df, twoWay_sum, by = "landcover")

slope_stats_df <- read.csv(hert("_analysis/slope-landcover-stats.csv"))
rulse2_stats <- read.csv(hert("_analysis/rusle2-landcover-stats.csv"))

slope_rusle <- left_join(slope_stats_df, rulse2_stats, by = "landcover.land_cover")

slope_rusle <- slope_rusle %>% 
  # Round
  mutate(
    s_mean = round(mean.x, digits = 2),
    s_median = round(median.x, digits = 2),
    s_area_km = round(area_km.x, digits = 0),
    s_area_perc = round(area_perc.x, digits = 1),
    
    e_mean = round(mean.y, digits = 2),
    e_median = round(median.y, digits = 2),
    e_sum = round(sum, digits = 0),
    e_area_km = round(area_km.y, digits = 0),
    e_area_perc = round(area_perc.y, digits = 1),
    e_erosion_perc = round(erosion_perc, digits = 1)
         
  ) %>% 
  
  # Rename landcover classes
  mutate(landcover = case_when(
    landcover.land_cover == "1 Temperate or Subpolar Needleaf Forest" ~ "Temperate or Subpolar Needleaf Forest",
    landcover.land_cover == "5 Temperate or Subpolar Broadleaf Deciduous Forest" ~ "Temperate or Subpolar Broadleaf Deciduous Forest",
    landcover.land_cover == "6 Mixed Forest" ~ "Mixed Forest",
    landcover.land_cover == "8 Temperate or Subpolar Shrubland" ~ "Temperate or Subpolar Shrubland",
    landcover.land_cover == "10 Temperate or Subpolar Grassland" ~ "Temperate or Subpolar Grassland",
    landcover.land_cover == "14 Wetland" ~ "Wetland",
    landcover.land_cover == "15 Cropland" ~ "Cropland",
    landcover.land_cover == "16 Barren Land" ~ "Barren Land",
    landcover.land_cover == "17 Urban and Built-up" ~ "Urban and Built-up",
    landcover.land_cover == "Overall" ~ "Overall"
  ))








# Add data and format table
slope_rusle.ft <- flextable(slope_rusle,
                            col_keys = c("landcover",
                                         
                                         "blank",
                                         "s_mean",
                                         "s_median",
                                         
                                         "blank2",
                                         "e_mean",
                                         "e_median",
                                         "e_sum",
                                         
                                         "blank3",
                                         "s_area_km",
                                         "e_area_perc",
                                         "e_erosion_perc"
                            )) %>% 
  empty_blanks() %>% 
  
  # All
  font(part = "all", fontname = "Calibri") %>% 
  fontsize(part = "all", size = 11) %>% 
  align(align = "right", part = "all") %>%
  
  # Header
  align(align = "center", part = "header") %>%
  valign(valign = "center", part = "header") %>% 
  
  # Landcover
  align(align = "left", part = "header", j = "landcover") %>% 
  align(align = "left", j = "landcover") %>% 
  
  width(j = c("s_area_km",
              "s_mean",
              "s_median",
              "e_mean",
              "e_median",
              "e_sum",
              "e_area_perc",
              "e_erosion_perc"), width = 0.75) %>% 
  width(j = "landcover", width = 3.5) %>% 
  
  #  padding(padding = 6, part = "header") %>% 
  line_spacing(space = 1.8, part = "header") %>% 
  
  bold(i = ~ landcover == "Temperate or Subpolar Broadleaf Deciduous Forest" |
         landcover == "Mixed Forest") %>% 
  
  bold(i = ~ landcover == "Overall") %>% 

  # Add headers
  add_header_row(values = c("",
                            "Slope (%)",
                            "Erosion (t/ha/yr)",
                            ""),
                 colwidths = c(2, # adds up to total number of cols
                               3,
                               4,
                               3)
                 ) %>% 
  
  
  # Add percent signs to all slope values
  set_formatter(
    `e_area_perc`  = function(x) ifelse(x != 0, paste0(x, "%"), ""),
    `e_erosion_perc`  = function(x) ifelse(x != 0, paste0(x, "%"), "")
  ) %>% 

  labelizor(
    part = "header", 
    labels = c("landcover" = "Landcover",
               "s_mean" = "Mean",
               "s_median" = "Median",
               
               "e_mean" = "Mean",
               "e_median" = "Median",
               "e_sum" = "Sum",
               "s_area_km" = "Area (km²)",
               
               "e_area_perc" = "% Total Area",
               "e_erosion_perc" = "% Total Erosion"
    ))





slope_rusle.ft



save_as_image(slope_rusle.ft, path = "C:/Users/natha/OneDrive/Onedrive Documents/01_MS1/Figures/slope_rusle.ft.svg")





#================================ Archive ================================


##================================ RUSLE2 ================================

###================================ Summary Stats ================================
# Calculate a quick mean, median, max, and min for each land cover


# input the raster needed for statics here
landcover.int <- landcover # call landcover
raster.in <- rusle2 # call rusle2 raster

levels(landcover.int)

# Compute statistics
rusle_mean <- terra::zonal(raster.in, landcover.int, "mean", na.rm = TRUE)[,2]
#rusle_median <- terra::zonal(raster.in, landcover.int, "median", na.rm = TRUE)[,2]
rusle_sum <- terra::zonal(raster.in, landcover.int, "sum", na.rm = TRUE)[,2]
rusle_area <- terra::zonal(cellSize(landcover.int, unit="km"),
                           landcover.int, fun="sum")[,2]


# # Global (total) stats
# g_rusle_mean <- terra::global(raster.in, "mean", na.rm = TRUE)[,1]
# g_rusle_median <- NA #terra::global(raster.in, "median", na.rm = TRUE)[,1] # doesn't work
# g_rusle_sum <- terra::global(raster.in, "sum", na.rm = TRUE)[,1]
# g_rusle_area <- terra::global(cellSize(landcover.int, unit="km"),
#                     landcover.int, fun = "sum")[,1]


# Build summary df
rusle_stats <- data.frame("landcover" = levels(landcover.int),
                          "mean" = rusle_mean[1:11], 
                          "median" = rusle_median[1:11],
                          "sum" = rusle_sum[1:11],
                          "area_km" = rusle_area[1:11]) %>% # number of cells per class
  filter(landcover.ID != 0 &
           landcover.ID != 18) %>% # remove NA and water before doing percentages
  
  mutate("area_perc" = area_km / sum(area_km) * 100, # percent of area each landcover is responsible for
         "erosion_perc" = sum / sum(sum) * 100)  # percent of erosion each landcover is responsible for

# Add totals row
rusle_stats <- rusle_stats %>%
  add_row(landcover.land_cover = "Total", mean = mean(rusle_stats$mean), sum = sum(rusle_stats$sum), area_km = sum(rusle_stats$area_km)) # Add totals row


tibble(rusle_stats) # print, data is in tonnes per hectare



#write.csv(rusle_stats, hert("_analysis/rusle2-landcover-stats.csv")) # export to excel



rulse2_stats <- read.csv(hert("_analysis/rusle2-landcover-stats.csv"))


##================================ Sl Summary Stats ================================
# calculate a quick mean, median, max, and min for each land cover


# input the raster needed for statics here
landcover.int <- landcover # landcover is also needed
raster.in <- slope # call slope raster


# Compute statistics
slope_mean <- terra::zonal(raster.in, landcover.int, "mean", na.rm = TRUE)[,2]
slope_median <- terra::zonal(raster.in, landcover.int, "median", na.rm = TRUE)[,2]
slope_area <- zonal(cellSize(landcover.int, unit="km"),
                    landcover.int, fun="sum")[,2]



# Global (total) stats
g_slope_mean <- terra::global(raster.in, "mean", na.rm = TRUE)[,1]
g_slope_median <- NA #terra::global(raster.in, "median", na.rm = TRUE)[,1] # doesn't work
g_slope_area <- terra::global(cellSize(landcover.int, unit="km"),
                              landcover.int, fun = "sum")[,1]


# Build summary df
slope_stats <- data.frame("landcover" = levels(landcover.int),
                          "mean" = slope_mean[1:11], 
                          "median" = slope_median[1:11],
                          "area_km" = slope_area[1:11]) %>% # number of cells per class
  filter(landcover.ID != 0 &
           landcover.ID != 18) %>% # Remove NA cells prior to area calculations
  
  mutate("area_perc" = area_km / sum(area_km) * 100)  # percent of area each landcover is responsible for

# Add totals row
slope_stats <- slope_stats %>%
  add_row(landcover.land_cover = "Overall", mean = mean(slope_stats$mean), area_km = sum(slope_stats$area_km)) # Add totals row


tibble(slope_stats) # print, data is in tonnes per hectare


#write.csv(slope_stats, hert("_analysis/slope-landcover-stats.csv")) # export to excel



#

# Because of the computational intensity required for these datasets, 
# we save data following computation and upload it here

slope_stats_df <- read.csv(hert("_analysis/slope-landcover-stats.csv"))
slope_stats_df <- slope_stats_df %>% # Rename landcover to be same as in first df
  rename(land_cover = landcover.land_cover)


slope_stack_lc_df <- read.csv(hert("_analysis/slope-stack_lc.csv"))
slope_stack_slope_df <- read.csv(hert("_analysis/slope-stack_slope.csv"))



# Join dataframes
slope_all <- left_join(slope_stats_df, slope_stack_lc_df, by = "land_cover")

# Clean up and rount stats
slope_all_df <- slope_all %>% 
  # Round
  mutate(mean = round(mean, digits = 2),
         median = round(median, digits = 2),
         area_km = round(area_km, digits = 0),
         area_perc = round(area_perc, digits = 1),
         "02" = round(X0_2, digits = 1),
         "05" = round(X02_5, digits = 1),
         "15" = round(X05_15, digits = 1),
         "30" = round(X15_30, digits = 1),
         "50" = round(X30_50, digits = 1),
         "100" = round(X50_100, digits = 1)
  ) %>% 
  select(-X.y, -X0_2, -X02_5, -X05_15, -X15_30, -X30_50, -X50_100) %>% 
  
  # Rename landcover classes
  mutate(landcover = case_when(
    land_cover == "1 Temperate or Subpolar Needleaf Forest" ~ "Temperate or Subpolar Needleaf Forest",
    land_cover == "5 Temperate or Subpolar Broadleaf Deciduous Forest" ~ "Temperate or Subpolar Broadleaf Deciduous Forest",
    land_cover == "6 Mixed Forest" ~ "Mixed Forest",
    land_cover == "8 Temperate or Subpolar Shrubland" ~ "Temperate or Subpolar Shrubland",
    land_cover == "10 Temperate or Subpolar Grassland" ~ "Temperate or Subpolar Grassland",
    land_cover == "14 Wetland" ~ "Wetland",
    land_cover == "15 Cropland" ~ "Cropland",
    land_cover == "16 Barren Land" ~ "Barren Land",
    land_cover == "17 Urban and Built-up" ~ "Urban and Built-up",
    land_cover == "Overall" ~ "Overall"
  )) 






# Add data and format table
slope_stats_df.ft <- flextable(slope_all_df,
                               col_keys = c("landcover",
                                            "blank",
                                            "area_km",
                                            "area_perc",
                                            "mean",
                                            "median",
                                            "blank2",
                                            "02",
                                            "05",
                                            "15",
                                            "30",
                                            "50",
                                            "100"
                               )) %>% 
  empty_blanks() %>%
  
  font(part = "all", fontname = "Calibri") %>% 
  fontsize(part = "all", size = 11) %>% 
  
  align(align = "right", part = "body") %>%
  valign(valign = "center", part = "header") %>% 
  
  align(align = "left", part = "header", j = "landcover") %>% 
  align(align = "left", j = "landcover") %>% 
  
  
  
  width(j = c("area_km",
              "area_perc",
              "mean",
              "median",
              "02",
              "05",
              "15",
              "30",
              "50",
              "100"), width = 0.7) %>% 
  width(j = "landcover", width = 3.5) %>% 
  
  line_spacing(space = 1.8, part = "header") %>% 
  
  bold(i = ~ landcover == "Temperate or Subpolar Broadleaf Deciduous Forest" |
         landcover == "Mixed Forest" |
         landcover == "Overall")


# slope_stats_df.ft <- slope_stats_df.ft %>% 
#   add_header_row(values = c("",
#                             "Summary Stats",
#                             "Slope Distribution (%)"),
#                  colwidths = c(2, # adds up to total number of cols
#                                5,
#                                6
#                                )
#                  )


# Add percent signs to all slope values
slope_stats_df.ft <- slope_stats_df.ft %>%
  set_formatter(
    `02`  = function(x) paste0(x, "%"),
    `05`  = function(x) paste0(x, "%"),
    `15`  = function(x) paste0(x, "%"),
    `30`  = function(x) paste0(x, "%"),
    `50`  = function(x) paste0(x, "%"),
    `100` = function(x) paste0(x, "%")
  )


slope_stats_df.ft <- slope_stats_df.ft %>% labelizor(
  part = "header", 
  labels = c("landcover" = "Landcover",
             "area_km" = "Area (km²)",
             "area_perc" = "Percent Area",
             "mean" = "Mean (%)",
             "median" = "Median (%)",
             "02" = "0 - 2%",
             "05" = "2 - 5%",
             "15" = "5 - 15%",
             "30" = "15 - 30%",
             "50" = "30 - 50%",
             "100" = "> 50%"
  ))

slope_stats_df.ft



save_as_image(slope_stats_df.ft, path = "C:/Users/natha/OneDrive/Onedrive Documents/01_MS1/Figures/slope_stats_ft.svg")







# Clean up and rount stats
rulse2_stats_df <- rulse2_stats %>% 
  # Round
  mutate(mean = round(mean, digits = 2),
         median = round(median, digits = 2),
         sum = round(sum, digits = 0),
         area_km = round(area_km, digits = 0),
         area_perc = round(area_perc, digits = 1),
         erosion_perc = round(erosion_perc, digits = 1)
  ) %>% 
  
  # Rename landcover classes
  mutate(landcover = case_when(
    landcover.land_cover == "1 Temperate or Subpolar Needleaf Forest" ~ "Temperate or Subpolar Needleaf Forest",
    landcover.land_cover == "5 Temperate or Subpolar Broadleaf Deciduous Forest" ~ "Temperate or Subpolar Broadleaf Deciduous Forest",
    landcover.land_cover == "6 Mixed Forest" ~ "Mixed Forest",
    landcover.land_cover == "8 Temperate or Subpolar Shrubland" ~ "Temperate or Subpolar Shrubland",
    landcover.land_cover == "10 Temperate or Subpolar Grassland" ~ "Temperate or Subpolar Grassland",
    landcover.land_cover == "14 Wetland" ~ "Wetland",
    landcover.land_cover == "15 Cropland" ~ "Cropland",
    landcover.land_cover == "16 Barren Land" ~ "Barren Land",
    landcover.land_cover == "17 Urban and Built-up" ~ "Urban and Built-up",
    landcover.land_cover == "Total" ~ "Overall"
  ))







# Add data and format table
rulse2_stats_df.ft <- flextable(rulse2_stats_df,
                                col_keys = c("landcover",
                                             "blank",
                                             "area_km",
                                             "mean",
                                             "median",
                                             "sum",
                                             "blank2",
                                             "area_perc",
                                             "erosion_perc"
                                )) %>% 
  empty_blanks() %>%
  
  font(part = "all", fontname = "Calibri") %>% 
  fontsize(part = "all", size = 11) %>% 
  
  align(align = "right", part = "all") %>%
  valign(valign = "center", part = "header") %>% 
  
  align(align = "left", part = "header", j = "landcover") %>% 
  align(align = "left", j = "landcover") %>% 
  
  width(j = c("mean",
              "median",
              "area_km",
              "area_perc",
              "erosion_perc"), width = 0.75) %>% 
  width(j = "landcover", width = 3.5) %>% 
  
  #  padding(padding = 6, part = "header") %>% 
  line_spacing(space = 1.8, part = "header") %>% 
  
  bold(i = ~ landcover == "Temperate or Subpolar Broadleaf Deciduous Forest" |
         landcover == "Mixed Forest") %>% 
  
  bold(i = ~ landcover == "Overall")



rulse2_stats_df.ft <- rulse2_stats_df.ft %>% labelizor(
  part = "header", 
  labels = c("landcover" = "Landcover",
             "mean" = "Mean (t/ha/yr)",
             "median" = "Median (t/ha/yr)",
             "sum" = "Sum (t/ha/yr)",
             "area_km" = "Area (km²)",
             "area_perc" = "Percent Area",
             "erosion_perc" = "Percent Erosion"
  ))

rulse2_stats_df.ft



save_as_image(rulse2_stats_df.ft, path = "C:/Users/natha/OneDrive/Onedrive Documents/01_MS1/Figures/rulse2_stats_ft.svg")





#Histograms 


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



r = slope.sample
values = values(slope.sample)
pal_func <- colorNumeric(c("red", "blue"), values(r), na.color = "transparent")

opacity_values <- scales::rescale(values(r), to = c(0.2, 1.0)) # Scale from 0.2 to 1.0
color_vector <- pal_func(values(r))





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







#' [TERRA][histograms] ---------------------------------------------------------------

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





