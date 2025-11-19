#'############################### [setup] ################################

# libraries needed
libs <- c("lidR", "shapefiles", "sf", "terra", "raster", "tidyr", "dplyr", "ggplot2", "easypackages", "spatialEco","here", "performance", "see", "RColorBrewer", "lme4", "nlme", "readxl", "writexl", "emmeans", "splines", "lspline", "ggeffects")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
lapply(libs, library, character.only = T)

# enter the file path for the highest level folder you're working in 
data_folder <- "C:/Users/natha/Box/box_data/_data/_erosion_pins/"

# when a file is needed, call the hert() function
# for example; data_frame = read.csv(hert("more_data/measurements_data.csv"))
hert <- function(file) {
  file_path = paste(data_folder, file, sep = "")
  return(file_path)
}

#'############################### [mostly for trial and error here] ################################

# pull precip data from box
precip.list <- list(
  read.csv(hert("_precip/ARB_PRISM_July1-Oct15.csv"), skip = 10),
  read.csv(hert("_precip/LR_PRISM_July1-Oct15.csv"), skip = 10),
  read.csv(hert("_precip/RC_PRISM_July1-Oct15.csv"), skip = 10),
  read.csv(hert("_precip/LM_PRISM_July1-Oct15.csv"), skip = 10),
  read.csv(hert("_precip/RHN_PRISM_July1-Oct15.csv"), skip = 10),
  read.csv(hert("_precip/RHS_PRISM_July1-Oct15.csv"), skip = 10)
)

# format data for processing
precip.list <- lapply(precip.list, function(df) {
  df <- mutate(df, date = as.Date(Date),
               precip = ppt..inches. / 2.54,
               mtemp = (tmean..degrees.F. - 32) * 5 / 9,
               cum.precip = cumsum(precip),
               .keep = "unused")
  return(df)
})

precip.all <- bind_rows(precip.list, .id = "id")

precip.all <- precip.all %>% 
  mutate(site = case_when(
    id == 1 ~ "ARB",
    id == 2 ~ "LR",
    id == 3 ~ "RC",
    id == 4 ~ "LM",
    id == 5 ~ "RHN",
    id == 6 ~ "RHS",
  ))

head(precip.all)


# plot hydrographs
ggplot(precip.all, mapping = aes(x = date, y = precip)) +
  geom_col() +
  facet_wrap(~site, ncol = 1)
