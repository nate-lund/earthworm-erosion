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

# input data, for now just dealing with arb
input.arb <- read.csv(hert("_precip/ARB_PRISM_July1-Oct1.csv"), skip = 10)
input.lr <- read.csv(hert("_precip/LR_PRISM_July1-Oct1.csv"), skip = 10)
input.p <- bind_rows(input.arb, input.lr, .id = "site")


# formatting 
input.arb <- input.arb %>%
  mutate(date = as.Date(Date), .keep = "unused") %>% 
  rename(precip = ppt..mm., mtemp = tmean..degrees.C.)

# formatting 
input.lr <- input.lr %>%
  mutate(date = as.Date(Date), .keep = "unused") %>% 
  rename(precip = ppt..mm., mtemp = tmean..degrees.C.)

input.p <- bind_rows(input.arb, input.lr, .id = "id")

input.p <- input.p %>% 
  mutate(site = case_when(
    id == 1 ~ "ARB",
    id == 2 ~ "LR"
  ))

head(input.p)


# plot hydrographs
ggplot(input.p, mapping = aes(x = date, y = precip)) +
  geom_col() +
  facet_wrap(~site, ncol = 1)





#'############################### [interseting code chunk, junk] ################################

# mutate(mm.period = case_when( # sections the precip by measurement period
#   date > dates.t[1,1] & date < dates.t[2,1] ~ dates.t[2,1],
#   date >= dates.t[2,1] & date < dates.t[3,1] ~ dates.t[3,1],
#   date >= dates.t[3,1] & date < dates.t[4,1] ~ dates.t[4,1],
#   date >= dates.t[4,1] & date < dates.t[5,1] ~ dates.t[5,1],
#   date >= dates.t[5,1] & date <= dates.t[6,1] ~ dates.t[6,1],
#   TRUE ~ dates.t[1,1])) %>% 
#   group_by(mm.period) %>% 
#   summarise(cum.precip = sum(precip)) %>% # sum precip over measurement periods
#   rename(date = mm.period)
