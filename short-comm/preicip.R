#================================ setup ================================

# chunk options
knitr::opts_chunk$set(eval = FALSE, echo = TRUE, width = 300, warning = FALSE, out.width = "45%", fig.show = 'hold', fig.align = "center")

# libraries needed
libs <- c("tinytex", "ggplot2", "dplyr", "tidyr", "rlang", "usethis", "bookdown", "knitr", "rmarkdown", "spelling", "lidR", "shapefiles", "sf", "terra", "raster", "ggplot2", "easypackages", "spatialEco", "rasterVis")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
lapply(libs, library, character.only = T)



#================================ ??? ================================

df <- read.csv("short-comm/study-precip_FCA.csv")

# Daily-level columns that stay as identifiers (one value per STATION/DATE)
id_cols <- c("STATION", "DATE", "DlySum", "DlySumMF", "DlySumQF", "DlySumS1", "DlySumS2")

long_df <- df %>%
  pivot_longer(
    cols = matches("^HR\\d{2}(MF|QF|S1|S2|Val)$"),
    names_to = c("Hour", ".value"),
    names_pattern = "^HR(\\d{2})(MF|QF|S1|S2|Val)$"
  ) %>%
  mutate(Hour = as.integer(Hour)) %>%
  arrange(STATION, DATE, Hour)

write.csv(long_df, "short-comm/study-precip_FCA_long.csv")


