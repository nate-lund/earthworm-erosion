#================================ Setup ================================

# libraries needed
libs <- c("httr", "jsonlite", "terra")

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

#================================ X ================================

# OpenTopography API
# https://portal.opentopography.org/apidocs/openapi.json?sid=0.3158753787358791
# API key
key = "a87bd1c14527afae952eb48288ca7ec3"

# Running this provides the response of the API server (opentopo) to the request.
# Status 200 means good to go

# Pull bounding box coordinates
uleft = c(45.058415407129154, -93.73965647886905) # Upper left
bright = c(45.05724714494162, -93.73484668084741) # Bottom right

res = GET(url = "https://portal.opentopography.org/API/usgsdem",
          query = list(datasetName ="USGS1m",
                       south = bright[1],
                       north = uleft[1],
                       west = uleft[2],
                       east = bright[2],
                       outputFormat = "GTiff",
                       API_Key = key))


# Write to a temp file that R cleans up automatically
tmp <- tempfile(fileext = ".tif")
writeBin(content(res, "raw"), tmp)
dem <- rast(tmp)

plot(dem)











