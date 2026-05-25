#================================ Setup ================================

# libraries needed
libs <- c("ggplot2", "dplyr", "tidyr", "rlang")

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


#================================ Define Parameters ================================

# Define basic parameters
nsteps = 10 # Number of timesteps
dt = 1 # Size of timesteps (yrs)

# Fill starting values of boxes (box1, box2, box3)
# Here boxes are 1 cm x 1 cm x h cm layers
inital_carbon <- c(0.10, 0.03, 0.01)
inital_height <- c(5, 5, 10)

# Define carbon decomposition rate constant (k) /timestep
k = 0.05 # Ballpark temporary value

# Define carbon input per timestep (g/cm2) per timestep
I = 0.04 # Ballpark for a temperate forest

#================================ Box Construction ================================

# Build a series of data frames. Each of these dfs will contain information
# about one box property. Rows are timesteps, and columns are boxes.


# Stores box height (cm) - AKA volume (cm^3), since this is a unit profile
height_df = data.frame(box1 = rep(NA, times = nsteps),
                       box2 = NA,
                       box3 = NA)

# Stores box mass (g)
mass_df = data.frame(box1 = rep(NA, times = nsteps),
                   box2 = NA,
                   box3 = NA)

# Stores bulk density (BD) (g/cm3)
bd_df = data.frame(box1 = rep(NA, times = nsteps),
                   box2 = NA,
                   box3 = NA)

# Stores organic carbon (OC) proportion
carbon_df = data.frame(box1 = rep(NA, times = nsteps),
                   box2 = NA,
                   box3 = NA)

# Stores whether a box is the top (1) or not (0).
top_df = data.frame(box1 = rep(1, times = nsteps),
                    box2 = 0,
                    box3 = 0)


#================================ Define Functions ================================

# Function for calculating BD from porosity (m3/m3) based on SOM fraction (g/g)
# Using Porosity = 0.1224 * log(SOM) + 0.9653 from Robinson et al. (2022)
# And Particle Density (PD) = 2.7 g/cm3 * (1 - OM) + 1.4g/cm3 * (OM)
# And BD = PD * (1 - Porosity)
bd_fun <- function(soc){
  som = soc * 2 # Compute SOM from SOC
  P = 0.1224 * log(som) + 0.9653 # Compute porosity
  PD = 2.7 * (1 - som) + 1.4 * som # Compute particle density
  BD = PD * (1 - P) # Compute BD
  
  return(BD)
}

# Function for calculating soc from last soc, k, and bd (g/cm3) 
carbon_fun <- function(soc_in, bd){
  soc_gcm <- soc_in * bd # Convert SOC to units of g/cm3
  new_soc = soc_gcm * k # Compute soc following decomposition
  soc_out = new_soc / bd # Convert back
  return(soc_out)
}

carbon_fun(0.1, 0.5)

0.1 
#================================ Load Initial State ================================

carbon_df[1,] <- inital_carbon
height_df[1,] <- inital_height

# Compute BD based on initial carbon condition
initial_bd <- lapply(inital_carbon, bd_fun)
  bd_df[1,] <- initial_bd

# Compute mass based on initial BD
mass_df[1,] <- inital_height * unlist(initial_bd)

#================================ Model Loop ================================

# The model processes soil data in the following steps:
# * I need to try this in a different order to assess if it impacts outputs.
# 1. Carbon input added to mass.
# 2. OC proportion re-calculated. (t-mass * t-OC + I / t-mass + I)
# 3. Carbon decomposition.
# 4. BD computation
# 5. Volume estimated

t = 1
for (t in seq_len(nsteps - 1)) {
  
  # Computations for Box 1
  # 1. Input added to mass
  mass_df$box1[t + 1] = mass_df$box1[t] + I
  
  # 2. OC recalculated
  temp_carbon = (mass_df$box1[t] * carbon_df$box1[t] + delta_mass) / mass_df$box1[t + 1]
  temp_bd = bd_fun(temp_carbon) # Compute an intermediary BD only including inputs 
  
  # 3. Carbon decomposition
  # Two options here, the first is to use the old BD value ignoring inputs.
  # delta_carbon = carbon_df$box1[t] - carbon_fun(carbon_df$box1[t + 1],
  #                                               bd_df$box1[t],
  #                                               I)
  # The second is to use the intermediary, considering inputs only, BD value.
  carbon_df$box1[t + 1] = carbon_fun(temp_carbon,
                                     temp_bd)
  
  # 4. Bulk Density
  bd_df$box1[t + 1] = bd_fun(carbon_df$box1[t + 1])
  
  
  # 5. Height/Volume
  height_df$box1[t + 1] = mass_df$box1[t + 1] / bd_df$box1[t + 1]
}


height_df$time = seq_len(nsteps)

ggplot(data = height_df, mapping = aes(x = time, y = box1)) +
  geom_point()

