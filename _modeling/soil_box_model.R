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
dt = 1 # Size of timesteps

# Fill starting values of boxes (box1, box2, box3)
# Here boxes are 1 cm x 1 cm x h cm layers
inital_carbon <- c(0.10, 0.03, 0.01)
inital_height <- c(5, 5, 10)

# Define carbon decomposition rate constant (k) /yr
k = 0.05 # Ballpark temporary value

# Define carbon inputs (0.04 g/cm2/yr)
I = 0.04 # Ballpark for a temperate forest

#================================ Box Construction ================================

# Build a series of data frames. Each of these dfs will contain information
# about one box property. Rows are timesteps, and columns are boxes.


# Stores box height (cm) - AKA volume, since this is a unit profile
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

# Stores wheter a box is the top (1) or not (0).
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
# Using 1 / k * (1 - exp(-k * soc)) 
carbon_fun <- function(soc_in, bd, input){
  soc_gcm <- soc_in * bd # Convert SOC to units of g/cm3
  new_soc = 1 / k * (I - I * exp(-k * 1)) # Compute soc following decomposition
  #' [^ WIP here, this equation is junk, maybe somehow integrate carbon steady state science in here. e.g. assume we are at carbon steady state at t0, but then something changes]
  soc_out = (new_soc + input) / bd # Add inputs and convert back
  return(soc_out)
}


#================================ Load Initial State ================================

carbon_df[1,] <- inital_carbon
height_df[1,] <- inital_height

# Compute BD based on initial carbon condition
initial_bd <- lapply(inital_carbon, bd_fun)
  bd_df[1,] <- initial_bd

# Compute mass based on initial BD
mass_df[1,] <- inital_height * unlist(initial_bd)

#================================ Model Loop ================================

t = 1
for (t in seq_len(nsteps)) {
  
  # Box 1
    # Carbon
    carbon_df$box1[t + 1] = carbon_fun(carbon_df$box1[t],
                                     bd_df$box1[t],
                                     I)
    # Bulk Density
    bd_df$box1[t + 1] = bd_fun(carbon_df$box1[t + 1])
    
    # Mass
    mass_df$box1[t + 1] = mass_df$box1[t] + I
    
    # Height
    height_df$box1[t + 1] = mass_df$box1[t + 1] / bd_df$box1[t + 1]
}
    








# Initial box properties (box 1 = top, box 3 = bottom)
box_heights   <- c(0.3, 0.4, 0.3)   # initial height of each box  [length units]
box_densities <- c(1.0, 1.3, 1.6)   # initial density of each box [mass / volume]

# Material input
input_volume  <- 0.02   # volume of material added to the top box each step
input_density <- 0.8    # density of the added material

# Time configuration
n_steps <- 50           # number of time steps to simulate
dt      <- 1            # length of each time step (for labelling; model is discrete)



#================================ Variables ================================

n_boxes    <- length(box_heights)
box_masses <- box_densities * box_heights   # mass = density × height (vol = height)

# ── STORAGE FOR OUTPUT ───────────────────────────────────────────────────────

# Matrices: rows = time steps (including t = 0), columns = boxes
height_mat  <- matrix(NA, nrow = n_steps + 1, ncol = n_boxes)
density_mat <- matrix(NA, nrow = n_steps + 1, ncol = n_boxes)
mass_mat    <- matrix(NA, nrow = n_steps + 1, ncol = n_boxes)

# Record the initial state (row 1 = time 0)
height_mat[1, ]  <- box_heights
density_mat[1, ] <- box_densities
mass_mat[1, ]    <- box_masses

# Vector to store total profile elevation at each time
elevation <- numeric(n_steps + 1)
elevation[1] <- sum(box_heights)

# ── SIMULATION LOOP ──────────────────────────────────────────────────────────

for (t in seq_len(n_steps)) {

  # ---- Add material to the top box (box 1) ----
  added_mass      <- input_volume * input_density
  box_masses[1]   <- box_masses[1] + added_mass
  box_heights[1]  <- box_heights[1] + input_volume        # volume added = height added
  box_densities[1] <- box_masses[1] / box_heights[1]      # updated bulk density

  # ---- Store the state for this time step ----
  height_mat[t + 1, ]  <- box_heights
  density_mat[t + 1, ] <- box_densities
  mass_mat[t + 1, ]    <- box_masses
  elevation[t + 1]     <- sum(box_heights)
}

# ── BUILD A TIDY RESULTS DATA FRAME ─────────────────────────────────────────

time_vec <- seq(0, n_steps * dt, by = dt)

results <- data.frame(
  time      = rep(time_vec, times = n_boxes),
  box       = rep(paste0("Box ", 1:n_boxes), each = n_steps + 1),
  height    = as.vector(height_mat),
  density   = as.vector(density_mat),
  mass      = as.vector(mass_mat)
)

elevation_df <- data.frame(
  time      = time_vec,
  elevation = elevation
)

# ── QUICK LOOK AT RESULTS ───────────────────────────────────────────────────

cat("── Initial state ──\n")
print(results[results$time == 0, ])

cat("\n── Final state ──\n")
print(results[results$time == max(time_vec), ])

cat("\n── Elevation: start =", elevation[1],
    " | end =", elevation[n_steps + 1], "──\n")

# ── PLOTTING ─────────────────────────────────────────────────────────────────

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

# 1. Total profile elevation over time
plot(time_vec, elevation, type = "l", lwd = 2, col = "steelblue",
     xlab = "Time", ylab = "Elevation",
     main = "Total Profile Elevation")

# 2. Height of each box over time
matplot(time_vec, height_mat, type = "l", lwd = 2, lty = 1,
        col = c("tomato", "goldenrod", "seagreen"),
        xlab = "Time", ylab = "Height",
        main = "Box Heights")
legend("topleft", legend = paste("Box", 1:n_boxes),
       col = c("tomato", "goldenrod", "seagreen"), lwd = 2, bty = "n")

# 3. Density of each box over time
matplot(time_vec, density_mat, type = "l", lwd = 2, lty = 1,
        col = c("tomato", "goldenrod", "seagreen"),
        xlab = "Time", ylab = "Density",
        main = "Box Densities")
legend("topright", legend = paste("Box", 1:n_boxes),
       col = c("tomato", "goldenrod", "seagreen"), lwd = 2, bty = "n")

# 4. Stacked box diagram at final time step (profile cross-section)
final_h <- height_mat[n_steps + 1, ]
final_d <- density_mat[n_steps + 1, ]
box_cols <- c("tomato", "goldenrod", "seagreen")

# Boxes stack from bottom (box 3) to top (box 1)
# Compute the bottom-edge y-coordinate for each box (bottom-up order: 3, 2, 1)
y_bot <- numeric(n_boxes)
stacking_order <- n_boxes:1
cum_h <- 0
for (i in stacking_order) {
  y_bot[i] <- cum_h
  cum_h    <- cum_h + final_h[i]
}

plot(NULL, xlim = c(0, 1), ylim = c(0, cum_h * 1.1),
     xlab = "", ylab = "Height", main = "Final Profile (cross-section)",
     xaxt = "n")

for (i in 1:n_boxes) {
  rect(0.1, y_bot[i], 0.9, y_bot[i] + final_h[i],
       col = adjustcolor(box_cols[i], alpha.f = 0.6), border = "black")
  text(0.5, y_bot[i] + final_h[i] / 2,
       labels = paste0("Box ", i, "\nh=", round(final_h[i], 3),
                        "\nd=", round(final_d[i], 3)),
       cex = 0.8)
}
