# =============================================================================
# SOIL-BOX MODEL — Scenario 1
# =============================================================================
# A simple 1-D soil-profile model with three vertically stacked boxes (layers).
# The profile has a fixed width and depth of 1, so each box's volume equals its
# height.  At every time step a fixed volume of material with a given density is
# added to the top box and fully incorporated, which changes the box's mass,
# density, and height (and therefore the total profile elevation).
#
# Key relationship (width = depth = 1):
#   mass   = density × height
#   height = mass / density
#
# When new material is added to the top box:
#   new_mass   = old_mass + (input_volume × input_density)
#   new_volume = old_volume + input_volume          [= new_height]
#   new_density = new_mass / new_volume
#
# The total elevation of the profile is the sum of all box heights.
# =============================================================================

# ── USER-CONFIGURABLE PARAMETERS ─────────────────────────────────────────────

# Initial box properties (box 1 = top, box 3 = bottom)
box_heights   <- c(0.3, 0.4, 0.3)   # initial height of each box  [length units]
box_densities <- c(1.0, 1.3, 1.6)   # initial density of each box [mass / volume]

# Material input
input_volume  <- 0.02   # volume of material added to the top box each step
input_density <- 0.8    # density of the added material

# Time configuration
n_steps <- 50           # number of time steps to simulate
dt      <- 1            # length of each time step (for labelling; model is discrete)

# ── DERIVED INITIAL STATE ────────────────────────────────────────────────────

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
