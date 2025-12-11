#'############################### [setup] ################################

# libraries needed
libs <- c("readxl", "ggplot2", "scales")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
lapply(libs, library, character.only = T)

# enter the file path for the highest level folder you're working in 
data_folder <- "C:/Users/natha/Box/box_data/_data/_MPDs/"

# when a file is needed, call the hert() function
# for example; data_frame = read.csv(hert("more_data/measurements_data.csv"))
hert <- function(file) {
  file_path = paste(data_folder, file, sep = "")
  return(file_path)
}

#'############################### [solving for R (from copiolet)] ################################

# MPD_analysis.R
# Implements MPD analysis following Ahmed et al. (2014) "Analysis Procedure"
# Reads an Excel file with columns: time_s (s), H_m (m)
# Fits Kfs (m/s) and psi (m) by minimizing residuals between observed dH/dt and model dH/dt

# --- User inputs: edit these before running ---------------------------------
xlsx_file   <- hert("MPD_test_data.xlsx")   # Excel file path
sheet_name  <- "Sheet1"                # sheet name or index
time_col    <- "time_s"          # column name for time (s)
head_col    <- "H_m"             # column name for head (m)

# MPD geometry & soil parameters (set to your values)
Hi          <- 0.385    # initial water height in infiltrometer (m)
ri          <- 0.05    # cylinder radius (m) (r_i in paper)
ro          <- 0.05    # equivalent spherical source radius r_o (m) (often = ri)
Lmax        <- 0.05    # insertion depth (m)
theta_s     <- 0.95*0.43   # field-saturated volumetric water content (m3/m3)
theta_i     <- 0.85*0.43   # initial volumetric water content (m3/m3)

# Optimization / numerical settings
Kfs_init    <- 1e-4    # initial guess for Kfs (m/s)
psi_init    <- 0.5     # initial guess for psi (m)
lower_bounds <- c(Kfs = 1e-9, psi = 1e-3)
upper_bounds <- c(Kfs = 1e-1, psi = 10)
max_iter     <- 2000

# Solver settings for R root finding
R_global_upper <- 1e3   # absolute maximum R to try (m)
expand_max     <- 60    # how many times to double upper when searching
tol_uniroot    <- 1e-10

# Output files
log_csv <- hert("mpd_skipped_log.csv")   # log of skipped indices and reasons
results_csv <- hert("mpd_results.csv")   # fitted results and data

# ---------------------------------------------------------------------------

# Read data
dat <- readxl::read_excel(xlsx_file, sheet = sheet_name)
if (!(time_col %in% names(dat) && head_col %in% names(dat))) {
  stop("Excel sheet must contain columns named: ", time_col, " and ", head_col)
}
time <- as.numeric(dat[[time_col]])
Hobs <- as.numeric(dat[[head_col]])
if (any(is.na(time)) || any(is.na(Hobs))) stop("Missing values in time or head columns")

# Ensure sorted by time
ord <- order(time)
time <- time[ord]; Hobs <- Hobs[ord]

# Precompute constants and thresholds
Rmin <- sqrt(ri^2 + Lmax^2)
min_rhs_at_Rmin <- (pi/3) * (theta_s - theta_i) * (2*Rmin^3 + 3*Rmin^2*Lmax - Lmax^3 - 4*ro^3)
min_LHS_threshold <- min_rhs_at_Rmin  # LHS must be >= this for geometry to apply

# Define LHS and RHS functions
lhs_from_H <- function(H) (Hi - H) * pi * ri^2
rhs_from_R <- function(R) (pi/3) * (theta_s - theta_i) * (2*R^3 + 3*R^2*Lmax - Lmax^3 - 4*ro^3)

# Safe solver that returns list(R, reason)
solve_R_from_H_safe <- function(H, prev_R = NULL,
                                Rmin_in = NULL, R_global_upper_in = NULL,
                                expand_max_in = NULL, tol_in = NULL) {
  # Use outer-scope defaults if NULL
  if (is.null(Rmin_in)) Rmin_in <- sqrt(ri^2 + Lmax^2)
  if (is.null(R_global_upper_in)) R_global_upper_in <- R_global_upper
  if (is.null(expand_max_in)) expand_max_in <- expand_max
  if (is.null(tol_in)) tol_in <- tol_uniroot
  
  # Basic checks
  if (!is.finite(H) || !is.finite(Hi)) return(list(R = NA_real_, reason = "non-finite H or Hi"))
  LHS <- (Hi - H) * pi * ri^2
  if (LHS <= 0) return(list(R = NA_real_, reason = "Hi - H <= 0 (no infiltrated volume)"))
  if ((theta_s - theta_i) <= 0) return(list(R = NA_real_, reason = "theta_s <= theta_i"))
  # Minimum geometry threshold (precomputed outside as min_LHS_threshold)
  if (LHS < min_LHS_threshold) {
    return(list(R = NA_real_, reason = "Early-time: LHS < threshold for capped-sphere geometry"))
  }
  
  # Define f(R) = RHS(R) - LHS
  f <- function(R) rhs_from_R(R) - LHS
  
  # Initial bracket using previous R if available
  if (!is.null(prev_R) && is.finite(prev_R) && prev_R > 0) {
    lower <- max(1e-12, prev_R * 0.9, Rmin_in * 0.9)
    upper <- max(prev_R * 1.5, lower * 1.1)
  } else {
    lower <- max(1e-12, Rmin_in * 0.9)
    upper <- max(1.0, lower * 2)
  }
  
  # Quick check at Rmin
  f_Rmin <- f(Rmin_in)
  if (f_Rmin > 0 && f(lower) > 0) {
    return(list(R = NA_real_, reason = "RHS at Rmin > LHS (no solution under geometry)"))
  }
  
  # Expand upper until sign change or until global limit
  tries <- 0
  while (tries < expand_max_in && f(lower) * f(upper) > 0 && upper < R_global_upper_in) {
    upper <- min(upper * 2, R_global_upper_in)
    tries <- tries + 1
  }
  if (f(lower) * f(upper) > 0) {
    return(list(R = NA_real_, reason = "no sign change after expanding bracket"))
  }
  
  # Try uniroot
  root <- tryCatch({
    uniroot(f, lower = lower, upper = upper, tol = tol_in)$root
  }, error = function(e) NA_real_)
  if (!is.finite(root)) return(list(R = NA_real_, reason = "uniroot failed"))
  return(list(R = root, reason = "ok"))
}

# Sequential solve for R using previous R as initial guess/bracket
n <- length(Hobs)
Rvals <- rep(NA_real_, n)
solve_log <- data.frame(index = integer(0), time = numeric(0), H = numeric(0),
                        R = numeric(0), reason = character(0), stringsAsFactors = FALSE)

prev_R <- NULL
for (i in seq_len(n)) {
  res <- solve_R_from_H_safe(Hobs[i], prev_R = prev_R)
  if (res$reason == "ok") {
    Rvals[i] <- res$R
    prev_R <- res$R
    solve_log <- rbind(solve_log, data.frame(index = i, time = time[i], H = Hobs[i],
                                             R = res$R, reason = res$reason, stringsAsFactors = FALSE))
  } else {
    # Log skip reason and continue; do not update prev_R
    solve_log <- rbind(solve_log, data.frame(index = i, time = time[i], H = Hobs[i],
                                             R = NA_real_, reason = res$reason, stringsAsFactors = FALSE))
  }
}

# Save log of skipped indices
write.csv(solve_log, log_csv, row.names = FALSE)

# Filter data to only rows with valid R
valid_idx <- which(is.finite(Rvals))
if (length(valid_idx) < 5) {
  stop("Too few valid R values after filtering. Check data and parameters. See log:", log_csv)
}
time_f <- time[valid_idx]; H_f <- Hobs[valid_idx]; R_f <- Rvals[valid_idx]

# Compute forward finite differences for dH/dt and dR/dt on filtered series
m <- length(time_f)
dHdt <- numeric(m); dRdt <- numeric(m)
for (i in 1:(m-1)) {
  dt <- time_f[i+1] - time_f[i]
  if (dt <= 0) stop("Time must be strictly increasing in filtered data")
  dHdt[i] <- (H_f[i+1] - H_f[i]) / dt
  dRdt[i] <- (R_f[i+1] - R_f[i]) / dt
}
dHdt[m] <- dHdt[m-1]; dRdt[m] <- dRdt[m-1]

# B and G functions
B_of_R <- function(R) {
  (1 / Lmax) * log( (R * (ri + Lmax)) / (ri * (R + Lmax)) )
}
G_of_R <- function(R) {
  2 * ri^2 * B_of_R(R)
}

# Model predicted dH/dt
dHdt_model_vec <- function(Kfs, psi, R, dRdt, H) {
  B <- B_of_R(R)
  G <- G_of_R(R)
  denom <- (B - G)
  denom[abs(denom) < 1e-12] <- sign(denom[abs(denom) < 1e-12]) * 1e-12
  term1 <- (3 * (theta_s - theta_i) / Lmax) * (R^2 + R * Lmax) * dRdt / denom
  term2 <- (Kfs / Lmax) * (psi - H - Lmax)
  term1 - term2
}

# Objective function for optimization (use filtered indices)
obj_fun <- function(par) {
  Kfs <- par[1]; psi <- par[2]
  if (Kfs <= 0 || psi <= 0) return(1e30)
  pred <- dHdt_model_vec(Kfs, psi, R_f, dRdt, H_f)
  idx <- which(is.finite(pred) & is.finite(dHdt))
  if (length(idx) < 3) return(1e30)
  resid <- dHdt[idx] - pred[idx]
  sum(resid^2)
}

# Run optimization
init_par <- c(Kfs_init, psi_init)
opt <- optim(par = init_par, fn = obj_fun, method = "L-BFGS-B",
             lower = lower_bounds, upper = upper_bounds,
             control = list(maxit = max_iter))

if (opt$convergence != 0) {
  warning("Optimization did not fully converge (optim code: ", opt$convergence, "). Check initial guesses and data.")
}

Kfs_fit <- opt$par[1]
psi_fit <- opt$par[2]

# Compute predicted dHdt and integrate to get modeled H(t) on filtered series
dHdt_pred <- dHdt_model_vec(Kfs_fit, psi_fit, R_f, dRdt, H_f)
H_model <- numeric(m); H_model[1] <- H_f[1]
for (i in 1:(m-1)) {
  dt <- time_f[i+1] - time_f[i]
  H_model[i+1] <- H_model[i] + dHdt_pred[i] * dt
}

# Diagnostics
resid <- dHdt - dHdt_pred
rmse_dHdt <- sqrt(mean(resid[1:(m-1)]^2, na.rm = TRUE))

cat("=== MPD analysis results ===\n")
cat(sprintf("Fitted Kfs = %.4e m/s\n", Kfs_fit))
cat(sprintf("Fitted psi = %.4f m\n", psi_fit))
cat(sprintf("Objective (sum sq) = %.6e\n", opt$value))
cat(sprintf("RMSE (dH/dt) = %.4e m/s\n", rmse_dHdt))
cat("Optimization convergence code (0 = success):", opt$convergence, "\n")
cat("Skipped / logged indices saved to:", log_csv, "\n")
cat("Filtered results saved to:", results_csv, "\n")

# Prepare results table and save
out_df <- data.frame(time = time_f, H_obs = H_f, R = R_f, dHdt_obs = dHdt, dHdt_pred = dHdt_pred, H_model = H_model)
write.csv(out_df, hert("results_csv"), row.names = FALSE)

# Plots
df <- out_df
p1 <- ggplot(df, aes(x = time)) +
  geom_point(aes(y = H_obs), color = "blue", size = 1.5) +
  geom_line(aes(y = H_model), color = "red", size = 0.8) +
  labs(title = "Observed vs Modeled H(t) on Filtered Data", x = "Time (s)", y = "Head H (m)") +
  theme_minimal()
print(p1)

p2 <- ggplot(df, aes(x = H_obs, y = R)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  labs(title = "Wetting-front radius R vs Observed head H", x = "H (m)", y = "R (m)") +
  theme_minimal()
print(p2)

p3 <- ggplot(data.frame(time = time_f, resid = resid), aes(x = time, y = resid)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(color = "purple", size = 1.2) +
  geom_line(color = "purple", alpha = 0.6) +
  labs(title = "Residuals: observed dH/dt - model dH/dt", x = "Time (s)", y = "Residual (m/s)") +
  theme_minimal()
print(p3)

# Also print a short summary of skipped indices
skipped <- subset(solve_log, is.na(R))
if (nrow(skipped) > 0) {
  cat("\nSkipped indices and reasons (first 20 shown):\n")
  print(head(skipped, 20))
} else {
  cat("\nNo indices were skipped.\n")
}

# Return summary invisibly
invisible(list(Kfs_fit = Kfs_fit, psi_fit = psi_fit, rmse_dHdt = rmse_dHdt,
               solve_log = solve_log, results = out_df))