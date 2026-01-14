#================================ Setup ================================

# libraries needed
libs <- c("lidR", "shapefiles", "sf", "terra", "raster", "tidyr", "dplyr", "ggplot2", "easypackages", "spatialEco","here", "performance", "see", "RColorBrewer", "lme4", "nlme", "readxl", "writexl", "emmeans", "splines", "lspline", "ggeffects", "lubridate", "cowplot", "gridGraphics", "broom", "DT")

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


#================================ Input & Prepping DF ================================

# Pull in the data from box and some pre-processing.
input.a <- read_excel(hert("working_measurements-data.xlsx"), sheet = "2025_ARB-LR") # ARB and LR data

input.b <- read_excel(hert("working_measurements-data.xlsx"), sheet = "2025_Par-Sci") # Par Sci data

input <- merge(input.a, input.b, all = TRUE) # merge datasets

# we need to first average the duplicate measurements for each pin.
input$mm_ch <- (input$mm1_ch + input$mm2_ch) / 2 # for changes
input$mm_bl <- (input$mm1_bl + input$mm2_bl) / 2 # for baseline

# convert date to days past
input$date <- as.Date(input$date) # format date column
input$dayof <- as.numeric(input$date) - 20283 + 195 # convert to days since 1970-01-01, subtract number of days until 2025-01-01

# create and index column, a unique ID for each mm
input$index <- as.numeric(factor((paste(input$site, input$forest, input$transect, input$slope_pos, input$pin_ID, sep = "_"))))
# and a unique ID for each pin
input$id <- 1:nrow(input)

# filter the data set to remove unneeded columns
input <- input %>% select(date, dayof, index, id, worms, site, forest, transect, slope_pos, pin_ID, mm_ch, mm_bl)

# arrange pins unique pin, in order of date
input <- input %>% 
  arrange(index) %>% # sort
  mutate(dt = 0) %>% # add a dt column for later
  mutate(dmm = 0) # add a dmm column for later

# write to an excel file to be used later
write_xlsx(input, hert("_analysis/input.xlsx"))

##================================ Differencing Pins ================================

# number of unique pins
nindex <- length(unique(input$index))

# create a list, where each item is a data frame with all the measurements of each pin 
input_list <- vector(mode = "list", length = nindex) # create empty list
for(i in 1:nindex) {
  input_list[[i]] <- input %>% filter(index == i)
}

for(i in 1:nindex){ # out for loop selects each data frame (individual pin) in the list
  for(j in 2:6) { # inner for loop goes through each row of list [row, column]
    input_list[[i]][j,13] <- input_list[[i]][j, 2] - input_list[[i]][j - 1, 2] # subtracts today's day of from last measurement's, dt
    input_list[[i]][j,14] <- input_list[[i]][j - 1, 12] - input_list[[i]][j, 11] # subtracts today's mm from last baseline, dmm
  }
  input_list[[i]] <- input_list[[i]] %>% drop_na(slope_pos) # remove rows with NA's (in slope_pos column)
}

# create a cumulative sum for each pin, mm (baseline 0)
for (i in 1:nindex){
  input_list[[i]]$mm <- cumsum(input_list[[i]]$dmm)
  input_list[[i]]$amm <- cumsum(abs(input_list[[i]]$dmm))
}

#  rebuild the data frame
output <- input_list[[1]] # jank start to the for loop, needs a base
nlist <- length(input_list)

for(i in 2:nlist) { # for loop to stack dataframes one list at a time
  output <- merge(output, input_list[[i]], all = TRUE)
} 

# write to an excel file
write_xlsx(output, hert("_analysis/output.xlsx"))

##================================ QAQC ================================

# clean up data
output.qc <- output %>%
  mutate(dmdt = ifelse(dt == 0, 0, dmm / dt)) %>% # create an erosion rate column (mm/day)
  mutate(abs = abs(dmdt)) # create abs_dmm column 

# arrange the data by forest for later plotting
output.qc$forest <- factor(output$forest, 
                        levels=c("ASH", "LRE", "LRW", "MAG", "WD", "LRJ",
                                 "RCE", "LME", "IH", "RCJ", "LMJ", "NH", "PLH")) # need to give it forests here


write_xlsx(output, hert("_analysis/output_qc.xlsx"))


#================================ Stats for Pins ================================

#' The goal here is to dig  into pin-specific and measurement to measurement trends. Here we plot the pins as lines, show the boxplots of thoses measurements, and fit a cubic spline. Below is a linear spline (which I think is more realistic since erosion is largely storm based). 

#' I would like to plot based on cumulative precipitation (thinking between each time period), see if there is a relationship between percip and slope [WIP].
  #' How much does precip impact the elevation of the forest as a whole versus pin specific.
  #' Is there a tighter relationship between erosion and precipitation in EW vs JW? Across all sites?

#' Could I also look at pins that experience only deposition? only erosion?

#' also consider doing abs splines / analysis

#' what if, I calcualted the erosion rate for every pin (easy), then I compared that to cumulate precip at that point. then, I do a scatter plot with precip on the x-axis and erosion on the y-axis. then fit a lm to the JW and to the EW data.

# create some needed columns and sort / remove uneeded ones
pin.p <- output.qc %>% 
  mutate(pin = (paste(forest, transect, slope_pos, pin_ID, sep = "_"))) %>% # unique id for each pin
  mutate(forest_date = interaction(forest, date, slope_pos, sep = "_")) %>% # combines  forest at each date
  select(date, dayof, site, forest, transect, slope_pos, pin_ID, mm, amm, worms, id, pin, forest_date, dmm, dmdt)


# prep values
forests <- c("ASH", "LRE", "LRW", "MAG", "WD", "LRJ", "RCE", "LME", "IH", "RCJ", "LMJ", "NH", "PLH")
nforests <- length(forests)

##================================ Calculating Precip ================================

head(as_tibble(pin.p))

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

# prep for use
precip.ARB <- precip.list[1]
precip.LR <- precip.list[2]
precip.RC <- precip.list[3]
precip.LM <- precip.list[4]
precip.RHN <- precip.list[5]
precip.RHS <- precip.list[6]



###================================ Merging Pin and Precip DF ================================
# Input precip data into the measurement data frame

# need to split dataframe for calculations, etc
pin.forests <- vector(mode = "list", length = nforests) # create empty list, nforests defined above

# for loop for calculations
for(i in 1:nforests) { 
  # setup
  df.t <- pin.p %>% filter(forest == forests[i]) # split dataframe into list
  site.t <- df.t[1,3] # extract site from forest df
  precip.t <- as.data.frame(get(paste("precip.", site.t, sep = ""))[[1]]) # pulls correct precip data 

  
  # for precip per chunk
  dates.t <- unique(df.t[1]) # gets unique dates
  
  precip.t <- precip.t %>%
    mutate(period = case_when( # precip on the day of measurement counts towards only the next period
      date == dates.t[1,] ~ 0,
      date > dates.t[1,] & date <= dates.t[2,] ~ 1,
      date > dates.t[2,] & date <= dates.t[3,] ~ 2,
      date > dates.t[3,] & date <= dates.t[4,] ~ 3,
      date > dates.t[4,] & date <= dates.t[5,] ~ 4,
      date > dates.t[5,] & date <= dates.t[6,] ~ 5,
      date > dates.t[6,] & date <= dates.t[7,] ~ 6,
      TRUE ~ NA # first measurement will be set to 0
    )) %>% 
    
    drop_na(period) %>% 
    group_by(period) %>% 
    mutate(period.sum = sum(precip, na.rm = TRUE)) %>%
    ungroup() %>% 
    select(-period)
  
  dfprecip.t <- merge(df.t, precip.t, by = "date", all = FALSE) # merge precip and pins datasets based on date
  
  ifelse(i == 1, pin.p2 <- dfprecip.t, pin.p2 <- merge(pin.p2, dfprecip.t, all = TRUE)) # merge consecutive forest data frames

}

write_xlsx(pin.p2, hert("_analysis/pins-with-precip.xlsx"))


##================================ 1 Fitting Linear Splines ================================
# Fitting linear splines, elevation by date, spline for each forest and hill slope position.

pin.p2 <- pin.p2

#' [Divide up data]  
hill.poses <- c("BS", "FS")
npos = length(hill.poses)

pin.list <- vector(mode = "list",
                   length = nforests * 2) # create empty list, 2x for hill slope positions


for(i in 1:nforests) { # for loop to split by forest for BS
  pin.list[[i]] <- pin.p2 %>% filter(forest == forests[i] & slope_pos == hill.poses[1])
}

for(i in 1:nforests) { # for loop to split by forest for FS
  pin.list[[i + nforests]] <- pin.p2 %>% filter(forest == forests[i] & slope_pos == hill.poses[2])
}


#' [fit lsplines and generated predicted values]
for (i in 1:(nforests * 2)){
  # fit lspline
  dates <- unique(pin.list[[i]]$dayof)[2:(length(unique(pin.list[[i]]$dayof))-1)] # knots at here, excludes first date
  
  pin.lspline <- lm(data = pin.list[[i]], mm ~ lspline(dayof, knots = dates)) # fit lspline
  pin.list[[i]]$lspline <- predict(pin.lspline) # create predictions
  
  ifelse(i == 1,
         pin.ls <- pin.list[[i]],
         pin.ls <- merge(pin.ls, pin.list[[i]], all = TRUE)) # merge data frames
  
  # save coefficients
  coefs.t <- tidy(pin.lspline)
  coefs.t$forest = unique(pin.list[[i]]$forest)
  coefs.t$slope_pos = unique(pin.list[[i]]$slope_pos)
  
  ifelse(i == 1,
         coefs.list <- coefs.t,
         coefs.list <- merge(coefs.list, coefs.t, all = TRUE)) # merge data frames
}

# Visualize data
datatable(pin.ls)

# Export data
write_xlsx(pin.ls, hert("_analysis/pins-with-lspline.xlsx"))
write_xlsx(coefs.list, hert("_analysis/lsp_coefs.xlsx"))


##================================ 2 Fitting Linear Splines ================================
# Fitting linear splines, elevation by date, spline for each forest.

# Split our master df (pin.p2) into different data frames in a list by forest, as these all had measurements taken on the same date (for knots)

pin.list <- vector(mode = "list", length = nforests ) # create empty list

for(i in 1:nforests) { # for loop to split
  pin.list[[i]] <- pin.p2 %>% filter(forest == forests[i])
}


# fit lsplines and generated predicted values
for (i in 1:nforests){
  # fit lspline
  dates <- unique(pin.list[[i]]$dayof)[2:(length(unique(pin.list[[i]]$dayof))-1)] # knots at here, excludes first date
  
  pin.lspline <- lm(data = pin.list[[i]], mm ~ lspline(dayof, knots = dates)) # fit lspline
  pin.list[[i]]$lspline <- predict(pin.lspline) # create predictions
  
  ifelse(i == 1,
         pin.ls <- pin.list[[i]],
         pin.ls <- merge(pin.ls, pin.list[[i]], all = TRUE)) # merge data frames
  
  # save coefficients
  coefs.t <- tidy(pin.lspline)
  coefs.t$forest = forests[i]
  
  ifelse(i == 1,
         coefs.list <- coefs.t,
         coefs.list <- merge(coefs.list, coefs.t, all = TRUE)) # merge data frames
  }

write_xlsx(pin.ls, hert("_analysis/pins-with-lspline.xlsx"))

write_xlsx(coefs.list, hert("_analysis/lsp_coefs.xlsx"))

###================================ Summary Stats for lsplines ================================

coefs.wide <- coefs.list %>%
  mutate(term = case_when(
    term == "(Intercept)" ~ "intercept",
    term == "lspline(dayof, knots = dates)1" ~ "slope.1",
    term == "lspline(dayof, knots = dates)2" ~ "slope.2",
    term == "lspline(dayof, knots = dates)3" ~ "slope.3",
    term == "lspline(dayof, knots = dates)4" ~ "slope.4",
    term == "lspline(dayof, knots = dates)5" ~ "slope.5",
  )) %>% 
  pivot_wider(
    names_from = term,
    values_from = c(estimate, std.error, statistic, p.value)
  )

    
write_xlsx(coefs.wide, hert("_analysis/lsp_coefs_wide.xlsx"))

datatable(coefs.wide)

# calculate average erosion 
coefs.wide <- coefs.wide %>% 
  mutate(ave_erosion = estimate_slope.1)

# WIP plot by period
ggplot(data = coefs.list, mapping = aes(y = estimate, x = forest)) +
  geom_boxplot()


##================================ Plotting Pins and Precip ================================

###================================ Pin by Precip ================================

filter.pin.ls <- pin.ls %>% filter(forest != "IH")

# WIP plot dmdt / precip
ggplot(data = filter.pin.ls, mapping = aes(y = dmm, x = period.sum, linetype = forest)) +
  geom_jitter(aes(color = forest)) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~worms, ncol = 1)

###================================ 1 Pin by Date ================================

pins.plots <- pin.ls %>% 
  filter(site == "LR" | site == "ARB")

#' [WIP] create scatter plots showing mm over time
ggplot(data = pins.plots,
       mapping = aes(x = date,
                     y = mm)) +
  
  geom_line(aes(group = pin,
                color =  slope_pos)) +

  geom_boxplot(aes(group = forest_date,
                   width = 3,
                   fill = slope_pos)) +

  geom_line(data = pins.plots,
            aes(x = date,
                y = lspline,
                linetype = slope_pos)) +
  
  facet_wrap(~forest) +
  
  # Visual elements
  scale_y_continuous(limits = c(-20, 20)) + # cuts off some outliers, fine for visualization
  scale_x_date(limits = c(ymd("2025-07-01"), ymd("2025-10-15"))) +
  geom_hline(yintercept = 0) +
  theme(axis.title.x = element_blank())


###================================ 2 Pin by Date ================================

# split
forest.list <- vector(mode = "list", length = nforests) # create empty list
for(i in 1:nforests) { # for loop to split
  forest.list[[i]] <- pin.ls %>% filter(forest == forests[i])
}


# create plots
pins.plots <- lapply(forest.list, function(df) {
  forest_name <- unique(df$forest)
  title_text <- if_else(length(forest_name) == 1, forest_name, paste(forest_name, collapse = ", "))

  a <- ggplot(data = df, mapping = aes(x = date, y = mm)) +
    geom_line(aes(group = pin, color =  slope_pos)) +
    geom_boxplot(aes(group = forest_date), width = 1.5) +
    geom_line(aes(x = date, y = lspline), color = "black", linewidth = 1, linetype = 2) +
    #scale_y_continuous(limits = c(-20, 20)) + # cuts off some out liers, fine for visualization
    scale_x_date(limits = c(ymd("2025-07-01"), ymd("2025-10-15"))) +
    geom_hline(yintercept = 0) +
    theme(axis.title.x = element_blank()) +
    ggtitle(label = title_text) +
    theme(legend.position = "none")
  
  return(a)
})


# get legend
legend <- get_legend(
  pins.plots[[1]] + theme(legend.position = "right")
)

plot_grid(legend, plotlist = pins.plots) # plot all plots

pins.plots[6] # plot a single plot


###================================ Precip ================================

# pull precip data from box
precip.list <- list(
  read.csv(hert("_precip/ARB_PRISM_July1-Oct15.csv"), skip = 10),
  read.csv(hert("_precip/LR_PRISM_July1-Oct15.csv"), skip = 10),
  read.csv(hert("_precip/RC_PRISM_July1-Oct15.csv"), skip = 10),
  read.csv(hert("_precip/LM_PRISM_July1-Oct15.csv"), skip = 10),
  read.csv(hert("_precip/RHN_PRISM_July1-Oct15.csv"), skip = 10),
  read.csv(hert("_precip/RHS_PRISM_July1-Oct15.csv"), skip = 10)
)

sites = c("ARB", "LR", "RC", "LM", "RHN", "RHS")
nsites = length(sites)

for (i in 1:nsites){
  df.t <- precip.list[[i]]
  df.t <- mutate(df.t, date = as.Date(Date),
               precip = ppt..inches. / 2.54,
               mtemp = (tmean..degrees.F. - 32) * 5 / 9,
               cum.precip = cumsum(precip),
               site = sites[i],
               .keep = "unused")
  precip.list[[i]] <- df.t
}

# create plots
precip.plots <- lapply(precip.list, function(df) {
  site_name <- unique(df$site)
  title_text <- if_else(length(site_name) == 1, site_name, paste(site_name, collapse = ", "))
  
  b <- ggplot(data = df, mapping = aes(x = date, y = precip)) +
    geom_col() +
    theme(axis.title.x = element_blank()) +
    ggtitle(label = title_text)

  return(b)
})

plot_grid(ncol = 1,
          plotlist = precip.plots) #plot all plots


###================================ Pins & Precip side-by-side ================================

pins.plots # pins
precip.plots # precip

# create global env objects from plot lists, named by titles
for (i in seq_along(pins.plots)) { #' [pins]
  p <- pins.plots[[i]]
  
  # Extract the title from the ggplot object
  title <- p$labels$title
  
  # Sanitize the title to make it a valid R object name
  safe_name <- paste(make.names(title), ".plot", sep = "")
  
  # Assign the plot to a variable with that name
  assign(safe_name, p, envir = .GlobalEnv)
}


for (i in seq_along(precip.plots)) { #' [precip]
  p <- precip.plots[[i]]
  
  # Extract the title from the ggplot object
  title <- p$labels$title
  
  # Sanitize the title to make it a valid R object name
  safe_name <- paste(make.names(title), ".plot", sep = "")
  
  # Assign the plot to a variable with that name
  assign(safe_name, p, envir = .GlobalEnv)
}

# LR and ARB
plot_grid(ncol = 3, nrow = 4,
          ASH.plot, LRE.plot, LRW.plot,
          ARB.plot, LR.plot, LR.plot,
          MAG.plot, WD.plot, LRJ.plot,
          ARB.plot, ARB.plot, LR.plot)

# Par-Sci
plot_grid(ncol = 4, nrow = 4,
          RCJ.plot, LMJ.plot, NH.plot, PLH.plot,
          RC.plot, LM.plot, RHN.plot, RHS.plot,
          RCE.plot, LME.plot, IH.plot, legend,
          RC.plot, LM.plot, RHN.plot)














#================================ OLD ================================


#' [plotting pin elevation (mm, in mm) over time by forest] 

# create a draft dataset
forest.t <- output.qc 

# create scatter plots showing mm over time
ggplot(data = forest.t, mapping = aes(x = date, y = mm, color = forest, shape = slope_pos)) +
  geom_jitter() +
  facet_wrap(~forest) +
  geom_smooth(method = "lm", se = TRUE, color = "black") # for all sites

ggplot(data = filter(forest.t, site == "ARB" | site == "LR"), mapping = aes(x = date, y = mm, color = forest, linetype = slope_pos, shape = slope_pos)) +
  geom_jitter() +
  facet_wrap(~forest) +
  geom_smooth(method = "lm", se = TRUE, color = "black") # for LR-ARB

ggplot(data = filter(forest.t, site == "Par-Sci"), mapping = aes(x = date, y = mm, color = forest, linetype = slope_pos, shape = slope_pos)) +
  geom_jitter() +
  facet_wrap(~forest) +
  geom_smooth(method = "lm", se = TRUE, color = "black") # for Par-Sci


# create a box plot showing mm over time with all forests 
forest.date.t <- forest.t %>% mutate(forest_date = interaction(forest, date, sep = "_")) # make a column for each forest at each date

ggplot(forest.date.t, aes(x = forest_date, y = mm, fill = forest)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#' [fitting lms for each forest and slope pos]

# prep values
forests <- c("ASH", "LRE", "LRW", "MAG", "WD", "LRJ",
             "RCE", "LME", "IH", "RCJ", "LMJ", "NH", "PLH") # list of all forests
place.h = 2 * rep(NA, times = length(forests))

# create a df to hold summary coef
stats.df <- data.frame("forest" = rep(forests, each = 2),
                       "slope_pos" = rep(c("BS", "FS"), times = length(place.h / 2)),
                       "slope" = c(place.h),
                       "SE" = c(place.h),
                       "p-value" = c(place.h),
                       "cf_lower" = c(place.h),
                       "cf_upper" = c(place.h),
                       "worms" = c("EW", "EW", "EW", "JW", "JW", "JW", "EW", "EW", "EW", "EW", "JW", "JW", "JW"))

# for loop to run lm and store summary statistics in the df made above
for(i in c(1,3,5,7,9,11,13,15,17,19,21,23,25)){
  lm.temp <- lm(data = forest.t %>% filter(forest == forests[(i+1)/2] ), mm ~ dayof * slope_pos)
  stats.df[i, 3] = coef(lm.temp)[2] # BS slope
  stats.df[i + 1, 3] = coef(lm.temp)[2] + coef(lm.temp)[4] # FS slope
  
  stats.df[i, 4] = summary(lm.temp)$coefficients[2, 2] # BS SE
  stats.df[i + 1, 4] = summary(lm.temp)$coefficients[4, 2] # FS SE
  
  stats.df[i, 5] = summary(lm.temp)$coefficients[2, 4] # BS p-value
  stats.df[i + 1, 5] = summary(lm.temp)$coefficients[4, 4] # FS p-value
  
  stats.df[i, 6] = confint(lm.temp)[2,1] # BS lower confint
  stats.df[i + 1, 6] = confint(lm.temp)[4,1] # FS lower confint
    
  stats.df[i, 7] = confint(lm.temp)[2,2] # BS upper confint
  stats.df[i + 1, 7] = confint(lm.temp)[4,2] # FS upper confint
}

print(stats.df)

#' [fit a LM to compare JW vs EW, across BS and FS]

# for all sites
lm.forest2 <- lm(data = stats.df, slope ~ worms + slope_pos)
summary(lm.forest2)

# for significant sites
stats.sig <- filter(stats.df, p.value < 0.049) # filter for low p-value
lm.forest2 <- lm(data = stats.sig, slope ~ worms + slope_pos) # fit lm for those values
summary(lm.forest2)

# compute erosion values in cm/yr
erosion <- stats.df %>%
  mutate("elevation change (cm/yr)" = slope * 365.25 / 10) %>%
  mutate("error +/- (cm/yr)" = ((slope - cf_lower) * 365.25 / 10)) %>% 
  mutate("signifigant" = if_else(p.value > 0.049, "N", "Y")) %>% 
  select("forest", "slope_pos", "elevation change (cm/yr)", "error +/- (cm/yr)", "p.value", "signifigant")

# visualzie erosion rates in a table
print(erosion)
filter(erosion, slope_pos == "BS")
filter(erosion, slope_pos == "FS")


#check assumptions, they look okay
check_model(lm.temp, check = c("linearity", "homogeneity", "qq", "normality"))

#' plot estimates to look for broader trends
ggplot(data = stats.df, mapping = aes(x = worms, y = slope)) +
  geom_point(aes(color = forest)) +
  geom_smooth(method = "lm", formula = slope ~ worms, color = "black") +
  facet_wrap(~slope_pos) # points

#' plot estimates to look for broader trends
ggplot(data = stats.df, mapping = aes(x = worms, y = slope)) +
  geom_boxplot(aes(fill = worms)) +
  geom_smooth(method = "lm", formula = slope ~ worms, color = "black") + 
  facet_wrap(~slope_pos) # boxplot


#' #'############################### [stats - MLM forests] ################################
#' 
#' #' [fitting a random intercept and slope model, clustering by forest]
#' 
#' forest.bs <- filter(forest.t, slope_pos == "BS") # filter for BS data
#' 
#' # random intercept only
#' lmer.fr <- lmer(data = forest.t, mm ~ dayof + (1 + dayof | forest)) 
#' summary(lmer.fr)
#' 
#' confint(lmer.fr, oldNames = FALSE)
#' 
#' ranef(lmer.fr)
#' 
#' 
#' #' [fit contrasts to a lm to test for signifigance]
#' 
#' # fit lm
#' lm.forest <- lm(data = forest.t, mm ~ dayof + forest)
#' 
#' # manual matrix multiplication for contrasts between ASH and LRE - for verification
#' cmat <- c(0, 1, -1, 0, 0, 0, 0)
#' cmat%*%coef(lm.forest) # estimate of ASH - LRE
#' Sigma_b <- vcov(lm.forest) 
#' (SEcontrast <- sqrt(t(cmat)%*%Sigma_b%*%(cmat))) # se = sqrt(variance)
#' 
#' # easy way, get contrasts between all forests accounting for dayof
#' pairs(emmeans(lm.forest, "forest"), adjust = "none")
#' 
#' # fit lm
#' lm.worms <- lm(data = forest.t, mm ~ dayof + forest + worms)
#' summary(lm.worms)
#' 
#' 
#' 
#' #' [perform ANOVA and Tukey's test to find sig, between forests] 
#' 
#' # fit lm
#' forest.lm <- lm(data = forest.t, mm ~ dayof + forest)
#' summary(forest.lm)
#' 
#' # fit ANOVA
#' forest.aov <- aov(forest.lm)
#' 
#' # Tukey's test, show no signifigance
#' TukeyHSD(forest.aov, conf.level=.95)
#' 
#' 
#' 
#' #' [DISREGARDING slope_pos: fitting lms to get the erosion rates WITH standard errors for each forest] 
#' 
#' # note: all the dates have the same sample size, n, and should have the same variance. however, I think fitting a lm to get slope (erosion rate) and then doing more analysis later is important.
#' 
#' # prep values
#' forests <- c("ASH", "LRE", "LRW", "MAG", "WD", "LRJ",
#'              "RCE", "LME", "IH", "RCJ", "LMJ", "NH", "PLH") # list of all forests
#' place.h = rep(NA, times = length(forests))
#' 
#' # create a df to hold summary coef
#' stats.df <- data.frame("forest" = forests,
#'                        "slope" = c(place.h),
#'                        "intercept" = c(place.h),
#'                        "SE" = c(place.h),
#'                        "p-value" = c(place.h),
#'                        "cf_lower" = c(place.h),
#'                        "cf_upper" = c(place.h),
#'                        "worms" = c("EW", "EW", "EW", "JW", "JW", "JW", "EW", "EW", "EW", "EW", "JW", "JW", "JW"))
#' 
#' # for loop to run lm and store summary statistics in the df made above
#' for(i in 1:length(forests)){
#'   lm.temp <- lm(data = forest.t %>% filter(forest == forests[i] ), mm ~ dayof)
#'   stats.df[i, 2] = coef(lm.temp)[2] # slope
#'   stats.df[i, 3] = coef(lm.temp)[1] # intercept
#'   stats.df[i, 4] = summary(lm.temp)$coefficients[2, 2] # SE
#'   stats.df[i, 5] = summary(lm.temp)$coefficients[2, 4] #p-value
#'   stats.df[i, 6] = confint(lm.temp)[2,1] # lower confint
#'   stats.df[i, 7] = confint(lm.temp)[2,2] # upper confint
#' }
#' 
#' print(stats.df)

#' #'############################### [stats - worms] ################################
#' 
#' # working df
#' worms.a <- output
#' 
#' ggplot(data = worms.a, mapping = aes(x = worms, y = dmdt, fill = worms)) +
#'   geom_boxplot()
#' 
#' ggplot(data = worms.a, mapping = aes(x = worms, y = (dmdt / 10 * 365.25), fill = forest)) +
#'   geom_boxplot()
#' 
#' 
#' 
#' #'############################### [old junk] ################################
#' 
#' # FOREST SPECEFIC boxplot for dhdt
#' ggplot(data = masteri, mapping = aes(x = forest, y = dhdt, fill = worms)) +
#'   geom_boxplot() +
#'   facet_wrap(~slope_pos) +
#'   scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
#'   scale_y_continuous(name = "Change in Pins (cm / yr)") +
#'   scale_x_discrete(name = "Forest") +
#'   theme(panel.grid.major = element_blank(),
#'         panel.grid.minor = element_blank()) +
#'   labs(fill = "Worms")
#' 
#' # WORM boxplot for abs_dmm/dt
#' ggplot(data = masteri, mapping = aes(x = worms, y = dhdt, fill = worms)) +
#'   geom_boxplot() +
#'   facet_wrap(~slope_pos)+ 
#'   scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
#'   scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
#'   scale_y_continuous(name = "Change in Pins (cm / yr)") +
#'   scale_x_discrete(name = "Worms") +
#'   theme(panel.grid.major = element_blank(),
#'         panel.grid.minor = element_blank(),
#'         legend.position = "none")
#' 
#' # FOREST SPECEFIC boxplot for dhdt
#' ggplot(data = masteri, mapping = aes(x = forest, y = dhdt, fill = worms)) +
#'   geom_boxplot() +
#'   facet_wrap(~slope_pos) +
#'   scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
#'   scale_y_continuous(name = "Change in Pins (cm / yr)") +
#'   scale_x_discrete(name = "Forest") +
#'   theme(panel.grid.major = element_blank(),
#'         panel.grid.minor = element_blank()) +
#'   labs(fill = "Worms")
#' 
#' ###############################non -abs
#' masteri <- master %>% 
#'   group_by(forest, transect, slope_pos, pin) %>%
#'   mutate(dhdt = abs_dmm / dt) %>% 
#'   summarize(dhdt_ = sum(dhdt), abs_dh = sum(abs_dmm), worms = first(worms)) %>% 
#'   mutate(dhdt = dhdt_ * 365 / 10 ) # dhdt in cm/yr
#' 
#' 
#' # arrange the data for later plotting
#' masteri$forest <- factor(masteri$forest, 
#'                          levels=c("ASH", "LRE", "LRW", "MAG", "WD", "LRJ"))
#' 
#' 
#' 
#' # FOREST SPECEFIC boxplot for dhdt
#' ggplot(data = masteri, mapping = aes(x = forest, y = dhdt, fill = worms)) +
#'   geom_boxplot() +
#'   facet_wrap(~slope_pos) +
#'   scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
#'   scale_y_continuous(name = "Change in Pins (cm / yr)") +
#'   scale_x_discrete(name = "Forest") +
#'   theme(panel.grid.major = element_blank(),
#'         panel.grid.minor = element_blank()) +
#'   labs(fill = "Worms")
#' 
#' # WORM boxplot for abs_dmm/dt
#' ggplot(data = masteri, mapping = aes(x = worms, y = dhdt, fill = worms)) +
#'   geom_boxplot() +
#'   facet_wrap(~slope_pos)+ 
#'   scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
#'   scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
#'   scale_y_continuous(name = "Change in Pins (cm / yr)") +
#'   scale_x_discrete(name = "Worms") +
#'   theme(panel.grid.major = element_blank(),
#'         panel.grid.minor = element_blank(),
#'         legend.position = "none")
#' 
#' # FOREST SPECEFIC boxplot for dhdt
#' ggplot(data = masteri, mapping = aes(x = forest, y = dhdt, fill = worms)) +
#'   geom_boxplot() +
#'   facet_wrap(~slope_pos) +
#'   scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
#'   scale_y_continuous(name = "Change in Pins (cm / yr)") +
#'   scale_x_discrete(name = "Forest") +
#'   theme(panel.grid.major = element_blank(),
#'         panel.grid.minor = element_blank()) +
#'   labs(fill = "Worms")
#' 
#' lmer1 <- lmer(data = masteri, dhdt ~ worms + (1 | forest))
#' 
#' lm1 <- lm(data = masteri, dhdt ~ worms + forest)
#' 
#' summary(lm1)
#' 
#' #'###################### [non-abs] 
#' 
#' # master
#' masterj <- master %>% 
#'   group_by(forest, transect, slope_pos, pin) %>%
#'   mutate(dhdt = dmm / dt) %>% 
#'   summarize(dhdt_ = sum(dhdt), dh = sum(dmm), worms = first(worms)) %>% 
#'   mutate(dhdt = dhdt_ * 365 / 10 ) # dhdt in cm/yr
#' 
#' 
#' # arrange the data for later plotting
#' masterj$forest <- factor(masteri$forest, 
#'                          levels=c("ASH", "LRE", "LRW", "MAG", "WD", "LRJ"))
#' 
#' 
#' 
#' # FOREST SPECEFIC boxplot for dhdt
#' ggplot(data = masterj, mapping = aes(x = forest, y = dhdt, fill = worms)) +
#'   geom_boxplot() +
#'   facet_wrap(~slope_pos) +
#'   scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
#'   scale_y_continuous(name = "Erosion Rate (cm / yr)") +
#'   scale_x_discrete(name = "Forest") +
#'   theme(panel.grid.major = element_blank(),
#'         panel.grid.minor = element_blank()) +
#'   labs(fill = "Worms")
#' 
#' # WORM boxplot for abs_dmm/dt
#' ggplot(data = masteri, mapping = aes(x = worms, y = dhdt, fill = worms)) +
#'   geom_boxplot() +
#'   facet_wrap(~slope_pos)+ 
#'   scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
#'   scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
#'   scale_y_continuous(name = "Change in Pins (cm / yr)") +
#'   scale_x_discrete(name = "Worms") +
#'   theme(panel.grid.major = element_blank(),
#'         panel.grid.minor = element_blank(),
#'         legend.position = "none")
#' 
#' # FOREST SPECEFIC boxplot for dhdt
#' ggplot(data = masteri, mapping = aes(x = forest, y = dhdt, fill = worms)) +
#'   geom_boxplot() +
#'   facet_wrap(~slope_pos) +
#'   scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
#'   scale_y_continuous(name = "Change in Pins (cm / yr)") +
#'   scale_x_discrete(name = "Forest") +
#'   theme(panel.grid.major = element_blank(),
#'         panel.grid.minor = element_blank()) +
#'   labs(fill = "Worms")
#' 
#' 
#' #'###################### [for aggregate data] 
#' 
#' # group and get averages for each array for each time step
#' master_grouped <- master %>% 
#'   group_by(date_date, forest, transect, slope_pos) %>% 
#'   summarize(mean_dmm = mean(dmm), mean_abs_dmm = mean(abs_dmm), dt = mean(dt), worms = first(worms), site = first(site))
#' 
#' # group and get averages for forest, BS and FS, for whole summer
#' master_net <- master_grouped %>% 
#'   group_by(forest, transect, slope_pos) %>% # group
#'   summarize(net_dmm = sum(mean_dmm), net_abs_dmm = sum(mean_abs_dmm), dt = sum(dt), worms = first(worms), site = first(site)) %>% # averages
#'   mutate(ID = paste(forest, transect, slope_pos, sep = "_")) %>%  # make an index column
#'   mutate(dmm_dt = net_dmm / dt, admm_dt = net_abs_dmm / dt) %>% # calculate dmm/dt
#'   filter(slope_pos == "BS")
#' 
#' 
#' print(master_net, n = 36)
#' 
#' lm1 <- lm(data = master_net, admm_dt ~ worms + forest)
#' 
#' summary(lm1)
#' 
#' # arrange the data for later plotting
#' master_net$forest <- factor(master_net$forest, levels=c("ASH", "LRE", "LRW", "MAG", "WD", "LRJ"))
#' 
#' 
#' # FOREST SPECEFIC boxplot for abs_dmm/dt
#' ggplot(data = master_net, mapping = aes(x = forest, y = admm_dt, fill = worms)) +
#'   geom_boxplot() +
#'   facet_wrap(~slope_pos) +
#'   scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3"))
#' 
#' 
#' # WORM boxplot for abs_dmm/dt
#' ggplot(data = master_net, mapping = aes(x = worms, y = admm_dt, fill = worms)) +
#'   geom_boxplot() +
#'   facet_wrap(~slope_pos)+ 
#'   scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3"))
#' 
#' 
#' 
#' 
#' # create a boxplot for dmm/dt
#' ggplot(data = master_net, mapping = aes(x = forest, y = dmm_dt, fill = worms)) +
#'   geom_boxplot() +
#'   facet_wrap(~slope_pos)
#' 
#' # WORM boxplot for dmm/dt
#' ggplot(data = master_net, mapping = aes(x = worms, y = dmm_dt, fill = worms)) +
#'   geom_boxplot() +
#'   facet_wrap(~slope_pos)
#' 
#' 
#' 
#' 
#' ######################################
#' 
#' # plot dmm/dt
#' ggplot(data = master_net, mapping = aes(x = ID, y = dmm_dt, fill = worms)) +
#'   geom_bar(stat = "identity") +
#'   facet_wrap(~slope_pos) +
#'   theme_light()
#' 
#' ?geom_bar
#' 
#' hist(master_summary$mean_dmm, breaks = 20)
#' hist(master$dmm, breaks = 20)
#' 
#' 
#' 
#' 
#' head(master_time)