#'############################### [setup] ################################

# libraries needed
libs <- c("lidR", "shapefiles", "sf", "terra", "raster", "tidyr", "dplyr", "ggplot2", "easypackages", "spatialEco","here", "performance", "see", "RColorBrewer", "lme4", "nlme", "readxl", "writexl", "emmeans")

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

#'############################### [start / prepping df] ################################

# Pull in the data from box and some pre-processing.
input.a <- read_excel(hert("working_measurements-data.xlsx"), sheet = "2025_ARB-LR") # ARB and LR data

input.b <- read_excel(hert("working_measurements-data.xlsx"), sheet = "2025_Par-Sci") # Par Sci data

input <- merge(input.a, input.b, all = TRUE) # merge datasets


# preview
head(input)

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

head(input)

#'############################### [differencing pins] ################################

# number of unique pins
nindex <- length(unique(input$index))

# create a list, where each item is a data frame with all the measurements of each pin 
input_list <- vector(mode = "list", length = nindex) # create empty list
for(i in 1:nindex) {
  input_list[[i]] <- input %>% filter(index == i)
}

for(i in 1:nindex){ # out for loop selects each data frame (individual pin) in the list
  for(j in 2:6) { # inner for loop goes through each row of list [row, column]
    input_list[[i]][j,13] <- input_list[[i]][j, 2] - input_list[[i]][j - 1, 2] # subtracts today's day of from last measurement's
    input_list[[i]][j,14] <- input_list[[i]][j - 1, 12] - input_list[[i]][j, 11] # subtracts today's mm from last baseline
  }
  input_list[[i]] <- input_list[[i]] %>% drop_na(dt)
}

# create a cumulative sum for each pin, mm (baseline 0)
for (i in 1:nindex){
  input_list[[i]]$mm <- cumsum(input_list[[i]]$dmm)
}

#  rebuild the data frame
output <- input_list[[1]] # jank start to the for loop, needs a base
nlist <- length(input_list)

for(i in 2:nlist) { # for loop to stack dataframes one list at a time
  output <- merge(output, input_list[[i]], all = TRUE)
} 

# write to an excel file
write_xlsx(output, hert("_analysis/output.xlsx"))


#'############################### [QAQC] ################################

# clean up data
output.qc <- output %>%
  mutate(dmdt = ifelse(dt == 0, 0, dmm / dt)) %>% # create an erosion rate column (mm/day)
  mutate(abs = abs(dmdt)) # create abs_dmm column 

# arrange the data by forest for later plotting
output.qc$forest <- factor(output$forest, 
                        levels=c("ASH", "LRE", "LRW", "MAG", "WD", "LRJ",
                                 "RCE", "LME", "IH", "RCJ", "LMJ", "NH", "PLH")) # need to give it forests here

hist(output.qc$dmdt, breaks = 30)

head(output.qc)

#'############################### [stats - forests] ################################

#' [plotting pin elevation (mm, in mm) over time by forest] 

# create a forest_date column
forest.t <- output.qc %>%
  mutate(forest_date = interaction(forest, date, sep = "_"))

# create scatter plot showing mm over time 
ggplot(data = forest.t, mapping = aes(x = date, y = mm, color = forest, shape = slope_pos)) +
  geom_jitter() +
  facet_wrap(~forest) +
  geom_smooth(method = "lm", se = TRUE, color = "black")


# create a box plot showing mm over time with all forests 
ggplot(forest.t, aes(x = forest_date, y = mm, fill = forest)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#' [fitting lms for each forest and slope pos]

lm.ash <- lm(data = forest.t %>% filter(forest == "ASH"), mm ~ dayof * slope_pos)

summary(lm.ash)


#' [fitting lms to get the erosion rates WITH standard errors for each forest NOT distinguising between FS and BS] 

# note: all the dates have the same sample size, n, and should have the same variance. however, I think fitting a lm to get slope (erosion rate) and then doing more analysis later is important.

# prep values
forests <- c("ASH", "LRE", "LRW", "MAG", "WD", "LRJ",
             "RCE", "LME", "IH", "RCJ", "LMJ", "NH", "PLH") # list of all forests
place.h = rep(NA, times = length(forests))

# create a df to hold summary coef
stats.df <- data.frame("forest" = forests,
                       "slope" = c(place.h),
                       "intercept" = c(place.h),
                       "SE" = c(place.h),
                       "p-value" = c(place.h),
                       "cf_lower" = c(place.h),
                       "cf_upper" = c(place.h),
                       "worms" = c("EW", "EW", "EW", "JW", "JW", "JW", "EW", "EW", "EW", "EW", "JW", "JW", "JW"))

# for loop to run lm and store summary statistics in the df made above
for(i in 1:length(forests)){
  lm.temp <- lm(data = forest.t %>% filter(forest == forests[i] ), mm ~ dayof)
  stats.df[i, 2] = coef(lm.temp)[2] # slope
  stats.df[i, 3] = coef(lm.temp)[1] # intercept
  stats.df[i, 4] = summary(lm.temp)$coefficients[2, 2] # SE
  stats.df[i, 5] = summary(lm.temp)$coefficients[2, 4] #p-value
  stats.df[i, 6] = confint(lm.temp)[2,1] # lower confint
  stats.df[i, 7] = confint(lm.temp)[2,2] # upper confint
}

print(stats.df)

#check assumptions, they look okay
check_model(lm.temp, check = c("linearity", "homogeneity", "qq", "normality"))

#' plot estimates to look for broader trends [WIP, come back to this w/ more sites]
ggplot(data = stats.df, mapping = aes(x = worms, y = slope)) +
  geom_point(aes(color = forest)) +
  geom_smooth(method = "lm", formula = slope ~ worms, color = "black")

?geom_smooth

# don't really care about the intercept, since should be 0
ggplot(data = stats.df, mapping = aes(x = worms, y = intercept)) +
  geom_point()


# compute erosion values in cm/yr
erosion <- stats.df %>%
  mutate("elevation change (cm/yr)" = slope * 365.25 / 10) %>%
  mutate("error +/- (cm/yr)" = ((slope - cf_lower) * 365.25 / 10))

print(erosion)

#' [fitting a random intercept and slope model, clustering by forest]

lmer.fr <- lmer(data = forest.t, mm ~ dayof + (1 + dayof | forest)) 
summary(lmer.fr)

#' [fit contrasts to a lm to test for signifigance]

# fit lm
lm.forest <- lm(data = forest.t, mm ~ dayof + forest)

# manual matrix multiplication for contrasts between ASH and LRE - for verification
cmat <- c(0, 1, -1, 0, 0, 0, 0)
cmat%*%coef(lm.forest) # estimate of ASH - LRE
Sigma_b <- vcov(lm.forest) 
(SEcontrast <- sqrt(t(cmat)%*%Sigma_b%*%(cmat))) # se = sqrt(variance)

# easy way, get contrasts between all forests accounting for dayof
pairs(emmeans(lm.forest, "forest"), adjust = "none")

# fit lm
lm.worms <- lm(data = forest.t, mm ~ dayof + forest + worms)
summary(lm.worms)



#' [perform ANOVA and Tukey's test to find sig, between forests] 

# fit lm
forest.lm <- lm(data = forest.t, mm ~ dayof + forest)
summary(forest.lm)

# fit ANOVA
forest.aov <- aov(forest.lm)

# Tukey's test, show no signifigance
TukeyHSD(forest.aov, conf.level=.95)


#'############################### [stats - worms] ################################

# working df
worms.a <- output

ggplot(data = worms.a, mapping = aes(x = worms, y = dmdt, fill = worms)) +
  geom_boxplot()

ggplot(data = worms.a, mapping = aes(x = worms, y = (dmdt / 10 * 365.25), fill = forest)) +
  geom_boxplot()



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