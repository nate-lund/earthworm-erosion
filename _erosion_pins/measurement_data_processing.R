#'############################### [setup] ################################

# libraries needed
libs <- c("lidR", "shapefiles", "sf", "terra", "raster", "tidyr", "dplyr", "ggplot2", "easypackages", "spatialEco","here", "performance", "RColorBrewer", "lme4", "nlme", "readxl", "writexl")

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
input <- read_excel(hert("working_measurements-data.xlsx"), sheet = "2025_4R")

# preview
head(input)

# we need to first average the duplicate measurements for each pin.
input$mm_ch <- (input$mm1_ch + input$mm2_ch) / 2 # for changes
input$mm_bl <- (input$mm1_bl + input$mm2_bl) / 2 # for baseline

# convert date to days past
input$date <- as.Date(input$date) # format date column
input$dayof <- as.numeric(input$date) - 20283 + 195 # convert to days since 1970-01-01

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
write_xlsx(input, hert("input.xlsx"))

head(input)

#'############################### [differencing pins] ################################

# number of unique pins, should be 216
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
}

input_list

#  rebuild the data frame
output <- input_list[[1]] # jank start to the for loop, needs a base
for(i in 2:nindex) { # for loop to stack dataframes one list at a time
  output <- merge(output, input_list[[i]], all = TRUE)
} 

# write to an excel file
write_xlsx(output, hert("output.xlsx"))



#'############################### [QAQC] ################################

# clean up data and remove inital mms
output <- output %>%
  mutate(abs_dmm = abs(dmm)) %>% # create abs_dmm column
  filter(dt > 0) # filter to remove inital sites


hist(output$dmm, breaks = 30)


# clean up data frame to remove outliers (greater than 3sd from the mean)
output <- output %>%
  mutate(abs_dmm = abs(dmm)) %>% # create abs_dmm column
  filter(abs_dmm < sd(master_df$dmm)*3 & dt > 0) # filter to remove inital sites and outliers



#'###################### [for individual pin data]

masteri <- master %>% 
  group_by(forest, transect, slope_pos, pin) %>%
  mutate(dhdt = abs_dmm / dt) %>% 
  summarize(dhdt_ = sum(dhdt), abs_dh = sum(abs_dmm), worms = first(worms)) %>% 
  mutate(dhdt = dhdt_ * 365 / 10 ) # dhdt in cm/yr


# arrange the data for later plotting
masteri$forest <- factor(masteri$forest, 
                         levels=c("ASH", "LRE", "LRW", "MAG", "WD", "LRJ"))



# FOREST SPECEFIC boxplot for dhdt
ggplot(data = masteri, mapping = aes(x = forest, y = dhdt, fill = worms)) +
  geom_boxplot() +
  facet_wrap(~slope_pos) +
  scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
  scale_y_continuous(name = "Change in Pins (cm / yr)") +
  scale_x_discrete(name = "Forest") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(fill = "Worms")

# WORM boxplot for abs_dmm/dt
ggplot(data = masteri, mapping = aes(x = worms, y = dhdt, fill = worms)) +
  geom_boxplot() +
  facet_wrap(~slope_pos)+ 
  scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
  scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
  scale_y_continuous(name = "Change in Pins (cm / yr)") +
  scale_x_discrete(name = "Worms") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

# FOREST SPECEFIC boxplot for dhdt
ggplot(data = masteri, mapping = aes(x = forest, y = dhdt, fill = worms)) +
  geom_boxplot() +
  facet_wrap(~slope_pos) +
  scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
  scale_y_continuous(name = "Change in Pins (cm / yr)") +
  scale_x_discrete(name = "Forest") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(fill = "Worms")

###############################non -abs
masteri <- master %>% 
  group_by(forest, transect, slope_pos, pin) %>%
  mutate(dhdt = abs_dmm / dt) %>% 
  summarize(dhdt_ = sum(dhdt), abs_dh = sum(abs_dmm), worms = first(worms)) %>% 
  mutate(dhdt = dhdt_ * 365 / 10 ) # dhdt in cm/yr


# arrange the data for later plotting
masteri$forest <- factor(masteri$forest, 
                         levels=c("ASH", "LRE", "LRW", "MAG", "WD", "LRJ"))



# FOREST SPECEFIC boxplot for dhdt
ggplot(data = masteri, mapping = aes(x = forest, y = dhdt, fill = worms)) +
  geom_boxplot() +
  facet_wrap(~slope_pos) +
  scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
  scale_y_continuous(name = "Change in Pins (cm / yr)") +
  scale_x_discrete(name = "Forest") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(fill = "Worms")

# WORM boxplot for abs_dmm/dt
ggplot(data = masteri, mapping = aes(x = worms, y = dhdt, fill = worms)) +
  geom_boxplot() +
  facet_wrap(~slope_pos)+ 
  scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
  scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
  scale_y_continuous(name = "Change in Pins (cm / yr)") +
  scale_x_discrete(name = "Worms") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

# FOREST SPECEFIC boxplot for dhdt
ggplot(data = masteri, mapping = aes(x = forest, y = dhdt, fill = worms)) +
  geom_boxplot() +
  facet_wrap(~slope_pos) +
  scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
  scale_y_continuous(name = "Change in Pins (cm / yr)") +
  scale_x_discrete(name = "Forest") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(fill = "Worms")

lmer1 <- lmer(data = masteri, dhdt ~ worms + (1 | forest))

lm1 <- lm(data = masteri, dhdt ~ worms + forest)

summary(lm1)

#'###################### [non-abs] 

# master
masterj <- master %>% 
  group_by(forest, transect, slope_pos, pin) %>%
  mutate(dhdt = dmm / dt) %>% 
  summarize(dhdt_ = sum(dhdt), dh = sum(dmm), worms = first(worms)) %>% 
  mutate(dhdt = dhdt_ * 365 / 10 ) # dhdt in cm/yr


# arrange the data for later plotting
masterj$forest <- factor(masteri$forest, 
                         levels=c("ASH", "LRE", "LRW", "MAG", "WD", "LRJ"))



# FOREST SPECEFIC boxplot for dhdt
ggplot(data = masterj, mapping = aes(x = forest, y = dhdt, fill = worms)) +
  geom_boxplot() +
  facet_wrap(~slope_pos) +
  scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
  scale_y_continuous(name = "Erosion Rate (cm / yr)") +
  scale_x_discrete(name = "Forest") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(fill = "Worms")

# WORM boxplot for abs_dmm/dt
ggplot(data = masteri, mapping = aes(x = worms, y = dhdt, fill = worms)) +
  geom_boxplot() +
  facet_wrap(~slope_pos)+ 
  scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
  scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
  scale_y_continuous(name = "Change in Pins (cm / yr)") +
  scale_x_discrete(name = "Worms") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

# FOREST SPECEFIC boxplot for dhdt
ggplot(data = masteri, mapping = aes(x = forest, y = dhdt, fill = worms)) +
  geom_boxplot() +
  facet_wrap(~slope_pos) +
  scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3")) +
  scale_y_continuous(name = "Change in Pins (cm / yr)") +
  scale_x_discrete(name = "Forest") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(fill = "Worms")


#'###################### [for aggregate data] 

# group and get averages for each array for each time step
master_grouped <- master %>% 
  group_by(date_date, forest, transect, slope_pos) %>% 
  summarize(mean_dmm = mean(dmm), mean_abs_dmm = mean(abs_dmm), dt = mean(dt), worms = first(worms), site = first(site))

# group and get averages for forest, BS and FS, for whole summer
master_net <- master_grouped %>% 
  group_by(forest, transect, slope_pos) %>% # group
  summarize(net_dmm = sum(mean_dmm), net_abs_dmm = sum(mean_abs_dmm), dt = sum(dt), worms = first(worms), site = first(site)) %>% # averages
  mutate(ID = paste(forest, transect, slope_pos, sep = "_")) %>%  # make an index column
  mutate(dmm_dt = net_dmm / dt, admm_dt = net_abs_dmm / dt) %>% # calculate dmm/dt
  filter(slope_pos == "BS")


print(master_net, n = 36)

lm1 <- lm(data = master_net, admm_dt ~ worms + forest)

summary(lm1)

# arrange the data for later plotting
master_net$forest <- factor(master_net$forest, levels=c("ASH", "LRE", "LRW", "MAG", "WD", "LRJ"))


# FOREST SPECEFIC boxplot for abs_dmm/dt
ggplot(data = master_net, mapping = aes(x = forest, y = admm_dt, fill = worms)) +
  geom_boxplot() +
  facet_wrap(~slope_pos) +
  scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3"))


# WORM boxplot for abs_dmm/dt
ggplot(data = master_net, mapping = aes(x = worms, y = admm_dt, fill = worms)) +
  geom_boxplot() +
  facet_wrap(~slope_pos)+ 
  scale_fill_manual(values = c("JW" = "#D6604D", "EW"= "#4393C3"))




# create a boxplot for dmm/dt
ggplot(data = master_net, mapping = aes(x = forest, y = dmm_dt, fill = worms)) +
  geom_boxplot() +
  facet_wrap(~slope_pos)

# WORM boxplot for dmm/dt
ggplot(data = master_net, mapping = aes(x = worms, y = dmm_dt, fill = worms)) +
  geom_boxplot() +
  facet_wrap(~slope_pos)




######################################

# plot dmm/dt
ggplot(data = master_net, mapping = aes(x = ID, y = dmm_dt, fill = worms)) +
  geom_bar(stat = "identity") +
  facet_wrap(~slope_pos) +
  theme_light()

?geom_bar

hist(master_summary$mean_dmm, breaks = 20)
hist(master$dmm, breaks = 20)




head(master_time)