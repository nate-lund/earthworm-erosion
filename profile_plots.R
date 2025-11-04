#'############################### [setup] ################################

# libraries needed
libs <- c("ggplot2", "dplyr", "tidyr")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
lapply(libs, library, character.only = T)

# enter the file path for the highest level folder you're working in 
data_folder <- "C:/Users/natha/Box/box_data/_data/_rusle2/"

# when a file is needed, call the hert() function
# for example; data_frame = read.csv(hert("more_data/measurements_data.csv"))
hert <- function(file) {
  file_path = paste(data_folder, file, sep = "")
  return(file_path)
}

#'############################### [plotting profiles] ################################

# there is some heterogeneity in MAG, so I looked at two, not a huge difference
mag.1 <- read.csv(hert("MAG_profile.csv"))
mag.2 <- read.csv(hert("MAG_profile2.csv"))

mags.df <- bind_rows(mag.1, mag.2, .id = "source")
ggplot(data = mags.df, mapping = aes(x = Distance, y = Elevation, size = pins))+
  geom_point() +
  facet_wrap(~source, scales = "free_y") +
  scale_size_manual(values = c(BS = 4, FS = 4, N = 1))


# load dfs
df.1 <- read.csv(hert("MAG_profile.csv"))
df.2 <- read.csv(hert("WD_profile.csv"))
df.3 <- read.csv(hert("LRJ_profile.csv"))
df.4 <- read.csv(hert("ASH_profile.csv"))
df.5 <- read.csv(hert("LRW_profile.csv"))
df.6 <- read.csv(hert("LRE_profile.csv"))

head(df.1)

# calculate slope at each point
df.1$slope <- diff(df.1$Elevation)

# stack dfs
profiles.a <- bind_rows(df.1, df.2, df.3, df.4, df.5, df.6, .id = "source")

# renames the .id column to forest name
profiles.a$forest = ifelse(profiles.a$source == 1, "MAG", ifelse(profiles.a$source == 2, "WD", ifelse(profiles.a$source == 3, "LRJ", ifelse(profiles.a$source == 4, "ASH", ifelse(profiles.a$source == 5, "LRW", "LRE")))))

head(profiles.a)


# plot
ggplot(data = profiles.a, mapping = aes(x = Distance, y = Elevation, size = pins))+
  geom_point() +
  facet_wrap(~forest, scales = "free_y") +
  scale_size_manual(values = c(BS = 4, FS = 4, N = 1))


