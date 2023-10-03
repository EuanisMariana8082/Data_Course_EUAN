# today's plot  this is the one that ran the plot might use this one USE THIS ONE!!
# Load the necessary libraries
library(tidyverse)
library(gganimate)

# Read the data
dat <- read_csv("C:/Users/Rushe/Desktop/Data_Course_EUAN/Data/BioLog_Plate_Data.csv")

# long format
dat_long <- dat %>%
  pivot_longer(cols = c("Hr_24", "Hr_48", "Hr_144"),
               names_to = "time",
               values_to = "absorbance")

#  new column to classify the sample type
dat_long <- dat_long %>%
  mutate(sample_type = case_when(
    str_detect(`Sample ID`, "Soil_1") ~ "Soil_1",
    str_detect(`Sample ID`, "Soil_2") ~ "Soil_2",
    str_detect(`Sample ID`, "Clear_Creek") ~ "Clear_Creek",
    str_detect(`Sample ID`, "Waste_Water") ~ "Waste_Water"
  ))

# Filter the data for a dilution of 0.1 and substrate "Itaconic Acid"
dat_filtered <- dat_long %>%
  filter(Dilution == 0.1, Substrate == "Itaconic Acid")

# Calculate mean absorbance values for each group
dat_mean <- dat_filtered %>%
  group_by(time, sample_type) %>%
  summarise(mean_absorbance = mean(absorbance))

# Convert 'time' to numeric
dat_mean$time <- as.numeric(gsub("Hr_", "", dat_mean$time))

# animated plot
ggplot(dat_mean, aes(x = time, y = mean_absorbance, color = sample_type)) +
  geom_line() +
  labs(x = "Time (hours)", y = "Mean Absorbance", color = "Sample Type") +
  theme_minimal() +
  transition_reveal(time)
# the second part for the multiple plots 
# Load the necessary libraries
library(tidyverse)
library(gganimate)

# Read the data
dat <- read_csv("C:/Users/Rushe/Desktop/Data_Course_EUAN/Data/BioLog_Plate_Data.csv")

# data into a long format
dat_long <- dat %>%
  pivot_longer(cols = c("Hr_24", "Hr_48", "Hr_144"),
               names_to = "time",
               values_to = "absorbance")

# Adding new column to classify the sample type
dat_long <- dat_long %>%
  mutate(sample_type = case_when(
    str_detect(`Sample ID`, "Soil_1") ~ "Soil_1",
    str_detect(`Sample ID`, "Soil_2") ~ "Soil_2",
    str_detect(`Sample ID`, "Clear_Creek") ~ "Clear_Creek",
    str_detect(`Sample ID`, "Waste_Water") ~ "Waste_Water"
  ))

#using filter the data for a dilution of 0.1
dat_filtered <- dat_long %>%
  filter(Dilution == 0.1)

# Calculate mean absorbance values for each group
dat_mean <- dat_filtered %>%
  group_by(time, sample_type, Substrate) %>%
  summarise(mean_absorbance = mean(absorbance), .groups = "drop") %>%
  complete(time, sample_type, Substrate, fill = list(mean_absorbance = NA))

# Converting 'time' to numeric
dat_mean$time <- as.numeric(gsub("Hr_", "", dat_mean$time))

# Generating the plot
ggplot(dat_mean, aes(x = time, y = mean_absorbance, color = sample_type)) +
  geom_line() +
  facet_wrap(sample_type ~ Substrate) +
  labs(x = "Time (hours)", y = "Mean Absorbance", color = "Sample Type") +
  theme_minimal()

