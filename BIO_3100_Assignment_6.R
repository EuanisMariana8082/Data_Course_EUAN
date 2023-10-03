library(tidyverse)
dat <- read_csv("C:/Users/Rushe/Desktop/Data_Course_EUAN/Data/BioLog_Plate_Data.csv")
# make sure that the variables names are correct.
dat_long <- dat %>%
  pivot_longer(cols = c("Hr_24", "Hr_48", "Hr_144"),
               names_to = "time",
               values_to = "absorbance")
#making a column using data_long/mutate classifying the sample ID
dat_long <- dat_long %>%
  mutate(sample_type = case_when(
    str_detect(`Sample ID`, "Soil_1") ~ "Soil_1",
    str_detect(`Sample ID`, "Soil_2") ~ "Soil_2",
    str_detect(`Sample ID`, "Clear_Creek") ~ "Clear_Creek",
    str_detect(`Sample ID`, "Waste_Water") ~ "Waste_Water"
  ))
# Filter the data for a dilution of 0.1
dat_filtered <- dat_long %>%
  filter(Dilution == 0.1)
# make the plot for the absorbency of 0.1
dat_long %>%
  filter(Dilution == 0.1) %>%
  ggplot(aes(x = time, y = absorbance)) +
  geom_line(aes(group = interaction(`Sample ID`, Dilution)), alpha = 0.5) +
  stat_summary(fun = mean, geom = "line", size = 1, color = "tan") +
  labs(x = "Time (hours)", y = "Absorbance")

#plotting, using flitering,stat_summary and basics for plot 
ggplot(dat_filtered, aes(x = time, y = absorbance, color = sample_type, group = interaction(`Sample ID`, Dilution))) +
  geom_line(alpha = 0.5) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  scale_color_manual(values = c("Soil_1" = "blue", "Soil_2" = "green", "Clear_Creek" = "red", "Waste_Water" = "purple")) +
  labs(x = "Time (hours)", y = "Absorbance", color = "Sample Type") +
  theme_minimal()


# now doing task 4 
# loading gganimate package 
if (!require(tidyverse)) {
  install.packages("tidyverse")
}
library(tidyverse)

if (!require(gganimate)) {
  install.packages("gganimate")
}
library(gganimate)
library(ggplot2)
# Install the gganimate package if you haven't already
if (!require(gganimate)) {
  install.packages("gganimate")
}

# Load the necessary libraries
library(ggplot2)
library(gganimate)
# using filter to get Itaconic Acid 
dat_itaconic <- dat_long %>%
  filter(Substrate == "Itaconic Acid")
# use mean  and summarise
dat_itaconic_mean <- dat_itaconic %>%
  group_by(`Sample ID`, time) %>%
  summarise(absorbance = mean(absorbance), .groups = "drop")
# Install the gganimate package if you haven't already
if (!require(gganimate)) {
  install.packages("gganimate")
}

# libraries
library(ggplot2)
library(gganimate)

# animated plot
p <- ggplot(dat_itaconic_mean, aes(x = time, y = absorbance, group = `Sample ID`, color = sample_type)) +
  geom_line() +
  transition_reveal(time) +
  labs(x = "Time (hours)", y = "Mean Absorbance")

# Render the animation
animate(p)


#  you can also do it this way 
library(tidyverse)
library(gganimate)

# Read the data
dat <- read_csv("C:/Users/Rushe/Desktop/Data_Course_EUAN/Data/BioLog_Plate_Data.csv")

# Transform into long format
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



