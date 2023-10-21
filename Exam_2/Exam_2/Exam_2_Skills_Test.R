# BIOL_3100_Exam_2
library(ggplot2)
library(tidyverse)
library(easystats)
library(tidyr)
# Set the working directory to Desktop
setwd("C:/Users/Rushe/Desktop/Data_Course_EUAN/Exam_2/Exam_2")
# Read the CSV file
data <- read.csv("unicef-u5mr.csv")
# Print the number of rows and columns
#dim(data)
# Print the number of rows
#print(paste("Number of rows: ", nrow(data)))
# Print the number of columns
#print(paste("Number of columns: ", ncol(data)))
# Print the names of the rows
#print(paste("Row names: ", rownames(data)))
# Print the names of the columns
#print(paste("Column names: ", colnames(data)))
# Print the names of the variables
#print(paste("Variable names: ", colnames(data)))
#TASK 2 TIDY VERSE 
# task 2 tidy verse   THIS PART RAN 
# Load the library
library(tidyverse)
# Convert the data to long format
data_long <- data %>%
  pivot_longer(cols = starts_with("U5MR."),
               names_to = "Year",
               values_to = "U5MR")
# task 3 Plot each country’s U5MR over time
# Create a plot: THIS PART RAN 
#ggplot(data_long, aes(x = Year, y = U5MR, group = CountryName)) +
  #geom_line() +
  #facet_wrap(~ Continent) +
  #labs(title = "Under-5 Mortality Rate Over Time", x = "Year", y = "Mortality Rate")

# Create a plot this one is good DON'T CHANGE 
ggplot(data_long, aes(x = Year, y = U5MR, group = CountryName)) +
  geom_line() +
  facet_wrap(~ Continent) +
  scale_x_discrete(breaks = c("U5MR.1960", "U5MR.1980", "U5MR.2000")) +
  labs(title = "Under-5 Mortality Rate Over Time", x = "Year", y = "Mortality Rate")

#TASK 4 Save this plot as LASTNAME_Plot_1.png 
#5. Create another plot that shows the mean U5MR for all the countries within a given continent at each year (20 pts)
# Save the plot
ggsave(filename = "EUAN_Plot_1.png")

# Load the necessary libraries
library(tidyverse)

# Read in the UNICEF data
data <- read.csv("unicef-u5mr.csv")

# Converting the data into long format
data_long <- data %>%
  pivot_longer(cols = starts_with("U5MR."),
               names_to = "Year",
               values_to = "U5MR")

# Converting Year to numeric after removing "U5MR." prefix
data_long$Year <- as.numeric(str_remove(data_long$Year, "U5MR."))

# Creating the plot
plot <- ggplot(data_long, aes(x = Year, y = U5MR, group = CountryName)) +
  geom_line() +
  facet_wrap(~ Continent) +
  scale_x_continuous(breaks = c(1960, 1980, 2000)) +
  labs(title = "Under-5 Mortality Rate Over Time", x = "Year", y = "Mortality Rate")

# Print plot
print(plot)

# Save the plot as a PNG file
ggsave(filename = "EUAN_Plot_1.png", plot = plot)

# TASK 5 PLOT WITH MANY COLORS 
# Create another plot that shows the mean U5MR for all the countries within a given continent at each year (20 pts)
# Calculate mean U5MR for each continent and year
data_mean <- data_long %>%
  group_by(Continent, Year) %>%
  summarise(MeanU5MR = mean(U5MR, na.rm = TRUE), .groups = "drop")

# Creating the plot
plot2 <- ggplot(data_mean, aes(x = Year, y = MeanU5MR, color = Continent)) +
  geom_line() +
  labs(title = "Mean Under-5 Mortality Rate Over Time", x = "Year", y = "Mean Mortality Rate")

# Print the plot
print(plot2)

# TASK 6 
# SAVING THE SECOND PLOT 
# Save the second plot
ggsave(filename = "EUAN_Plot_2.png", plot = plot2)

# TASK 7 
#Create three models of U5MR: You can use the lm() function in R to create linear regression models.
# Creating the 3 models
mod1 <- lm(U5MR ~ Year, data = data_long)
mod2 <- lm(U5MR ~ Year + Continent, data = data_long)
mod3 <- lm(U5MR ~ Year * Continent, data = data_long)

# TASK 8 COMPARING ALL MODELS 
# Compare the models
summary(mod1)
summary(mod2)
summary(mod3)

# THOUGHTS ON WHICH MODEL IS THE BEST ONE
# Include a comment line explaining which of these three models you think is best
# The best model is 2 becuase the R-squared is higher (0.6004) which means 60.04% of the variation in U5MR by the years and the contients than model 1 and model 3.

# TASK 9 
# Get the predicted values
data_long$pred_mod1 <- predict(mod1, data_long)
data_long$pred_mod2 <- predict(mod2, data_long)
data_long$pred_mod3 <- predict(mod3, data_long)

# Reshape the data to long format
data_long <- data_long %>%
  pivot_longer(cols = starts_with("pred_"),
               names_to = "Model",
               values_to = "Prediction")

# Creating a plot for each model
ggplot(data_long, aes(x = Year, y = Prediction, color = Model)) +
  geom_line() +
  labs(title = "Model Predictions", x = "Year", y = "Predicted U5MR")

# another option for task 9
# Creating a new data frame for the predictions
predictions <- expand.grid(Year = unique(data_long$Year),
                           Continent = unique(data_long$Continent),
                           Model = c("mod1", "mod2", "mod3"))

# the predicted values for each model
predictions$Prediction[predictions$Model == "mod1"] <- predict(mod1, newdata = predictions[predictions$Model == "mod1", ])
predictions$Prediction[predictions$Model == "mod2"] <- predict(mod2, newdata = predictions[predictions$Model == "mod2", ])
predictions$Prediction[predictions$Model == "mod3"] <- predict(mod3, newdata = predictions[predictions$Model == "mod3", ])

# CREATING A MODEL FOR EACH ONE 
ggplot(predictions, aes(x = Year, y = Prediction, color = Model)) +
  geom_line() +
  facet_wrap(~ Continent) +
  labs(title = "Model Predictions", x = "Year", y = "Predicted U5MR")

############### TASK 9 PLOT THAT I AM ACTUALLY USING 
# Create a plot for each model 
ggplot(predictions, aes(x = Year, y = Prediction, color = Continent)) +
  geom_line() +
  facet_grid(. ~ Model) +
  labs(title = "Model Predictions", x = "Year", y = "Predicted U5MR")

#TASK 10 EXTRA CREDIT 
# Creating a new data frame for the prediction
new_data <- data.frame(Year = 2020, Continent = "Americas", CountryName = "Ecuador")
# Predict the U5MR for Ecuador in 2020 using Model 2
new_data$Prediction <- predict(mod2, newdata = new_data)
# The real value for Ecuador for 2020
Reality <- 13
# Calculate the difference
Difference <- abs(new_data$Prediction - Reality)
# Print the result
print(paste("Model Prediction: ", new_data$Prediction))
print(paste("Reality: ", Reality))
print(paste("Difference: ", Difference))


