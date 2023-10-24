# assignment 8 
#loading the packages to loaded into Rstudio 
library(modelr)
library(easystats)
library(broom)
library(tidyverse)
library(fitdistrplus)
# file path
file_path <- "C:/Users/Rushe/Desktop/Data_Course_EUAN/Data/mushroom_growth.csv"
# read the file 
data <- read.csv(file_path)
# looking at the data
glimpse(data)
#linear model 
mod1 = lm(GrowthRate ~ Light, data = data)
summary(mod1)
# ggplot2
library(ggplot2)

# plotting
ggplot(data, aes(x=Light, y=GrowthRate)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()
# defining the models 
# Define Model 1: GrowthRate explained by Light
model1 <- lm(GrowthRate ~ Light, data=data)

# Define Model 2: GrowthRate explained by Nitrogen
model2 <- lm(GrowthRate ~ Nitrogen, data=data)

# Define Model 3: GrowthRate explained by Humidity
model3 <- lm(GrowthRate ~ Humidity, data=data)

# Define Model 4: GrowthRate explained by Temperature
model4 <- lm(GrowthRate ~ Temperature, data=data)
# Define Model 5: GrowthRate explained by Light and Nitrogen
model5 <- lm(GrowthRate ~ Light + Nitrogen, data=data)

# comparing models 
# Calculating the mean squared error (MSE) for model1
mse_model1 <- mean(model1$residuals^2)
print(paste("MSE for model1 is", mse_model1))

# same thing for model2
mse_model2 <- mean(model2$residuals^2)
print(paste("MSE for model2 is", mse_model2))

# dumping the models that is the worst 
# modelr package being loaded int rstudio 
library(modelr)


# Adding predictions to the data
data <- data %>% 
  add_predictions(model1)  # replace model1 with your best model

# predictions 
# Select actual and predicted GrowthRate values
data %>% dplyr::select("GrowthRate","pred")
# Create a new data frame with hypothetical Light values
new_data <- data.frame(Light = seq(from = min(data$Light), to = max(data$Light), length.out = 100))

# Making predictions for the new data
new_data$pred <- predict(model1, newdata = new_data)  # replace model1 with your best model

# now we can plot the prediction 
# ggplot2
library(ggplot2)
# Plot actual GrowthRate values and predicted GrowthRate values
ggplot() +
  geom_point(data = data, aes(x = Light, y = GrowthRate)) +
  geom_line(data = new_data, aes(x = Light, y = pred), color = "hotpink") +
  theme_minimal()

# Define Model 3: GrowthRate explained by Light, Nitrogen, and Temperature
model3 <- lm(GrowthRate ~ Light + Nitrogen + Temperature, data=data)
# Put all models into a list
models <- list(model1=model1, model2=model2, model3=model3)
# loading the packages
library(ggplot2)
library(modelr)

# Gathering residuals from all three models 
data %>% 
  gather_residuals(model1, model2, model3) %>% 
  ggplot(aes(x=model,y=resid,fill=model)) +
  geom_boxplot(alpha=.5) +
  geom_point() + 
  theme_minimal()
# Gathering predictions from all three models
data %>% 
  gather_predictions(model1, model2, model3) %>% 
  ggplot(aes(x=Light, y=GrowthRate)) +  # replace 'Light' with the variable you're interested in
  geom_point(size=.7) +
  geom_point(aes(y=pred, color=model)) +
  geom_smooth(aes(y=pred, color=model)) +
  theme_minimal()
# loading easy stats
library(easystats)
# Generate a report for model3
report(model3)

#stats 
#  summary statistics for model1
summary(model1)

# summary statistics for model2
summary(model2)

# summary statistics for model3
summary(model3)

# summary statistics for model4
summary(model4)

# statistics for model5
summary(model5)

# Make predictions 
predictions <- predict(model1, newdata = data)  # replace 'model1' with your best model

# Print
print(predictions)
