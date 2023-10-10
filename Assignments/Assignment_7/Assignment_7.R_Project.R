#Assignment_7
#getwd()  
#Assignment_7.R_Project.R
#list.files()
library(tidyverse)
library(ggplot2)
# Setting working directory 
setwd("C:/Users/Rushe/Desktop/Data_Course_EUAN/Assignments/Assignment_7")
# Importing the CSV file
Utah_Religions_by_County <- read.csv("Utah_Religions_by_County.csv")
# use head to see what i have to work with 
head(Utah_Religions_by_County)
# Transform  data into tidy format
tidy_data <- Utah_Religions_by_County %>%
  pivot_longer(cols = -c(County, Pop_2010), names_to = "Religion", values_to = "Value")
#tidy_data <- Utah_Religions_by_County %>%
  #pivot_longer(cols = -County, names_to = "Religion", values_to = "Value")
#tidy_data <- Utah_Religions_by_County %>%
  #pivot_longer(cols = -County, names_to = "Religion", values_to = "Value")
#trying to explore the data 
# Plotting the proportion of each religion in each county
ggplot(tidy_data, aes(x = County, y = Value, fill = Religion)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 4 addressing the question Does population of a county correlate with the proportion of any specific religious group in that county?
# Correlation between population and proportion of a specific religion
specific_religion <- tidy_data %>% filter(Religion == 'Evangelical')
cor.test(specific_religion$Pop_2010, specific_religion$Value)
# Assuming you have a column named 'Population' in your data frame
# Correlation between proportion of a specific religion and non-religious people
non_religious <- tidy_data %>% filter(Religion == 'Non.Religious') 
cor.test(specific_religion$Value, non_religious$Value)
# Summary statistics for all columns
summary(tidy_data)
# Mean value for each religion
tidy_data %>% group_by(Religion) %>% summarise(Mean = mean(Value, na.rm = TRUE))
# Boxplot to see the distribution of values for each religion
ggplot(tidy_data, aes(x = Religion, y = Value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90))
# Scatter plot to visualize the correlation
ggplot(specific_religion, aes(x = Pop_2010, y = Value)) +
  geom_point() +
  labs(x = "Population", y = "Proportion", title = "Correlation between Population and Proportion of Evangelical")
#QUESTIONS FOR THE ASSIGNMENT 
#1.Does population of a county correlate with the proportion of any specific religious group in that county?” No,There
#for question 1). It seems to have a cluster in a section and then a linear line with an out lier right side
#95 percent confidence interval:-0.3331612  0.3989346 sample estimates:cor 0.03798236. NO, the confidence intervals are far from cor 0.03798236.
#use filter for a specific religion 
specific_religion <- tidy_data %>% filter(Religion == 'Evangelical')
# correlation test
correlation_test <- cor.test(specific_religion$Pop_2010, specific_religion$Value)
# Print corr
print(correlation_test)
#2.Does proportion of any specific religion in a given county correlate with the proportion of non-religious people?”
# for question 2). It seems like there is because of the 95 percent confidence interval: 0.03126845 0.66405900 sample estimates: cor 0.3932673 which falls into range with the 95% confidence interval. 
# Use filter for non-religious people
non_religious <- tidy_data %>% filter(Religion == 'Non.Religious')
# correlation test
correlation_test <- cor.test(specific_religion$Value, non_religious$Value)
# Print 
print(correlation_test)






