# Skills Test 1 (the real thing)
# I. Read the cleaned_covid_data.csv file into an R data frame.
# Read the CSV file into a data frame
#data<- read.csv("cleaned_covid_data.csv") # it didn't work

# Read the CSV file from a specific path into a data frame
data <- read.csv("/Users/Rushe/Desktop/BIOL3100_Exams/Exam_1/data/cleaned_covid_data.csv")

#II. Subset the data set to just show states that begin with “A” and save this as an object called A_states. (20 pts)
#Use the tidyverse suite of packages
#Selecting rows where the state starts with “A” is tricky (you can use the grepl() function or just a vector of those states if you prefer)

# use the library function to load tidyverse
library(tidyverse)

# Subset the data 
A_states <- data %>% 
  filter(str_starts(Province_State, "A"))
head(A_states)

# III.Create a plot of that subset showing Deaths over time, with a separate facet for each state. (20 pts)
# make sure to Create a scatterplot,Add loess curves WITHOUT standard error shading & ,Keep scales “free” in each facet
# start with using ggplot()
# Load the ggplot2 package
library(ggplot2)

# Converting the 'Last_Update' to Date class if it's not or it needs to be
A_states$Last_Update <- as.Date(A_states$Last_Update)

# using ggplot()to create a scatterplot
p <- ggplot(A_states, aes(x = Last_Update, y = Deaths)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ Province_State, scales = "free") +
  theme_minimal() +
  labs(title = "Deaths over time in states starting with 'A'",
       x = "Date",
       y = "Deaths")

# use the Print function to print the plot 
print(p)
names(A_states)

#IV. (Back to the full dataset) Find the “peak” of Case_Fatality_Ratio for each state and save this as a new data frame object called state_max_fatality_rate. (20 pts)
#I’m looking for a new data frame with 2 columns:
# “Province_State”
#“Maximum_Fatality_Ratio”
#Arrange the new data frame in descending order by Maximum_Fatality_Ratio

#finding the peak of Case_Fatality_Ratio of each of the states needed 
state_max_fatality_rate <- data %>%
  group_by(Province_State) %>%
  summarise(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>%
  arrange(desc(Maximum_Fatality_Ratio))

# to see the first few rows of state_max_fatality_rate
head(state_max_fatality_rate)


#V. Use that new data frame from task IV to create another plot.
#X-axis is Province_State
#Y-axis is Maximum_Fatality_Ratio
#bar plot
#x-axis arranged in descending order, just like the data frame (make it a factor to accomplish this)
#X-axis labels turned to 90 deg to be readable
#Even with this partial data set (not current), you should be able to see that (within these dates), different states had very different fatality ratios.

# Load ggplot2 package
library(ggplot2)

# Convert 'Province_State' to a factor and arrange it in descending order of 'Maximum_Fatality_Ratio'
state_max_fatality_rate$Province_State <- factor(state_max_fatality_rate$Province_State, levels = state_max_fatality_rate$Province_State[order(state_max_fatality_rate$Maximum_Fatality_Ratio, decreasing = TRUE)])

# Create a bar plot
p <- ggplot(state_max_fatality_rate, aes(x = Province_State, y = Maximum_Fatality_Ratio)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Maximum Fatality Ratio for each State",
       x = "State",
       y = "Maximum Fatality Ratio")

# using the print function in order to print my plot 
print(p)

#VI. (BONUS 10 pts) Using the FULL data set, plot cumulative deaths for the entire US over time
#You’ll need to read ahead a bit and use the dplyr package functions group_by() and summarize() to accomplish this.
# Load the tidyverse package
library(tidyverse)

# Read the CSV file into a data frame
data <- read_csv("C:/Users/Rushe/Desktop/BIOL3100_Exams/Exam_1/data/cleaned_covid_data.csv")

# Converting the 'Last_Update' to Date class if it's not
data$Last_Update <- as.Date(data$Last_Update)

# Calculate cumulative deaths for the entire US over time
cumulative_deaths <- data %>%
  group_by(Last_Update) %>%
  summarise(Cumulative_Deaths = sum(Deaths, na.rm = TRUE))
# make a plot showing the cumulative deaths overtime
p <- ggplot(cumulative_deaths, aes(x = Last_Update, y = Cumulative_Deaths)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Cumulative Deaths for the Entire US Over Time",
       x = "Date",
       y = "Cumulative Deaths")

# use the function print in order to show the plot
print(p)
