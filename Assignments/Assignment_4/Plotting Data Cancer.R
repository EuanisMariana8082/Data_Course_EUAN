# Load the necessary library
#library(ggplot2)
# Read the Excel file
#data <- read_excel("C:/Users/Rushe/Desktop/Data_Course_EUAN/Assignments/Assignment_4/cancer geno stats.xlsx")

# Assuming 'column1' and 'column2' are the columns you want to plot
#ggplot(data, aes(x=Name, y=Mutations)) + geom_point()


# how do I space this plot
# another edited one 
# Load library
library(ggplot2)

# columns to plot
p <- ggplot(data, aes(x=Name, y=Mutations))

# layer for points and color
p <- p + geom_point(color = "tan")

# title on plot
p <- p + ggtitle("Cancer Genomics Names and Number of Mutations")
# Rotate x-axis text
p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.9, hjust=1))

# print the plot
print(p)
