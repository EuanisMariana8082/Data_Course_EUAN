# Assignment 2
csv_files <- list.files(path="Data",pattern = ".csv")
csv_files
?length()
length(csv_files)
length(1:10)
df <- read.csv("./Data/wingspan_vs_mass.csv")
head(df ,n=5)
?head
list.files(path = "Data" ,pattern = "^b",
           recursive = TRUE)
?list.files()
#9 
