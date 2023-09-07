# Assignment 2
#I need to read the csv_files first
csv_files <- list.files(path="Data",pattern = ".csv")
csv_files
#Find how many files match that description using the length() function
#length of the csv_files
?length()
length(csv_files)
length(1:10)
#Open the wingspan_vs_mass.csv file and store the contents as an R object named “df” using the read.csv() function
df <- read.csv("./Data/wingspan_vs_mass.csv")
# look at first five lines #Inspect the first 5 lines of this data set using the head() function
head(df,n=5)
?head
list.files(path = "Data" ,
           pattern = "^b",
           recursive = TRUE,
           full.names = TRUE)
?list.files()
#9 Write a command that displays the first line of each of those “b” files (this is tricky… use a for-loop)
b <- list.files(recursive = TRUE,
                path = "Data",
                pattern = "^b" ,
                full.names = TRUE)
readLines("Data/data-shell/creatures/basilisk.dat",
          n=1)
readLines("Data/data-shell/data/pdb/benzaldehyde.pdb",
           n=1)
readLines("Data/Messy_Take2/b_df.csv",
          n=1)
#find all files that start with b 
bfiles
for(i in bfiles){
print(readLines(i,n=1)  )
}

x <- 1:10
for (i in x){
  print(i*2)
}
x*2

# 10 for all ".csv" files #Do the same thing for all files that end in “.csv”
for(i in list.files(path = "Data",full.names = TRUE,
                    recursive = "TRUE,pattern = .csv")){
}
