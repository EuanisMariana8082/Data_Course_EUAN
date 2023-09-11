?iris
iris[1,]
View(iris)
mean(iris$Petal.Width)
summary(iris$Petal.Width)
names(iris)
#length * width
iris$Petal.Length *iris$Petal.Width
iris$Petal.Area <- iris$Petal.Length * iris$Petal.Width

iris[,"sepal.Length"]
for(i in names(iris)){
  x <- iris[,i]
  print(summary(x))
}

x <- c("sucks","is stupid", "is cool")
for (i in x){
  print(paste0("Your mom",i))
}