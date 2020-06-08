library(datasets)
data(iris)
#1
ques1Data <- tapply(iris$Sepal.Length, iris$Species, mean)
ques1 <- round(ques1["virginica"], 0)
#2
ques2 <- apply(iris[, 1:4], 2, mean)

library(datasets)
data(mtcars)
#3
ques3 <- sapply(split(mtcars$mpg, mtcars$cyl), mean)
tapply(mtcars$mpg, mtcars$cyl, mean )
with(mtcars, tapply(mpg, cyl, mean))
#4
ques4Data<- sapply(split(mtcars$hp, mtcars$cyl), mean)
ques4 <- abs(round(ques4Data["4"] - ques4Data["8"]))