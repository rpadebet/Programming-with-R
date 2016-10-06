library(datasets)
data(iris)

tapply(iris$Sepal.Length,iris$Species,mean)

apply(iris[,1:4],2,mean)

library(datasets)
data(mtcars)
str(mtcars)

tapply(mtcars$mpg,mtcars$cyl,mean)
sapply(split(mtcars$mpg,mtcars$cyl),mean)
with(mtcars,tapply(mpg,cyl,mean))

hp_cyl<-tapply(mtcars$hp,mtcars$cyl,mean)
abs(hp_cyl[1]-hp_cyl[3])
ls()
undebug(ls)
