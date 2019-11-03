##########################
## Appendix A. R Basics ##
##########################

###########################
## A.1 Install and Usage ##
###########################

options(digits=3)
options(stringsAsFactors=FALSE)
options(show.signif.stars=FALSE)
options(prompt=">> ")
options(continue="++ ")

.libPaths("D:/MyWorkspaceR/myRLibrary")

.First <- function() {
  library(ggplot2)
  setwd("D:/MyWorkspaceR")
  cat("\nWelcome at ", date(), "\n")
}

.Last <- function() {
   cat("\nGoodbye at ", date(), "\n")
}

11 + 12 + 13

q()

fah <- readline("Fahrenheit? ")
fah <- as.numeric(fah)
cel <- (fah-32)/1.8
print(paste("Celsius =", cel))

source("FahCel.R")

#####################
## A.2 Environmnet ##
#####################

getwd()

setwd("D:/MyWorkspaceR")
getwd()

save.image()

ls()

rm(list=ls())
ls()

x <- 100
y <- c(2,3,5,7)
z <- 3.14
hero <- c("Superman", "Batman", "Spiderman")
f <- function(y) (y-32)/1.8
ls()
ls(pattern="e")

rm(z)
z

rm(x, y)

ls()
rm(list=ls())
ls()

hero <- c("Superman", "Batman", "Spiderman")
hero
save(hero, file="hero.rda")

rm(hero)
hero

load("hero.rda")
hero

history()

library()

Sys.getlocale()
Sys.setlocale("LC_ALL", "English_United States.1252")
library()
Sys.setlocale()

search()

xyplot(dist ~ speed, data=cars)

library(lattice)
search()
xyplot(dist ~ speed, data=cars)

detach(package:lattice)
search()

install.packages("ggplot2")
install.packages(ggplot2)

library("ggplot2")

.libPaths()

data()

data(package="ggplot2")

cars
head(cars)
tail(cars)
head(cars, 3)

help.start()

help(median)
?median

help(Syntax)
help(cars)

example(median)

help.search("xyplot")
??xyplot

help(xyplot, package="lattice")

RSiteSearch("topicmodels")

install.packages("sos")
library(sos)
findFn("social network analysis")

########################
## A.3 Data Structure ##
########################

## Vector

vec1 <- c(2, 3, 5, 7, 11)
vec2 <- c("cat", "dog", "fox", "horse", "pig", "rabbit")
vec3 <- c(TRUE, FALSE, TRUE)
vec4 <- c(4:-2)

vec2
vec2[3]
vec2[c(1,3,5)]
vec2[3:5]

c(1,2,3) + c(4,5,6)

c(1:3) + c(4:9)

1 + 2
"+"(1, 2)

10 + c(1,3,5)

fruit <- c("Apple", "Banana", "Strawberry")
food <- c("Pie", "Juice", "Cake")
paste(fruit, food)

paste(fruit, "Juice")

abs(-3:3)
sqrt(1:5)

ifelse(c(0, 25, 50, 75, 100) > 50, "pass", "fail")

## Factor

review <- c("Good", "Good", "Indifferent", "Bad", "Good", "Bad")
review
review.factor <- factor(review)
review.factor

str(review.factor)

as.numeric(review.factor)

levels(review.factor)

levels(review.factor) <- c("B", "G", "I")
review.factor

eval <- c("Medium", "Low", "High", "Medium", "High")
eval.ordered <- factor(eval, levels=c("Low", "Medium", "High"), order=TRUE)
eval.ordered

eval.factor <- factor(eval)
eval.factor

table(eval.factor)
table(eval.ordered)

sex <- c(2,1,2,2,1,0)
sex.factor <- factor(sex, levels=c(1, 2), labels=c("Male", "Female"))

sex.factor
table(sex.factor)

## Matrix

matrix(data=1:12, nrow=3, ncol=4)

rnames <- c("R1", "R2", "R3")
cnames <- c("C1", "C2", "C3", "C4")
matrix(1:12, 3, 4, dimnames=list(rnames, cnames), byrow=TRUE)

mat <- matrix(1:12, nrow=3)
mat
mat[2,]
mat[,3]
mat[c(1,2), c(2,4)]

mat[2,,drop=FALSE]

## Array

ary <- array(data=1:12, dim=c(2,3,2))
ary

array(data=1:48, dim=c(2,3,2,4))

ary[1,3,2]

ary[,1,2]

ary[,1,2,drop=FALSE]

ary[2,,]

## Data frame

ID <- c(1,2,3,4,5)
name <- c("Mouse", "Keyboard", "USB", "CPU", "Monitor")
price <- c(30000, 90000, 45000, 550000, 250000)
madeby <- c("Logitech", "Logitech", "Samsung", "Intel", "Samsung")
country <- c("USA", "China", "Korea", "USA", "Korea")
product <- data.frame(ID, name, price, madeby, madein=country)
product

names(product)

product[1:2]
product[c("name", "madeby")]
product$price

table(product$madeby, product$madein)

## List

a <- "List Example"
b <- c(1:3)
c <- c("one", "two", "three")
d <- matrix(1:12, nrow=3)
e <- data.frame(num=b, word=c)
f <- list(num=b, word=c)
g <- mean
h <- lm(mpg ~ wt, data=mtcars)
lst <- list(title=a, number=b, c, d, e, f, g, h)
lst

lst[[2]]
lst[["number"]]
lst$number

lst <- list(one=1, two=2, three=list(alpha=3.1, beta=3.2))
lst

lst[["three"]]
lst$three

lst[["three"]][["beta"]]
lst$three$beta

rainfall <- list(21.6,23.6,45.8,77.0,102.2,133.3,
                 327.9,348.0,137.6,49.3,53.0,24.9)
mean(rainfall)

mean(unlist(rainfall))

###################################
## A.4 Data Shape Transformation ##
###################################

trade <- data.frame(country=c("A", "B", "C"),
                    import=c(100, 350, 530),
                    export=c(250, 720, 990))
trade

library(tidyr)
trade.long <- gather(trade, key="type", value="amount", import, export)
trade.long

trade.wide <- spread(trade.long, key=type, value=amount)
trade.wide

head(airquality)
library(tidyr)
aq.long <- gather(data=airquality, key=Factor, value=Measurement, Ozone:Temp)
head(aq.long)
tail(aq.long)

gather(airquality, Factor, Measurement, -Month, -Day)

gather(airquality, Factor, Measurement, 1:4)
gather(airquality, Factor, Measurement, Ozone, Solar.R, Wind, Temp)

aq.wide <- spread(data=aq.long, key=Factor, value=Measurement)
head(aq.wide)
tail(aq.wide)

head(iris)
iris.long <- gather(iris, Element, Measurement, -Species)
head(iris.long)

iris.sep <- separate(data=iris.long, col=Element, into=c("Part", "Measures"))
head(iris.sep)

iris.unite <- unite(data=iris.sep, col=Factor, Part, Measures, sep="_")
head(iris.unite)
