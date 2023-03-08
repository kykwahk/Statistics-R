
######################################
## R을 이용한 통계데이터분석(제2판) ##
##      (곽기영, 도서출판 청람)     ## 
######################################

####################
## 부록 A. R 기초 ##
####################

######################
## A.1 설치 및 사용 ##
######################

options(digits=3)
options(show.signif.stars=FALSE)
options(prompt=">> ")
options(continue="++ ")

library(ggplot2)
setwd("D:/MyWorkspaceR")
cat("\nWelcome at ", date(), "\n")

11 + 12 + 13

q()

fah <- readline("Fahrenheit? ")
fah <- as.numeric(fah)
cel <- (fah-32)/1.8
print(paste("Celsius =", cel))

source("FahCel.R")

##############
## A.2 환경 ##
##############

## 작업환경

getwd()

setwd("D:/MyWorkspaceR")
getwd()

save.image()

ls()

rm(list=ls())
ls()

x <- 100
y <- c(2, 3, 5, 7)
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

## 패키지

library()

search()

xyplot(dist ~ speed, data=cars)

library(lattice)
search()
xyplot(dist ~ speed, data=cars)

detach(package:lattice)
search()

install.packages("ggplot2")

.libPaths()

data()

data(package="ggplot2")

cars
head(cars)
tail(cars)
head(cars, 3)

## 도움말

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

library(sos)
findFn("social network analysis")

#####################
## A.3 데이터 구조 ##
#####################

## 벡터

vec1 <- c(2, 3, 5, 7, 11)
vec2 <- c("cat", "dog", "fox", "horse", "pig", "rabbit")
vec3 <- c(TRUE, FALSE, TRUE)
vec4 <- c(4:-2)

vec2
vec2[3]
vec2[c(1, 3, 5)]
vec2[3:5]

c(1, 2, 3) + c(4, 5, 6)

c(1:3) + c(4:9)

1 + 2
"+"(1, 2)

10 + c(1, 3, 5)

fruit <- c("Apple", "Banana", "Strawberry")
food <- c("Pie", "Juice", "Cake")
paste(fruit, food)

paste(fruit, "Juice")

abs(-3:3)
sqrt(1:5)

ifelse(c(0, 25, 50, 75, 100) > 50, "pass", "fail")

## 팩터

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

sex <- c(2, 1, 2, 2, 1, 0)
sex.factor <- factor(sex, levels=c(1, 2), labels=c("Male", "Female"))

sex.factor
table(sex.factor)

## 행렬

matrix(data=1:12, nrow=3, ncol=4)

rnames <- c("R1", "R2", "R3")
cnames <- c("C1", "C2", "C3", "C4")
matrix(1:12, 3, 4, dimnames=list(rnames, cnames), byrow=TRUE)

mat <- matrix(1:12, nrow=3)
mat
mat[2,]
mat[,3]
mat[c(1, 2), c(2, 4)]

mat[2,,drop=FALSE]

## 배열

ary <- array(data=1:12, dim=c(2, 3, 2))
ary

ary[1,3,2]

ary[,1,2]

ary[,1,2,drop=FALSE]

ary[2,,]

## 데이터프레임

ID <- c(1, 2, 3, 4, 5)
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

## 리스트

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

rainfall <- list(21.6, 23.6, 45.8, 77.0, 102.2, 133.3,
                 327.9, 348.0, 137.6, 49.3, 53.0, 24.9)
mean(rainfall)

mean(unlist(rainfall))

#####################
## A.4 데이터 입력 ##
#####################

read.csv(file="product.csv")

read.csv("product-with-no-header.csv", header=FALSE)
read.csv("product-with-no-header.csv", header=FALSE, col.names=c("id", "name", "price"))

read.table(file="product.txt")

read.table("product.txt", header=TRUE)

read.table("product-colon.txt", sep=":", header=TRUE)

read.table("product-missing.txt", header=TRUE)
read.table("product-missing.txt", header=TRUE, na.strings=".")

readLines(con="won-dollar.txt")

paste(readLines(con="won-dollar.txt"), collapse=" ")

readLines("won-dollar.txt", n=2)

scan(file="won-dollar.txt", what=character())

scan("won-dollar.txt", what=list(character(), numeric(), numeric()))

scan("won-dollar.txt", 
     what=list(date=character(), buy=numeric(), sell=numeric()))

scan("won-dollar.txt", 
     what=list(date=character(), buy=numeric(), sell=numeric()),
     nlines=2)
scan("won-dollar.txt",
     what=list(date=character(), buy=numeric(), sell=numeric()),
     skip=3)

library(readxl)
read_excel(path="product.xlsx", sheet=1)

library(openxlsx)
read.xlsx(xlsxFile="product.xlsx", sheet=1)

##########################
## A.5 데이터 형태 변환 ##
##########################

library(tidyr)
head(airquality)
aq.long <- pivot_longer(data=airquality, cols=c(Ozone, Solar.R, Wind, Temp), 
                        names_to="Factor", values_to="Measurement")
head(aq.long)
tail(aq.long)

pivot_longer(data=airquality, cols=Ozone:Temp, 
             names_to="Factor", values_to="Measurement")
pivot_longer(data=airquality, cols=c(-Month, -Day), 
             names_to="Factor", values_to="Measurement")

aq.wide <- pivot_wider(data=aq.long, names_from=Factor, values_from=Measurement)
head(aq.wide)
tail(aq.wide)

head(iris)
iris.long <- pivot_longer(data=iris, cols=-Species, 
                          names_to="Element", values_to="Measurement")
head(iris.long)

iris.sep <- separate(data=iris.long, col=Element, into=c("Part", "Measures"))
head(iris.sep)

iris.unite <- unite(data=iris.sep, col=Factor, Part, Measures, sep="_")
head(iris.unite)
