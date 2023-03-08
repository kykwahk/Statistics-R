
######################################
## R을 이용한 통계데이터분석(제2판) ##
##      (곽기영, 도서출판 청람)     ## 
######################################

##############################
## 제1장 데이터 수집과 요약 ##
##############################

#####################
## 1.1 데이터 수집 ##
#####################

## 신뢰도분석

library(psych)
str(sat.act)

ability <- sat.act[c("ACT", "SATV", "SATQ")]
head(ability)

alpha(ability)

alpha(ability[c(2, 3)])

#####################
## 1.2 데이터 요약 ##
#####################

## 범주형 변수 요약

library(MASS)
str(survey)
levels(survey$Smoke)

frqtab <- table(survey$Smoke)
frqtab
class(frqtab)
frqtab[2]

frqtab==max(frqtab)
frqtab[frqtab==max(frqtab)]
names(frqtab[frqtab==max(frqtab)])

which.max(frqtab)
frqtab[which.max(frqtab)]
names(frqtab[which.max(frqtab)])

frqtab.prop <- prop.table(frqtab)
frqtab.prop
frqtab.prop["Never"]
frqtab.prop * 100

mean(survey$Smoke=="Never", na.rm=TRUE)

mean(anorexia$Postwt > anorexia$Prewt)
mean(abs(mammals$brain-mean(mammals$brain)) > 2*sd(mammals$brain))
mean(diff(SP500) > 0)

library(vcd)
str(Arthritis)

crosstab <- table(Arthritis$Improved, Arthritis$Treatment)
crosstab
crosstab["Marked", "Treated"]

crosstab <- xtabs(~ Improved + Treatment, data=Arthritis)
crosstab

table(Arthritis$Improved, Arthritis$Treatment, dnn=c("Impreoved", "Treatment"))

margin.table(crosstab, margin=1)
prop.table(crosstab, 1)

margin.table(crosstab, margin=2)
prop.table(crosstab, 2)

prop.table(crosstab)

addmargins(crosstab, margin=1)
addmargins(crosstab, margin=2)
addmargins(crosstab)

addmargins(prop.table(crosstab, 2), 1)
addmargins(prop.table(crosstab, 1), 2)

library(gmodels)
CrossTable(Arthritis$Improved, Arthritis$Treatment, prop.chisq=FALSE, 
           dnn=c("Improved", "Treatment"))

multtab <- with(Arthritis, table(Improved, Sex, Treatment))
multtab <- xtabs(~ Improved + Sex + Treatment, data=Arthritis)
multtab
ftable(multtab)
ftable(multtab, row.vars=c(2, 3))

ftable(Arthritis[c("Improved", "Sex", "Treatment")], row.vars=c(2, 3))

margin.table(multtab, 1)
margin.table(multtab, 2)
margin.table(multtab, 3)

margin.table(multtab, c(1, 3))

ftable(prop.table(multtab, c(2, 3)))

ftable(addmargins(prop.table(multtab, c(2, 3)), 1))

## 연속형 변수 요약

library(MASS)
median(survey$Pulse, na.rm=TRUE)

quantile(survey$Pulse, probs=0.05, na.rm=TRUE)

quantile(survey$Pulse, 0.5, na.rm=TRUE)

quantile(survey$Pulse, c(0.05, 0.95), na.rm=TRUE)

quantile(survey$Pulse, na.rm=TRUE)
quantile(survey$Pulse, seq(0, 1, 0.25), na.rm=TRUE)

mean(survey$Pulse <= 80, na.rm=TRUE)

mean(survey$Pulse, na.rm=TRUE)
median(survey$Pulse, na.rm=TRUE)

str(iris)

summary(iris$Sepal.Width)

summary(iris$Species)

summary(as.character(iris$Species))

summary(iris)

iris.lst <- as.list(iris)
summary(iris.lst)

lapply(iris.lst, summary)

range(survey$Pulse, na.rm=TRUE)
diff(range(survey$Pulse, na.rm=TRUE))

var(survey$Pulse, na.rm=TRUE)
sd(survey$Pulse, na.rm=TRUE)

str(mtcars)

library(pastecs)
stat.desc(mtcars[c("mpg", "hp", "wt")])
stat.desc(mtcars[c("mpg", "hp", "wt")], norm=TRUE)

library(psych)
describe(mtcars[c("mpg", "hp", "wt")])

tapply(survey$Pulse, INDEX=survey$Exer, FUN=mean, na.rm=TRUE)

tapply(survey$Pulse, survey$Sex, mean, na.rm=TRUE)

tapply(survey$Pulse, list(survey$Exer, survey$Sex), mean, na.rm=TRUE)

aggregate(survey$Pulse, by=list(Exercise=survey$Exer), FUN=mean, na.rm=TRUE)
aggregate(survey$Pulse, list(Exercise=survey$Exer, Sex=survey$Sex), 
          mean, na.rm=TRUE)

aggregate(survey[c("Pulse", "Age")], 
          list(Exercise=survey$Exer), mean, na.rm=TRUE)

myStats <- function(x, na.rm=FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  mean <- mean(x)
  sd <- sd(x)
  skew <- sum((x-mean)^3/sd^3)/n
  kurt <- sum((x-mean)^4/sd^4)/n - 3
  return(c(n=n, mean=mean, sd=sd, skewness=skew, kurtosis=kurt))
}
aggregate(survey[c("Pulse", "Age")], 
          list(Exercise=survey$Exer), myStats, na.rm=TRUE)

by(survey[c("Pulse", "Age")], INDICES=list(Exercise=survey$Exer), FUN=summary)
aggregate(survey[c("Pulse", "Age")], list(Exercise=survey$Exer), summary)

by(survey[c("Pulse", "Age")], list(Exercise=survey$Exer), 
   function(x) sapply(x, myStats, na.rm=TRUE))

library(psych)
describeBy(survey[c("Pulse", "Age")], group=list(Exercise=survey$Exer))

describeBy(survey[c("Pulse", "Age")], group=list(Exercise=survey$Exer), summary) #error
