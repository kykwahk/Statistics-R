
###############################
## R을 이용한 통계데이터분석 ##
## (곽기영, 도서출판 청람)   ## 
###############################

####################
## 제3장 평균검정 ##
####################

###############
## 3.1 t검정 ##
###############

t <- (135-115)/(25/sqrt(20))
t

pt(3.58, df=19, lower.tail=FALSE)*2
(1 - pt(3.58, df=19))*2

qt(0.025, df=19, lower.tail=FALSE)
qt(0.975, df=19)

135-2.09*(25/sqrt(20))
135+2.09*(25/sqrt(20))

qt(0.005, df=19, lower.tail=FALSE)

#########################
## 3.2 일표본 평균검정 ##
#########################

library(MASS)
str(cats)

t.test(x=cats$Bwt, mu=2.6)

t.test(cats$Bwt, mu=2.7)

t.test(cats$Bwt, mu=2.6, alternative="greater")

cats.t <- t.test(cats$Bwt, mu=2.6)
str(cats.t)

cats.t$p.value
cats.t$conf.int

t.test(cats$Bwt, mu=2.6, conf.level=0.99)

prop.test(x=18, n=30, p=0.5, alternative="greater")

###########################
## 3.3 독립표본 평균검정 ##
###########################

t.test(formula=Bwt ~ Sex, data=cats)

# [그림 3-6]
bars <- tapply(cats$Bwt, cats$Sex, mean)
lower <- tapply(cats$Bwt, cats$Sex, function(x) t.test(x)$conf.int[1])
upper <- tapply(cats$Bwt, cats$Sex, function(x) t.test(x)$conf.int[2])

windows(width=4.0, height=5.5)
install.packages("gplots")
library(gplots)
barplot2(bars, space=0.4, ylim=c(0, 3.0),
         plot.ci=TRUE, ci.l=lower, ci.u=upper, ci.color="maroon", ci.lwd=4, 
         names.arg=c("Female", "Male"), col=c("coral", "darkkhaki"),
         xlab="Cats", ylab="Body Weight (kg)", 
         main="Body Weight by Sex\nwith Confidence Interval")

Bwt.f <- cats$Bwt[cats$Sex=="F"]
Bwt.m <- cats$Bwt[cats$Sex=="M"]
t.test(Bwt.f, Bwt.m)

smokers  <- c(83, 90, 129, 70)
patients <- c(86, 93, 136, 82)

prop.test(x=smokers, n=patients)

###########################
## 3.4 대응표본 평균검정 ##
###########################

str(sleep)
sleep[seq(1, 20, 2), ]

t.test(extra ~ group, data=sleep, paired=TRUE)

install.packages("tidyr")
library(tidyr)
sleep.wide <- spread(sleep, key=group, value=extra)
sleep.wide

install.packages("reshape2")
library(reshape2)
sleep.wide <- dcast(sleep, ID ~ group, value.var="extra")
sleep.wide

t.test(sleep.wide$'1', sleep.wide$'2', paired=TRUE)
