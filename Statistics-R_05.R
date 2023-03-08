
######################################
## R을 이용한 통계데이터분석(제2판) ##
##      (곽기영, 도서출판 청람)     ## 
######################################

###################################
## 제5장 독립성검정과 적합성검정 ##
###################################

######################
## 5.1 카이제곱검정 ##
######################

survivors <- matrix(c(1443, 151, 47, 1781, 312, 135), ncol=2)
dimnames(survivors) <- list(Status=c("minor injury", "serious injury", "dead"),
                            Seatbelt=c("with seatbelt", "without seatbelt"))
survivors

addmargins(survivors)
addmargins(prop.table(addmargins(survivors, 2), 2), 1)

# [그림 5-1]
windows(width=9.0, height=5.0)
par(mfrow=c(1, 2))
barplot(survivors, ylim=c(0, 2500), las=1, 
        col=c("yellowgreen", "lightsalmon", "orangered"),
        ylab="Frequency", main="Frequency of Survivors")
legend(0.2, 2500, rownames(survivors), 
       fill=c("yellowgreen", "lightsalmon", "orangered"))
survivors.prop <- prop.table(survivors, 2)
barplot(survivors.prop*100, las=1, col=c("yellowgreen", "lightsalmon", "orangered"),
        ylab="Percent", main="Percent of Survivors")

0.833*1641
0.833*2228

0.120*1641
0.120*2228

0.047*1641
0.047*2228

(1443-1366.95)^2/1366.95+(1781-1855.92)^2/1855.92+
  (151-196.92)^2/196.92+(312-267.36)^2/267.36+
  (47-77.13)^2/77.13+(135-104.72)^2/104.72

(1443-1367.0)^2/1367.0+(1781-1855.9)^2/1855.9+
  (151-196.9)^2/196.9+(312-267.4)^2/267.4+
  (47-77.1)^2/77.1+(135-104.7)^2/104.7

pchisq(45.91, df=2, lower.tail=FALSE)

qchisq(0.05, df=2, lower.tail=FALSE)

####################
## 5.2 독립성검정 ##
####################

str(Titanic)
Titanic

Titanic.margin <- margin.table(Titanic, margin=c(4, 1))
Titanic.margin

addmargins(Titanic.margin)
addmargins(prop.table(addmargins(Titanic.margin, 2), 2), 1)

chisq.test(Titanic.margin)

library(vcd)
assocstats(Titanic.margin)

# [그림 5-4]
library(vcd)
windows(width=7.0, height=5.5)
mosaic(Titanic.margin, shade=TRUE, legend=TRUE)
mosaic(~ Survived + Class, data=Titanic.margin, shade=TRUE, legend=TRUE)

library(MASS)
str(survey)

with(survey, chisq.test(Fold, Sex))

crosstab <- with(survey, table(Fold, Sex))
crosstab
chisq.test(crosstab)

####################
## 5.3 적합성검정 ##
####################

chisq.test(c(60, 55, 35))

oc <- c(60, 55, 35)
null.p <- c(0.45, 0.30, 0.25)
chisq.test(oc, p=null.p)

chisq.test(oc, p=c(45, 25, 15)/85)

str(HairEyeColor)

hairs <- margin.table(HairEyeColor, margin=1)
hairs

chisq.test(hairs, p=c(0.25, 0.50, 0.10, 0.15))

library(MASS)
smokers <- table(survey$Smoke)
smokers

chisq.test(smokers, p=c(0.1, 0.7, 0.10, 0.10))
