
######################################
## R을 이용한 통계데이터분석(제2판) ##
##      (곽기영, 도서출판 청람)     ## 
######################################

####################
## 제6장 상관분석 ##
####################

##################
## 6.1 상관관계 ##
##################

library(MASS)
str(cats)

# [그림 6-1]
windows(width=7.0, height=5.5)
plot(cats$Hwt ~ cats$Bwt, 
     pch=21, col="forestgreen", bg="green3", las=1,
     xlab="Body Weight (kg)", ylab="Heart Weight (g)",
     main="Body Weight and Heart Weight of Cats")

cor(cats$Bwt, cats$Hwt)
with(cats, cor(Bwt, Hwt))

with(cats, cor.test(Bwt, Hwt))

with(cats, cor.test(Bwt, Hwt, alternative="greater", conf.level=0.99))

with(cats, cor.test(~ Bwt + Hwt))

cor.test(~ Bwt + Hwt, data=cats)

cor.test(~ Bwt + Hwt, data=cats, subset=(Sex=="F"))

str(iris)
cor(iris[-5])

iris.cor <- cor(iris[-5])
class(iris.cor)
str(iris.cor)

iris.cor["Petal.Width", "Petal.Length"]

library(psych)
corr.test(iris[-5])

print(corr.test(iris[-5]), short=FALSE)

old.op <- options(digits=2)
cor(state.x77)
options(old.op)

# [그림 6-2]
library(psych)
windows(width=7.0, height=7.0)
pairs.panels(state.x77, pch=21, bg="red", hist.col="gold", 
             main="Correlation Plot of US States Data")

# [그림 6-3]
library(corrgram)
windows(width=7.0, height=7.0)
corrgram(state.x77, order=TRUE, lower.panel=panel.shade, 
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of US States Data")

# [그림 6-4]
library(corrgram)
windows(width=7.0, height=7.0)
cols <- colorRampPalette(c("darkgoldenrod4", "burlywood1", 
                           "darkkhaki", "darkgreen"))
corrgram(state.x77, order=FALSE, col.regions=cols,
         lower.panel=panel.pie, upper.panel=panel.conf, 
         text.panel=panel.txt, main="Corrgram of US States Data")

####################
## 6.2 편상관관계 ##
####################

colnames(mtcars)
mtcars2 <- mtcars[, c("mpg", "cyl", "hp", "wt")]
cor(mtcars2)

library(ggm)
pcor(c(1, 3, 2, 4), cov(mtcars2))
pcor(c("mpg", "hp", "cyl", "wt"), cov(mtcars2))

pcor.test(pcor(c(1, 3, 2, 4), cov(mtcars2)), q=2, n=nrow(mtcars2))

library(ppcor)
pcor(mtcars2)

pcor.test(mtcars2["mpg"], mtcars2["hp"], mtcars2[c("cyl", "wt")])
