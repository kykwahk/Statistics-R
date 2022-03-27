
###############################
## R을 이용한 통계데이터분석 ##
## (곽기영, 도서출판 청람)   ## 
###############################

####################
## 부록 B. 결측값 ##
####################

#####################
## B.1 결측값 식별 ##
#####################

x <- c(1, 2, 3, NA)
is.na(x)

str(airquality)

complete.cases(airquality)

airquality[complete.cases(airquality),]
na.omit(airquality)

airquality[!complete.cases(airquality),]

sum(!complete.cases(airquality))
mean(!complete.cases(airquality))
42/153*100

sum(is.na(airquality))
mean(is.na(airquality))
44/(153*6)*100

# [그림 B-1]
install.packages("mice")
library(mice)
windows(width=7.0, height=5.5)
md.pattern(airquality)

install.packages("VIM")
library(VIM)

# [그림 B-2]
windows(width=7.0, height=5.5)
miss <- aggr(airquality, prop=FALSE, numbers=TRUE, sortVar=TRUE)

summary(miss)$combinations

aggr(airquality, prop=TRUE, numbers=TRUE, sortVar=TRUE)

# [그림 B-3]
windows(width=7.0, height=5.5)
matrixplot(airquality, sortby=5)

# [그림 B-4]
windows(width=7.0, height=5.5)
marginplot(airquality[c("Solar.R", "Ozone")], 
           pch=20, col=c("cornflowerblue", "orangered", "purple"))

head(airquality)
x <- data.frame(abs(is.na(airquality)))
head(x)

y <- x[colSums(x) > 0]
head(y)

cor(y)
with(y, cor.test(Ozone, Solar.R))

cor(airquality, y, use="pairwise.complete.obs")

#####################
## B.2 결측값 처리 ##
#####################

cor(airquality[complete.cases(airquality),])
cor(na.omit(airquality))

cor(airquality, use="complete.obs")

lm(Ozone ~ Temp, data=na.omit(airquality))
sum(!complete.cases(airquality))

cor(airquality, use="pairwise.complete.obs")

air.lm <- lm(Ozone ~ Temp, data=airquality)
summary(air.lm)

airquality.new <- airquality
for (i in 1:ncol(airquality.new)) {
  if(sum(is.na(airquality.new[,i])) > 0) {
    na.idx <- which(is.na(airquality.new[,i]))
    airquality.new[na.idx, i] <- mean(airquality.new[,i], na.rm=TRUE)
  }
}

mean(airquality$Ozone, na.rm=TRUE)
mean(airquality$Solar.R, na.rm=TRUE)

head(airquality)
head(airquality.new)

library(mice)
imp <- mice(airquality, method="mean", m=1, maxit=1)

complete(imp)

methods(mice)

library(mice)
str(nhanes)
head(nhanes)

imp <- mice(nhanes, seed=123)
imp

attributes(imp)

imp$data
imp$imp
imp$imp$bmi
imp$predictorMatrix
imp$method

c3 <- complete(imp, action=3)
md.pattern(c3)

c.broad <- complete(imp, action="broad")
c.broad

c.long <- complete(imp, action="long")
c.long

c.median <- aggregate(c.long[3:6], by=list(id=c.long$.id), median)
head(c.median[-1])
head(nhanes)

ini <- mice(nhanes, maxit=0)
pred <- ini$predictorMatrix
pred

pred[, "hyp"] <- 0
pred

imp <- mice(nhanes, predictorMatrix=pred)
imp

imp <- mice(nhanes, predictorMatrix=quickpred(nhanes, mincor=0.3))
imp

str(nhanes2)

imp <- mice(nhanes2)
imp$method

methods(mice)

ini <- mice(nhanes2, maxit=0)
meth <- ini$method
meth
meth["bmi"] <- "norm.predict"
meth
imp <- mice(nhanes2, method=meth)
imp

imp$imp
complete(imp)

# [그림 B-6], [그림 B-7]
imp <- mice(nhanes2, seed=123)
windows(width=7.0, height=5.5)
stripplot(imp, bmi ~ .imp, pch=21, cex=1.5)
stripplot(imp, pch=21, cex=1.2)

fit <- with(imp, lm(chl ~ bmi + hyp))
fit

ls(fit)

summary(fit$analyses[[3]])

pooled <- pool(fit)
summary(pooled)
