
###############################
## R을 이용한 통계데이터분석 ##
## (곽기영, 도서출판 청람)   ## 
###############################

#######################
## 제14장 비모수검정 ##
#######################

###################
## 14.1 순위검정 ##
###################

library(MASS)
str(UScrime)

with(UScrime, by(Prob, So, median))
wilcox.test(Prob ~ So, data=UScrime)

str(airquality)

with(airquality, by(Ozone, Month, median, na.rm=TRUE))
kruskal.test(Ozone ~ Month, data=airquality)

library(MASS)
str(immer)

sapply(immer[c("Y1", "Y2")], median)
wilcox.test(immer$Y1, immer$Y2, paired=TRUE)

install.packages("agricolae")
library(agricolae)
data(grass)
str(grass)
head(grass); tail(grass)

friedman.test(evaluation ~ trt | judge, data=grass)

with(grass, friedman(judge, trt, evaluation, alpha=0.05, 
                     group=FALSE, main=NULL, console=TRUE))

#########################
## 14.2 퍼뮤테이션검정 ##
#########################

choose(10, 5)

score <- c(73, 68, 79, 85, 65, 77, 91, 88, 83, 95)
area <- factor(c(rep("A", 5), rep("B", 5)))
math <- data.frame(area, score)
math

install.packages("coin")
library(coin)
oneway_test(score ~ area, data=math, distribution="exact")

t.test(score ~ area, data=math, var.equal=TRUE)

library(MASS)
wilcox_test(Prob ~ factor(So), data=UScrime, distribution="exact")

wilcox.test(Prob ~ So, data=UScrime)

install.packages("multcomp")
library(multcomp)
str(cholesterol)
table(cholesterol$trt)

set.seed(123)
oneway_test(response ~ trt, data=cholesterol, distribution="approximate")

set.seed(123)
kruskal_test(response ~ trt, data=cholesterol, distribution="approximate")

library(MASS)
wilcoxsign_test(U1 ~ U2, data=UScrime, distribution="exact")

library(agricolae)
data(grass)
set.seed(123)
friedman_test(evaluation ~ trt | factor(judge), data=grass, 
              distribution="approximate")

install.packages("vcd")
library(vcd)
str(Arthritis)

set.seed(123)
chisq_test(Treatment ~ factor(Improved, ordered=FALSE), data=Arthritis, 
           distribution="approximate")

library(MASS)
str(Animals)

set.seed(123)
spearman_test(body ~ brain, data=Animals, distribution="approximate")

#######################
## 14.3 부트스트래핑 ##
#######################

qt(p=0.025, df=9, lower.tail=FALSE)

30+2.262*(5/sqrt(10))
30-2.262*(5/sqrt(10))

qt(p=0.025, df=9, lower.tail=FALSE)

rsq <- function(formula, data, indices) {
  d <- data[indices, ]
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
}

install.packages("boot")
library(boot)
state <- data.frame(state.x77)
set.seed(123)
bs.results <- boot(data=state, statistic=rsq, R=1000, 
                   formula=Life.Exp ~ Income + Illiteracy)
bs.results

bs.results$t0
head(bs.results$t, 3); tail(bs.results$t, 3)

# [그림 14-2]
windows(width=7.0, height=5.5)
plot(bs.results)

boot.ci(bs.results, type=c("perc", "bca"))
boot.ci(bs.results)

coeffs <- function(formula, data, indices) {
  d <- data[indices, ]
  fit <- lm(formula, data=d)
  return(coef(fit))
}

library(boot)
set.seed(123)
bs.results <- boot(data=state, statistic=coeffs, R=1000, 
                   formula=Life.Exp ~ Income + Illiteracy)
bs.results

bs.results$t0
bs.results$t

# [그림 14-3]
windows(width=7.0, height=5.5)
plot(bs.results, index=2)

boot.ci(bs.results, type="bca", index=2)
boot.ci(bs.results, type="bca", index=3)
