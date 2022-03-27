
###############################
## R을 이용한 통계데이터분석 ##
## (곽기영, 도서출판 청람)   ## 
###############################

###############################
## 제2장 가설검정과 확률분포 ##
###############################

##################
## 2.2 확률분포 ##
##################

dbinom(7, size=10, prob=0.5)

pbinom(7, size=10, prob=0.5)
sum(dbinom(0:7, size=10, prob=0.5))

pbinom(7, size=10, prob=0.5, lower.tail=FALSE)

pbinom(7, size=10, prob=0.5) - pbinom(3, size=10, prob=0.5)

pbinom(c(3, 7), size=10, prob=0.5)

diff(pbinom(c(3, 7), size=10, prob=0.5))

rbinom(1, size=10, prob=0.5)
rbinom(5, size=10, prob=0.5)

pnorm(110, mean=100, sd=15)
pnorm(110, mean=100, sd=15, lower.tail=FALSE)

pnorm(0)
pnorm(0, mean=0, sd=1)

dnorm(110, mean=100, sd=15)

pnorm(110, mean=100, sd=15) - pnorm(90, mean=100, sd=15)
diff(pnorm(c(90, 110), mean=100, sd=15))

qnorm(0.05, mean=100, sd=15)

qnorm(0.95, mean=100, sd=15)

qnorm(c(0.05, 0.95), mean=100, sd=15)

qnorm(0.025)
qnorm(0.975)

qnorm(c(0.025, 0.975))

rnorm(1, mean=100, sd=15)
rnorm(5, mean=100, sd=15)
rnorm(1)
rnorm(5)

rnorm(3, mean=c(-10, 0, 10), sd=1)

rnorm(6, mean=c(-10, 0, 10), sd=1)

?Binomial
?Normal
?TDist
?FDist
?Chisquare
?Uniform

set.seed(123)
shapiro.test(rnorm(100, mean=100, sd=15))
shapiro.test(runif(100, min=2, max=4))

# [그림 2-3]
windows(width=7.5, height=5.5)
par("mar")
old.par <- par(mfrow=c(1,2))
set.seed(123)
qqnorm(rnorm(100, mean=100, sd=15), col="blue", 
       main="Sample from Normal Distribution")
qqline(rnorm(100, mean=100, sd=15))
qqnorm(runif(100, min=2, max=4), col="red", 
       main="Sample from Uniform Distribution")
qqline(runif(100, min=2, max=4))
par(old.par)
