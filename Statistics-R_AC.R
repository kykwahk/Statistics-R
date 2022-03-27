
###############################
## R을 이용한 통계데이터분석 ##
## (곽기영, 도서출판 청람)   ## 
###############################

#########################
## 부록 C. 테이블 출력 ##
#########################

install.packages("stargazer")
library(stargazer)

str(mtcars)

# [그림 C-1]
stargazer(mtcars, type="html", title="Descriptive Statistics", 
          digits=1, out="cars.html")

install.packages("pander")
library(pander)
openFileInOS("cars.html")
openFileInOS("mtcars.html")
openFileInOS("cor.html")

# [그림 C-2]
stargazer(mtcars, type="html", title="Descriptive Statistics", 
          digits=1, out="cars.html", flip=TRUE)

# [그림 C-3]
stargazer(mtcars[c("mpg", "hp", "wt")], type="html", title="Descriptive Statistics", 
          digits=1, out="cars.html",
          covariate.labels=c("Miles per gallon", 
                             "Gross horsepower", 
                             "Weight (1000 lbs)"))

# [그림 C-4]
stargazer(mtcars[c("mpg", "hp", "wt")], summary=FALSE, 
          type="html", title="Car Information", out="mtcars.html")

# [그림 C-5]
stargazer(cor(mtcars[c("mpg", "hp", "wt")]), type="html", 
          title="Correlation Matrix", out="cor.html")

data(mtcars)
mtcars$highmpg <- factor(mtcars$mpg > mean(mtcars$mpg))
mtcars$gear <- factor(mtcars$gear)
mtcars$am <- factor(mtcars$am)
m1 <- lm(mpg ~ disp, data=mtcars)
m2 <- lm(mpg ~ disp + drat, data=mtcars)
m3 <- lm(mpg ~ disp + drat + gear, data=mtcars)
m4 <- glm(highmpg ~ disp + drat + am, family=binomial(link="logit"), data=mtcars)

# [그림 C-6]
stargazer(m1, m2, m3, m4, type="html", title="Model Comparison", out="cars.html")

# [그림 C-7]
stargazer(m1, m2, m3, m4, type="html", title="Model Comparison", out="cars.html",
          dep.var.labels=c("Miles per gallon", "High MPG car"), 
          covariate.labels=c("Displacement (cu.in.)", "Rear axle ratio",
                             "Four gears", "Five gears", "Transmission (manual=1)"),
          omit.stat=c("LL", "ser", "f"), no.space=TRUE)

# [그림 C-8]
stargazer(m1, m2, m3, type="html", title="Model Comparison", out="cars.html",
          dep.var.labels=c("Miles per gallon"), 
          covariate.labels=c("Displacement (cu.in.)", "Rear axle ratio",
                             "Four gears", "Five gears"),
          omit.stat=c("LL", "ser", "f"), ci=TRUE, ci.level=0.95, single.row=TRUE)

stargazer(m1, m2, m3, type="text", title="Model Comparison", out="cars.txt",
          dep.var.labels=c("Miles per gallon"), 
          covariate.labels=c("Displacement (cu.in.)", "Rear axle ratio",
                             "Four gears", "Five gears"),
          omit.stat=c("LL", "ser", "f"), ci=TRUE, ci.level=0.95, single.row=TRUE)
