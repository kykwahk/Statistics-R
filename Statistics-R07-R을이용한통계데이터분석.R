####################################
## CHAPTER 7. Regression Analysis ##
####################################

###########################################
## 7.1 Simple Linear Regression Analysis ##
###########################################

install.packages("car")
library(car)
str(Prestige)

Prestige.lm <- lm(income ~ education, data=Prestige)
class(Prestige.lm)
Prestige.lm

# [Figure 7-2]
windows(width=7.0, height=5.5)
plot(Prestige$income ~ Prestige$education,
     col="cornflowerblue", pch=19,
     xlab="Education (years)", ylab="Income (dollars)",
     main="Education and Income")
abline(Prestige.lm, col="salmon", lwd=2)

summary(Prestige.lm)

Prestige.lm.summary <- summary(Prestige.lm)
coef(Prestige.lm.summary)

anova(Prestige.lm)

rownames(anova(Prestige.lm)); colnames(anova(Prestige.lm))
anova(Prestige.lm)["education", "Pr(>F)"]
anova(Prestige.lm)[1, 5]

coef(Prestige.lm)

confint(Prestige.lm)
confint(Prestige.lm, level=0.99)

fitted(Prestige.lm)[1:3]
resid(Prestige.lm)[1:3]

Prestige.new <- data.frame(education=c(5, 10, 15))
predict(Prestige.lm, newdata=Prestige.new)

predict(Prestige.lm, newdata=Prestige.new, interval="confidence")

mean(Prestige$education)
lm(income ~ education, data=Prestige, subset=(education > mean(education)))
lm(income ~ education, data=Prestige, subset=(education <= mean(education)))

########################################
## 7.2 Polynomial Regression Analysis ##
########################################

# [Figure 7-4]
library(car)
windows(width=7.0, height=5.5)
scatterplot(income ~ education, data=Prestige, pch=19, col="orangered", cex=1.2,
            regLine=list(method=lm, lty=2, lwd=3, col="royalblue"),
            smooth=list(smoother=loessLine, spread=FALSE, 
                        lty.smooth=1, lwd.smooth=3, col.smooth="green3"),
            xlab="Education (years)", ylab="Income (dollars)",
            main="Education and Income")

Prestige.poly <- lm(income ~ education + I(education^2), data=Prestige)

summary(Prestige.poly)

# [Figure 7-6]
windows(width=7.0, height=5.5)
plot(Prestige$income ~ Prestige$education, pch=19, col="darkorange",
     xlab="Education (years)", ylab="Income (dollars)",
     main="Education and Income")
library(dplyr)
lines(arrange(data.frame(Prestige$education, fitted(Prestige.poly)), 
              Prestige$education), col="cornflowerblue", lwd=2)

# [Figure 7-7]
windows(width=7.0, height=5.5)
scatterplot(eruptions ~ waiting, data=faithful, pch=19, col="deepskyblue", cex=1.2,
            regLine=list(method=lm, lty=2, lwd=3, col="blueviolet"),
            smooth=list(smoother=loessLine, spread=FALSE, 
                        lty.smooth=1, lwd.smooth=3, col.smooth="coral"),
            xlab="Waiting (minutes)", ylab="Eruptions (minutes)",
            main="Waiting Time Between Eruptions and the Duration of the Eruption")

faithful.poly <- lm(eruptions ~ waiting + I(waiting^2) + I(waiting^3), 
                    data=faithful)
summary(faithful.poly)

faithful.lm <- lm(eruptions ~ waiting, data=faithful)
summary(faithful.lm)

#############################################
## 7.3 Multiple Linear Regression Analysis ##
#############################################

data(mtcars)
str(mtcars)
mtcars <- mtcars[c("mpg", "hp", "wt", "disp", "drat")]

summary(mtcars)
cor(mtcars)

# [Figure 7-8]
library(car)
windows(width=7.0, height=5.5)
scatterplotMatrix(mtcars, pch=19, col="royalblue", cex=1.2,
                  regLine=list(method=lm, lty=1, lwd=3, col="salmon"),
                  smooth=list(smoother=loessLine, spread=FALSE, 
                              lty.smooth=1, lwd.smooth=3, col.smooth="forestgreen"),
                  main="Car Performance")

mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
summary(mtcars.lm)
install.packages("stargazer")
library(stargazer)
stargazer(mtcars.lm, type="text", no.space=TRUE)

mtcars.lm <- lm(scale(mpg) ~ scale(hp) + scale(wt) + scale(disp) + scale(drat), 
                data=mtcars)
summary(mtcars.lm)

install.packages("QuantPsyc")
library(QuantPsyc)
mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
lm.beta(mtcars.lm)

# [Figure 7-9]
mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
windows(width=7.0, height=7.0)
old.par <- par(mfrow=c(2, 2))
plot(mtcars.lm)
old.par

library(car)
vif(mtcars.lm)
vif(mtcars.lm) > 4
vif(mtcars.lm) > 10

library(car)
summary(powerTransform(mtcars$mpg))

library(car)
boxTidwell(mpg ~ hp + wt, data=mtcars)

library(car)
windows(width=7.0, height=5.5)
spreadLevelPlot(lm(mpg ~ hp + wt, data=mtcars))

mtcars.lm1 <- lm(mpg ~ hp + wt, data=mtcars)
mtcars.lm2 <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
anova(mtcars.lm1, mtcars.lm2)

AIC(mtcars.lm1, mtcars.lm2)

mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
step(mtcars.lm, direction="backward")

install.packages("leaps")
library(leaps)
mtcars.regsubsets <- regsubsets(x=mpg ~ hp + wt + disp + drat, data=mtcars, nbest=4)

# [Figure 7-11]
windows(width=7.0, height=5.5)
library(RColorBrewer)
plot(mtcars.regsubsets, scale="adjr2", col=brewer.pal(9, "Pastel1"),
     main="All Subsets Regression")

summary(mtcars.regsubsets)

names(summary(mtcars.regsubsets))

summary(mtcars.regsubsets)$adjr2

which.max(summary(mtcars.regsubsets)$adjr2)
coef(mtcars.regsubsets, 9)

str(InsectSprays)
levels(InsectSprays$spray)

tapply(InsectSprays$count, InsectSprays$spray, mean)

sprays.lm <- lm(count ~ spray, data=InsectSprays)
summary(sprays.lm)

contrasts(InsectSprays$spray)

sprays.aov <- aov(count ~ spray, data=InsectSprays)
summary(sprays.aov)
TukeyHSD(sprays.aov)

respray <- relevel(InsectSprays$spray, ref=6)
sprays.lm <- lm(count ~ respray, data=InsectSprays)
summary(sprays.lm)

contrasts(relevel(InsectSprays$spray, ref=6))

##################################################
## 7.4 Mediation and Moderation Effect Analysis ##
##################################################

## Mediation effect analysis

data(mtcars)
model.total <- lm(mpg ~ disp, data=mtcars)
summary(model.total)

model.M <- lm(wt ~ disp, data=mtcars)
summary(model.M)

model.Y <- lm(mpg ~ disp + wt, data=mtcars)
summary(model.Y)

install.packages("multilevel")
library(multilevel)
model.sob <- sobel(pred=mtcars$disp, med=mtcars$wt, out=mtcars$mpg)
model.sob

pnorm(abs(model.sob$z.value), lower.tail=FALSE)*2

install.packages("bda")
library(bda)
mediation.test(mv=mtcars$wt, iv=mtcars$disp, dv=mtcars$mpg)

install.packages("mediation")
library(mediation)
set.seed(123)
model.M <- lm(wt ~ disp, data=mtcars)
model.Y <- lm(mpg ~ disp + wt, data=mtcars)
model.mediation <- mediate(model.m=model.M, model.y=model.Y, 
                           treat="disp", mediator="wt", boot=TRUE, sims=500)
summary(model.mediation)

# [Figure 7-16]
windows(width=7.0, height=5.5)
plot(model.mediation, cex=1.2, col="royalblue", lwd=2,
     main="Mediation Effect Analysis")

## Moderation effect analysis

mtcars.lm <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)
summary(mtcars.lm)

# [Figure 7-19]
install.packages("effects")
library(effects)
windows(width=7.0, height=5.5)
m <- round(mean(mtcars$wt), 1); m
s <- round(sd(mtcars$wt), 1); s
plot(effect(term="hp:wt", mod=mtcars.lm, xlevels=list(wt=c(m-s, m, m+s))), 
     lines=list(multiline=TRUE, lwd=2, lty=c(3, 2, 1), 
                col=c("royalblue", "violet", "maroon")),
     main="Interaction Plot for Horsepower and Weight")

# [Figure 7-20]
install.packages("rockchalk")
library(rockchalk)
windows(width=7.0, height=5.5)
plotSlopes(model=mtcars.lm, plotx="hp", modx="wt", modxVals="std.dev.", 
           pch=21, col=rainbow(3), cex=1, bg="dimgray",
           main="Interaction Plot for Horsepower and Weight")

## Moderated mediation effect analysis

data(mtcars)

model.M <- lm(wt ~ disp*am, data=mtcars)
model.Y <- lm(mpg ~ disp*am + wt*am, data=mtcars)

library(mediation)
set.seed(12)
model.med1 <- mediate(model.m=model.M, model.y=model.Y, covariates=list(am=0),
                      treat="disp", mediator="wt", boot=TRUE, sims=500)
summary(model.med1)
set.seed(12)
model.med2 <- mediate(model.m=model.M, model.y=model.Y, covariates=list(am=1),
                      treat="disp", mediator="wt", boot=TRUE, sims=500)
summary(model.med2)

set.seed(12)
model.med <- mediate(model.m=model.M, model.y=model.Y,
                     treat="disp", mediator="wt", sims=500)
set.seed(12)
test.modmed(object=model.med, 
            covariates.1=list(am=0), covariates.2=list(am=1), sims=500)

#######################################
## 7.5 Penalized Regression Analysis ##
#######################################

library(MASS)
str(Boston)

library(caret)
set.seed(123)
train <- createDataPartition(y=Boston$medv, p=0.7, list=FALSE)
Boston.train <- Boston[train,]
Boston.test <- Boston[-train,]

x <- model.matrix(medv ~ ., Boston.train)[,-1]
y <- Boston.train$medv

library(glmnet)

## Ridge

set.seed(123)
Boston.cv <- cv.glmnet(x=x, y=y, family="gaussian", alpha=0)

# [Figure 7-23]
windows(width=7.0, height=5.5)
plot(Boston.cv)

Boston.cv$lambda.min
log(Boston.cv$lambda.min)

Boston.gnet <- glmnet(x, y, family="gaussian", alpha=0, lambda=Boston.cv$lambda.min)

coef(Boston.gnet)

Boston.test.x <- model.matrix(medv ~ ., Boston.test)[,-1]
Boston.pred <- predict(Boston.gnet, newx=Boston.test.x)

postResample(pred=Boston.pred, obs=Boston.test$medv)

## Lasso

set.seed(123)
Boston.cv <- cv.glmnet(x=x, y=y, family="gaussian", alpha=1)
Boston.cv$lambda.min
log(Boston.cv$lambda.min)

# [Figure 7-24]
windows(width=7.0, height=5.5)
plot(Boston.cv)

Boston.cv$lambda.1se
log(Boston.cv$lambda.1se)

coef(Boston.cv, Boston.cv$lambda.min)

coef(Boston.cv, Boston.cv$lambda.1se)

Boston.gnet1 <- glmnet(x, y, family="gaussian", 
                       alpha=1, lambda=Boston.cv$lambda.min)
Boston.pred1 <- predict(Boston.gnet1, newx=Boston.test.x)
postResample(pred=Boston.pred1, obs=Boston.test$medv)

Boston.gnet2 <- glmnet(x, y, family="gaussian", 
                       alpha=1, lambda=Boston.cv$lambda.1se)
Boston.pred2 <- predict(Boston.gnet2, newx=Boston.test.x)
postResample(pred=Boston.pred2, obs=Boston.test$medv)

## Elastic net

library(caret)
set.seed(123)
Boston.cv <- train(form=medv ~ ., data=Boston.train, method="glmnet",
                   trControl=trainControl(method="cv", number=10),
                   tuneLength=10)

Boston.cv$bestTune

Boston.gnet <- glmnet(x, y, family="gaussian", 
                      alpha=Boston.cv$bestTune$alpha, 
                      lambda=Boston.cv$bestTune$lambda)
coef(Boston.gnet)

Boston.pred <- predict(Boston.gnet, newx=Boston.test.x)
postResample(pred=Boston.pred, obs=Boston.test$medv)

## Comparing different models

library(caret)
lambda <- 10^seq(-5, 5, length=100)

set.seed(123)
ridge <- train(medv ~ ., data=Boston.train, method="glmnet",
               trControl=trainControl(method="cv", number=10),
               tuneGrid=expand.grid(alpha=0, lambda=lambda))
coef(ridge$finalModel, ridge$bestTune$lambda)

ridge.pred <- predict(ridge, Boston.test)
postResample(pred=ridge.pred, obs=Boston.test$medv)

set.seed(123)
lasso <- train(medv ~ ., data=Boston.train, method="glmnet",
               trControl=trainControl(method="cv", number=10),
               tuneGrid=expand.grid(alpha=1, lambda=lambda))
coef(lasso$finalModel, lasso$bestTune$lambda)
lasso.pred <- predict(lasso, Boston.test)
postResample(pred=lasso.pred, obs=Boston.test$medv)

set.seed(123)
elastic <- train(form=medv ~ ., data=Boston.train, method="glmnet",
                 trControl=trainControl(method="cv", number=10),
                 tuneLength=10)
coef(elastic$finalModel, elastic$bestTune$lambda)
elastic.pred <- predict(elastic, Boston.test)
postResample(pred=elastic.pred, obs=Boston.test$medv)

models <- list(ridge=ridge, lasso=lasso, elastic=elastic)
summary(resamples(models), metric="RMSE")

summary(diff(resamples(models), metric="RMSE"))
