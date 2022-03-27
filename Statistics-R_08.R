
###############################
## R을 이용한 통계데이터분석 ##
## (곽기영, 도서출판 청람)   ## 
###############################

#############################################
## 제8장 로지스틱회귀분석과 포아송회귀분석 ##
#############################################

###############################
## 8.2 이항 로지스틱회귀분석 ##
###############################

install.packages("modeldata")
library(modeldata)
data(mlc_churn)
str(mlc_churn)

library(tibble)
churn <- mlc_churn
levels(churn$churn)
churn <- churn[-c(1, 3)]
churn$churn <- factor(ifelse(churn$churn=="no", 1, 2),
                      levels=c(1, 2), labels=c("no", "yes"))
levels(churn$churn)

churn.train <- churn[1:3333,]
churn.test <- churn[3334:5000,]
churn.train
churn.test

table(churn.train$churn)
prop.table(table(churn.train$churn))
table(churn.test$churn)
prop.table(table(churn.test$churn))

churn.logit <- glm(churn ~ ., data=churn.train, family=binomial(link="logit"))
summary(churn.logit)

# 로지싀틱회귀모델 유의성 검정
pchisq(q=2758.3-2158.7, df=3332-3315, lower.tail=FALSE) 
pchisq(q=churn.logit$null.deviance-churn.logit$deviance, 
       df=churn.logit$df.null-churn.logit$df.residual, 
       lower.tail=FALSE)
# R-squared
(2758.3-2158.7)/2758.3
(churn.logit$null.deviance-churn.logit$deviance)/churn.logit$null.deviance

exp(coef(churn.logit))

churn.test$international_plan <- ifelse(churn.test$international_plan=="no", 0, 1)
churn.test$voice_mail_plan <- ifelse(churn.test$voice_mail_plan=="no", 0, 1)
z <- coef(churn.logit)[1] + 
  (as.matrix(churn.test[-18]) %*% coef(churn.logit)[-1])
p <- 1/(1+exp(-z))
head(p)

churn.test <- churn[3334:5000,]
churn.logit.pred <- predict(churn.logit, newdata=churn.test, type="response")
head(churn.logit.pred)

churn.logit.pred <- factor(churn.logit.pred > 0.5, 
                           levels=c(FALSE, TRUE), labels=c("no", "yes"))
head(churn.logit.pred)
table(churn.logit.pred)

table(churn.test$churn, churn.logit.pred, dnn=c("Actual", "Predicted"))
mean(churn.test$churn==churn.logit.pred)

churn.logit2 <- step(churn.logit)
summary(churn.logit2)

churn.test$international_plan <- ifelse(churn.test$international_plan=="no", 0, 1)
churn.test$voice_mail_plan <- ifelse(churn.test$voice_mail_plan=="no", 0, 1)
table(churn.test$number_customer_service_calls)
testdata <- data.frame(international_planyes=mean(churn.test$international_plan),
                       voice_mail_planyes=mean(churn.test$voice_mail_plan),
                       number_vmail_messages=mean(churn.test$number_vmail_messages),
                       total_day_charge=mean(churn.test$total_day_charge),
                       total_eve_minutes=mean(churn.test$total_eve_minutes),
                       total_night_charge=mean(churn.test$total_night_charge),
                       total_intl_calls=mean(churn.test$total_intl_calls),
                       total_intl_charge=mean(churn.test$total_intl_charge),
                       number_customer_service_calls=c(0:7))
testdata

z <- coef(churn.logit2)[1] + 
  (as.matrix(testdata) %*% coef(churn.logit2)[-1])
p <- 1/(1+exp(-z))
testdata$prob <- p
testdata[c("number_customer_service_calls", "prob")]

testdata <- data.frame(number_customer_service_calls=c(0:7),
                       international_plan="no",
                       voice_mail_plan="no",
                       number_vmail_messages=mean(churn.test$number_vmail_messages),
                       total_day_charge=mean(churn.test$total_day_charge),
                       total_eve_minutes=mean(churn.test$total_eve_minutes),
                       total_night_charge=mean(churn.test$total_night_charge),
                       total_intl_calls=mean(churn.test$total_intl_calls),
                       total_intl_charge=mean(churn.test$total_intl_charge))
testdata
testdata$prob <- predict(churn.logit2, newdata=testdata, type="response")
testdata[c("number_customer_service_calls", "prob")]

deviance(churn.logit2)/df.residual(churn.logit2)

fit.origin <- glm(formula=churn ~ international_plan + voice_mail_plan + 
                    number_vmail_messages + total_day_charge + total_eve_minutes + 
                    total_night_charge + total_intl_calls + total_intl_charge + 
                    number_customer_service_calls, family=binomial(), 
                  data=churn.train)
fit.overdis <- glm(formula=churn ~ international_plan + voice_mail_plan + 
                     number_vmail_messages + total_day_charge + total_eve_minutes + 
                     total_night_charge + total_intl_calls + total_intl_charge + 
                     number_customer_service_calls, family=quasibinomial(), 
                  data=churn.train)
pchisq(summary(fit.overdis)$dispersion * fit.origin$df.residual, 
       fit.origin$df.residual, lower.tail=FALSE)

#################################
## 8.3 페널티 로지스틱회귀분석 ##
#################################

install.packages("mlbench")
library(mlbench)
data(PimaIndiansDiabetes2)
str(PimaIndiansDiabetes2)

PimaIndiansDiabetes3 <- na.omit(PimaIndiansDiabetes2)

library(caret)
set.seed(123)
train <- createDataPartition(y=PimaIndiansDiabetes3$diabetes, p=0.7, list=FALSE)
diabetes.train <- PimaIndiansDiabetes3[train,]
diabetes.test <- PimaIndiansDiabetes3[-train,]

x <- model.matrix(diabetes ~ ., diabetes.train)[,-1]
y <- ifelse(diabetes.train$diabetes == "pos", 1, 0)

library(glmnet)
set.seed(123)
diabetes.cv <- cv.glmnet(x=x, y=y, family="binomial", alpha=1)

diabetes.cv$lambda.min
diabetes.cv$lambda.1se

coef(diabetes.cv, diabetes.cv$lambda.min)
coef(diabetes.cv, diabetes.cv$lambda.1se)

diabetes.gnet1 <- glmnet(x, y, family="binomial",
                         alpha=1, lambda=diabetes.cv$lambda.min)
diabetes.test.x <- model.matrix(diabetes ~ ., diabetes.test)[,-1]
diabetes.pred1 <- predict(diabetes.gnet1, newx=diabetes.test.x, type="response")
diabetes.pred1 <- ifelse(diabetes.pred1 > 0.5, "pos", "neg")
table(diabetes.test$diabetes, diabetes.pred1, dnn=c("Actual", "Predicted"))
mean(diabetes.pred1==diabetes.test$diabetes)

diabetes.gnet2 <- glmnet(x, y, family="binomial",
                         alpha=1, lambda=diabetes.cv$lambda.1se)
diabetes.test.x <- model.matrix(diabetes ~ ., diabetes.test)[,-1]
diabetes.pred2 <- predict(diabetes.gnet2, newx=diabetes.test.x, type="response")
diabetes.pred2 <- ifelse(diabetes.pred2 > 0.5, "pos", "neg")
table(diabetes.test$diabetes, diabetes.pred2, dnn=c("Actual", "Predicted"))
mean(diabetes.pred2==diabetes.test$diabetes)

diabetes.logit <- glm(diabetes ~ ., data=diabetes.train, family=binomial(link="logit"))
diabetes.logit.pred <- predict(diabetes.logit, newdata=diabetes.test, type="response")
diabetes.logit.pred <- ifelse(diabetes.logit.pred > 0.5, "pos", "neg")
table(diabetes.test$diabetes, diabetes.logit.pred, dnn=c("Actual", "Predicted"))
mean(diabetes.logit.pred==diabetes.test$diabetes)

###############################
## 8.4 다항 로지스틱회귀분석 ##
###############################

install.packages("EffectStars")
library(EffectStars)
data(PID)
str(PID)
head(PID)

library(VGAM)
pid.mlogit <- vglm(PID ~ ., family=multinomial(), data=PID)
summary(pid.mlogit)

exp(coef(pid.mlogit))

pid.mlogit.pred <- fitted(pid.mlogit)
head(pid.mlogit.pred)

testdata <- data.frame(Education=c("low", "high"),
                       TVnews=mean(PID$TVnews),
                       Income=mean(PID$Income),
                       Age=mean(PID$Age),
                       Population=mean(PID$Population))
testdata

pid.mlogit.pred <- predict(pid.mlogit, newdata=testdata, type="response")
cbind(testdata, pid.mlogit.pred)

testdata <- data.frame(Education=rep("low", 5),
                       TVnews=mean(PID$TVnews),
                       Income=seq(20, 100, 20),
                       Age=mean(PID$Age),
                       Population=mean(PID$Population))
testdata
pid.mlogit.pred <- predict(pid.mlogit, newdata=testdata, type="response")
cbind(testdata, pid.mlogit.pred)

library(MASS)
str(fgl)
head(fgl)

fgl.scaled <- cbind(scale(fgl[,1:9]), fgl[10])
head(fgl.scaled)
str(fgl.scaled)

set.seed(123)
train <- sample(nrow(fgl), 0.7*nrow(fgl))
fgl.train <- fgl.scaled[train,]
fgl.test <- fgl.scaled[-train,]
table(fgl.train$type); sum(table(fgl.train$type))
table(fgl.test$type); sum(table(fgl.test$type))

library(nnet)
fgl.mlogit <- multinom(type ~ ., data=fgl.train)
summary(fgl.mlogit)

z <- summary(fgl.mlogit)$coefficients/summary(fgl.mlogit)$standard.errors
p <- (1-pnorm(abs(z), 0, 1))*2
print(p, digits=3)

fgl.mlogit.pred <- predict(fgl.mlogit, newdata=fgl.test, type="probs")
head(fgl.mlogit.pred)

cbind(round(fgl.mlogit.pred, 3), fgl.test["type"])

# [그림 8-2]
idx1 <- fgl.test$type == "WinF"
idx2 <- fgl.test$type == "WinNF"
idx3 <- fgl.test$type == "Veh"
idx4 <- fgl.test$type == "Con"
idx5 <- fgl.test$type == "Tabl"
idx6 <- fgl.test$type == "Head"
ys <- c(fgl.mlogit.pred[idx1, 1], fgl.mlogit.pred[idx2, 2],
        fgl.mlogit.pred[idx3, 3], fgl.mlogit.pred[idx4, 4],
        fgl.mlogit.pred[idx5, 5], fgl.mlogit.pred[idx6, 6])
xs <- c(fgl.test$type[idx1], fgl.test$type[idx2], fgl.test$type[idx3],
        fgl.test$type[idx4], fgl.test$type[idx5], fgl.test$type[idx6])
windows(width=7.0, height=5.5)
boxplot(ys ~ xs, names=levels(fgl.test$type), ylim=c(0,1), col=rainbow(6),
        xlab="Glass Type", ylab="Estimated Probabilities",
        main="Probabilities of Group Membership against True Group")

fgl.mlogit.pred <- colnames(fgl.mlogit.pred)[max.col(fgl.mlogit.pred)]
table(fgl.test$type, 
      factor(fgl.mlogit.pred, levels=levels(fgl.test$type),
             labels=levels(fgl.test$type)), dnn=c("Actual", "Predicted"))
mean(fgl.test$type==fgl.mlogit.pred)

data(fgl)
fgl.scaled <- cbind(scale(fgl[,1:9]), fgl[10])
fgl.mlogit.cv <- numeric()
for (i in 1:100) {
  train <- sample(nrow(fgl), 0.7*nrow(fgl))
  fgl.train <- fgl.scaled[train,]
  fgl.test <- fgl.scaled[-train,]
  fgl.mlogit <- multinom(type ~ ., data=fgl.train)
  fgl.mlogit.pred <- predict(fgl.mlogit, newdata=fgl.test, type="probs")
  fgl.mlogit.pred <- colnames(fgl.mlogit.pred)[max.col(fgl.mlogit.pred)]
  fgl.mlogit.cv[i] <- mean(fgl.test$type==fgl.mlogit.pred)
}
fgl.mlogit.cv
summary(fgl.mlogit.cv)

# [그림 8-3]
windows(width=7.0, height=5.5)
boxplot(fgl.mlogit.cv, horizontal=TRUE, col="tomato", xlab="Accuracy",
        main="Accuracy for Forensic Glass (100 samples)")

########################
## 8.5 포아송회귀분석 ##
########################

install.packages("robust")
library(robust)
data(breslow.dat)
str(breslow.dat)

seizure <- breslow.dat[c("Base", "Age", "Trt", "sumY")]

summary(seizure)

# [그림 8-4]
windows(width=7.0, height=5.5)
hist(seizure$sumY, breaks=20, col="cornflowerblue", 
     xlab="Seizure Count", main="Distribution of Seizures")

seizure.poisson <- glm(sumY ~ Base + Age + Trt, data=seizure, family=poisson)
summary(seizure.poisson)

coef(seizure.poisson)

exp(coef(seizure.poisson))

deviance(seizure.poisson)/df.residual(seizure.poisson)

install.packages("qcc")
library(qcc)
qcc.overdispersion.test(seizure$sumY, type="poisson")

seizure.qpoisson <- glm(sumY ~ Base + Age + Trt, data=seizure, 
                        family=quasipoisson())
summary(seizure.qpoisson)

0.022740/0.013800
-0.152701/0.163943

library(MASS)
str(ships)
?ships

shipsinc <- subset(ships, service > 0)
shipsinc$year <- factor(shipsinc$year)
shipsinc$period <- factor(shipsinc$period)
levels(shipsinc$year); levels(shipsinc$period)

shipsinc.poisson <- glm(incidents ~ type + year + period, data=shipsinc, 
                        family=poisson(), offset=log(service))
summary(shipsinc.poisson)

deviance(shipsinc.poisson)/df.residual(shipsinc.poisson)
library(qcc)
qcc.overdispersion.test(shipsinc$incidents, type="poisson")

shipsinc.qpoisson <- update(shipsinc.poisson, family=quasipoisson())
summary(shipsinc.qpoisson)
exp(coef(shipsinc.qpoisson))
