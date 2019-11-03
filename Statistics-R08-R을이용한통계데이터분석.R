####################################################################
## CHAPTER 8. Logistic Regression and Poisson Regression Analysis ##
####################################################################

###############################################
## 8.2 Binomial Logistic Regression Analysis ##
###############################################

install.packages("C50")
library(C50)
data(churn)
str(churnTrain)

churn.train <- churnTrain[-c(1, 3)]
churn.train$churn <- factor(ifelse(churn.train$churn=="no", 1, 2), 
                            levels=c(1, 2), labels=c("no", "yes"))
str(churn.train)

table(churn.train$churn)
prop.table(table(churn.train$churn))

churn.logit <- glm(churn ~ ., data=churn.train, family=binomial(link="logit"))
summary(churn.logit)

exp(coef(churn.logit))

churn.test <- churnTest[-c(1, 3)]
churn.test$churn <- factor(ifelse(churn.test$churn=="no", 1, 2),
                           levels=c(1, 2), labels=c("no", "yes"))
churn.test$international_plan <- ifelse(churn.test$international_plan=="no", 0, 1)
churn.test$voice_mail_plan <- ifelse(churn.test$voice_mail_plan=="no", 0, 1)
z <- coef(churn.logit)[1] + 
  (as.matrix(churn.test[-18]) %*% coef(churn.logit)[-1])
p <- 1/(1+exp(-z))
head(p)

data(churn)
churn.test <- churnTest[-c(1, 3)]
churn.test$churn <- factor(ifelse(churn.test$churn=="no", 1, 2),
                           levels=c(1, 2), labels=c("no", "yes"))
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

data(churn)
churn.test <- churnTest[-c(1, 3)]
churn.test$churn <- factor(ifelse(churn.test$churn=="no", 1, 2),
                           levels=c(1, 2), c("no", "yes"))
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

################################################
## 8.3 Penalized Logistic Regression Analysis ##
################################################

install.packages("mlbench")
library(mlbench)
data(PimaIndiansDiabetes2)
str(PimaIndiansDiabetes2)

PimaIndiansDiabetes3 <- na.omit(PimaIndiansDiabetes2)

library(caret)
set.seed(123)
train <- createDataPartition(y=PimaIndiansDiabetes3$diabetes, p=0.7, list=FALSE)
diabete.train <- PimaIndiansDiabetes3[train,]
diabete.test <- PimaIndiansDiabetes3[-train,]

x <- model.matrix(diabetes ~ ., diabete.train)[,-1]
y <- ifelse(diabete.train$diabetes == "pos", 1, 0)

library(glmnet)
set.seed(123)
diabete.cv <- cv.glmnet(x=x, y=y, family="binomial", alpha=1)

diabete.cv$lambda.min
diabete.cv$lambda.1se

coef(diabete.cv, diabete.cv$lambda.min)
coef(diabete.cv, diabete.cv$lambda.1se)

diabete.gnet1 <- glmnet(x, y, family="binomial",
                        alpha=1, lambda=diabete.cv$lambda.min)
diabete.test.x <- model.matrix(diabetes ~ ., diabete.test)[,-1]
diabete.pred1 <- predict(diabete.gnet1, newx=diabete.test.x, type="response")
diabete.pred1 <- ifelse(diabete.pred1 > 0.5, "pos", "neg")
table(diabete.test$diabetes, diabete.pred1, dnn=c("Actual", "Predicted"))
mean(diabete.pred1==diabete.test$diabetes)

diabete.gnet2 <- glmnet(x, y, family="binomial",
                        alpha=1, lambda=diabete.cv$lambda.1se)
diabete.test.x <- model.matrix(diabetes ~ ., diabete.test)[,-1]
diabete.pred2 <- predict(diabete.gnet2, newx=diabete.test.x, type="response")
diabete.pred2 <- ifelse(diabete.pred2 > 0.5, "pos", "neg")
table(diabete.test$diabetes, diabete.pred2, dnn=c("Actual", "Predicted"))
mean(diabete.pred2==diabete.test$diabetes)

diabete.logit <- glm(diabetes ~ ., data=diabete.train, family=binomial(link="logit"))
diabete.logit.pred <- predict(diabete.logit, newdata=diabete.test, type="response")
diabete.logit.pred <- ifelse(diabete.logit.pred > 0.5, "pos", "neg")
table(diabete.test$diabetes, diabete.logit.pred, dnn=c("Actual", "Predicted"))
mean(diabete.logit.pred==diabete.test$diabetes)

##################################################
## 8.4 Multinomial Logistic Regression Analysis ##
##################################################

install.packages("EffectStars")
library(EffectStars)
data(PID)
str(PID)
head(PID)

install.packages("VGAM")
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

set.seed(123)
train <- sample(nrow(fgl), 0.7*nrow(fgl))
fgl.train <- fgl.scaled[train,]
fgl.test <- fgl.scaled[-train,]
table(fgl.train$type); sum(table(fgl.train$type))
table(fgl.test$type); sum(table(fgl.test$type))

install.packages("nnet")
library(nnet)
fgl.mlogit <- multinom(type ~ ., data=fgl.train)
summary(fgl.mlogit)

z <- summary(fgl.mlogit)$coefficients/summary(fgl.mlogit)$standard.errors
p <- (1-pnorm(abs(z), 0, 1))*2
print(p, digits=3)

fgl.mlogit.pred <- predict(fgl.mlogit, newdata=fgl.test, type="probs")
head(fgl.mlogit.pred)

cbind(round(fgl.mlogit.pred, 3), fgl.test["type"])

# [Figure 8-2]
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

# [Figure 8-3]
windows(width=7.0, height=5.5)
boxplot(fgl.mlogit.cv, horizontal=TRUE, col="tomato", xlab="Accuracy",
        main="Accuracy for Forensic Glass (100 samples)")

#####################################
## 8.5 Poisson Regression Analysis ##
#####################################

install.packages("robust")
library(robust)
data(breslow.dat)
str(breslow.dat)

seizure <- breslow.dat[c("Base", "Age", "Trt", "sumY")]

summary(seizure)

# [Figure 8-4]
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

library(MASS)
str(ships)

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
