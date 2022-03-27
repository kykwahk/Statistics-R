
###############################
## R을 이용한 통계데이터분석 ##
## (곽기영, 도서출판 청람)   ## 
###############################

#####################
## 제12장 생존분석 ##
#####################

############################
## 12.2 카플란-마이어분석 ##
############################

install.packages("survival")
library(survival)

surv.ex <- data.frame(id=c(1:15),
                      time=c(1,1,4,5,6,8,9,9,12,15,22,25,37,55,72),
                      status=c(1,1,1,1,0,1,0,1,1,0,1,0,1,1,0),
                      trt=c(2,2,2,2,2,1,2,2,1,1,2,1,1,1,1),
                      age=c(65,69,85,76,66,75,72,70,61,73,66,73,68,59,71))
surv.ex
fit <- survfit(Surv(time, status) ~ 1, data=surv.ex[surv.ex$trt==2,])
fit
names(fit)
fit.df <- data.frame(time=fit$time, n.risk=fit$n.risk, n.event=fit$n.event,
                     n.censor=fit$n.censor, surv=fit$surv, 
                     upper=fit$upper, lower=fit$lower)
fit.df

install.packages("survminer")
library(survminer)

# [그림 12-3]
windows(width=7.0, height=5.5)
ggsurvplot(fit, conf.int=FALSE, break.time.by=5, xlim=c(0, 25), 
           ggtheme=theme_light(),
           legend="none", xlab="Days", ylab="Survival Probability")

fit.group <- survfit(Surv(time, status) ~ trt, data=surv.ex)
fit.group

# [그림 12-4]
windows(width=7.0, height=5.5)
ggsurvplot(fit.group, conf.int=TRUE, 
           break.time.by=5,xlab="Days", ylab="Survival Probability",
           surv.median.line="hv", 
           legend.labs=c("Treatment 1", "Treatment 2"), legend.title="",
           ggtheme=theme_light(), palette=c("royalblue", "salmon"))

pchisq(q=4.59, df=1, lower.tail=FALSE)
qchisq(p=0.05, df=1, lower.tail=FALSE)

fit.group.test <- survdiff(Surv(time, status) ~ trt, data=surv.ex)
fit.group.test

library(survival)
data(lung)
str(lung)
lung$sex <- factor(lung$sex, levels=c(1, 2), labels=c("male", "female"))

Surv(time=lung$time, event=lung$status)
class(Surv(lung$time, lung$status))

km.fit <- survfit(Surv(time, status) ~ 1, data=lung)
km.fit

names(km.fit)

km.df <- data.frame(time=km.fit$time, n.risk=km.fit$n.risk, n.event=km.fit$n.event,
                    n.censor=km.fit$n.censor, surv=km.fit$surv, 
                    upper=km.fit$upper, lower=km.fit$lower)
head(km.df)

# [그림 12-6]
windows(width=7.0, height=5.5)
plot(km.fit, xlab="Days", ylab="Overall Survival Probability")

install.packages("survminer")
library(survminer)

# [그림 12-7]
windows(width=7.0, height=5.5)
ggsurvplot(km.fit, xlab="Days", ylab="Overall Survival Probability")

summary(km.fit, times=c(180, 360))

km.fit

quantile(km.fit, probs=1-c(0.7, 0.3))
quantile(km.fit, probs=1-0.5)

km.group <- survfit(Surv(time, status) ~ sex, data=lung)
km.group

summary(km.group)
summary(km.group)$table
summary(km.group, times=c(180, 360))

km.summary <- surv_summary(km.group, data=lung)
head(km.summary)
attr(km.summary, "table")

survdiff(Surv(time, status) ~ sex, data=lung)

library(survminer)

# [그림 12-8]
windows(width=7.0, height=7.0)
ggsurvplot(km.group, pval=TRUE, conf.int=TRUE, 
           risk.table="absolute", risk.table.col="strata", 
           linetype="strata", surv.median.line="hv", 
           ggtheme=theme_bw(), palette=c("royalblue", "salmon"))

# [그림 12-9]
windows(width=7.0, height=7.0)
ggsurvplot(km.group, pval=TRUE, conf.int=TRUE, conf.int.style="step", 
           xlab="Days", break.time.by=180,
           risk.table="abs_pct", risk.table.fontsize=3.5, risk.table.y.text=FALSE,
           ncensor.plot=TRUE, surv.median.line="hv", 
           legend.labs=c("Male", "Female"), legend.title="",
           ggtheme=theme_light(), palette=c("royalblue", "salmon"))

# [그림 12-10]
windows(width=7.0, height=5.5)
ggsurvplot(km.group, conf.int=TRUE,  
           linetype="strata", ggtheme=theme_bw(), 
           palette=c("royalblue", "salmon"), xlim=c(0, 600))

# [그림 12-11]
windows(width=7.0, height=5.5)
ggsurvplot(km.group, conf.int=TRUE, 
           linetype="strata", ggtheme=theme_bw(), 
           palette=c("royalblue", "salmon"), fun="event")

# [그림 12-12]
windows(width=7.0, height=5.5)
ggsurvplot(km.group, conf.int=TRUE,  
           linetype="strata", ggtheme=theme_bw(), 
           palette=c("royalblue", "salmon"), fun="cumhaz")

data(colon)
str(colon)
head(colon)

colon.death <- colon[colon$etype==2,]
colon.death$sex <- factor(colon.death$sex, levels=c(0, 1), 
                          labels=c("female", "male"))
colon.death$differ <- factor(colon.death$differ, levels=c(1, 2, 3), 
                             labels=c("well", "moderate", "poor"))

km.fit <- survfit(Surv(time, status) ~ sex + rx + differ, data=colon.death)

# [그림 12-13]
windows(width=9.0, height=7.0)
ggsurv <- ggsurvplot(km.fit, conf.int=TRUE, conf.int.style="step", 
                     ggtheme=theme_bw())
ggsurv$plot +  theme_bw() + 
  theme(legend.position="right", legend.title=element_blank())  +
  facet_grid(rx ~ differ, labeller=label_both)

#######################
## 12.3 콕스회귀분석 ##
#######################

library(survival)
data(lung)
str(lung)
lung$sex <- factor(lung$sex, levels=c(1, 2), labels=c("male", "female"))

cox <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data=lung)
cox

summary(cox)

# [그림 12-14]
library(survminer)
windows(width=7.0, height=5.5)
ggforest(cox, data=lung)

cox.fit <- survfit(cox, data=lung)
cox.fit

# [그림 12-15]
windows(width=7.0, height=5.5)
ggsurvplot(cox.fit, palette="cornflowerblue", ggtheme=theme_minimal(),
           legend="none", xlab="Days", ylab="Overall Survival Probability")

sex.df <- with(lung,
               data.frame(sex=c("male", "female"), 
                          age=rep(mean(age, na.rm=TRUE), 2),
                          ph.ecog=rep(mean(ph.ecog, na.rm=TRUE), 2)))
sex.df

sex.fit <- survfit(cox, newdata=sex.df, data=lung)
summary(sex.fit)

# [그림 12-16]
windows(width=7.0, height=5.5)
ggsurvplot(sex.fit, conf.int=FALSE, ggtheme=theme_minimal(), 
           legend.labs=c("Male", "Female"), legend.title="",
           xlab="Days", ylab="Survival Probability")

summary(lung$ph.ecog)
ph.df <- with(lung,
              data.frame(sex=rep("male", 4), 
                         age=rep(mean(age, na.rm=TRUE), 4),
                         ph.ecog=c(0, 1, 2, 3)))
ph.df

ph.fit <- survfit(cox, newdata=ph.df, data=lung)

# [그림 12-17]
windows(width=7.0, height=5.5)
ggsurvplot(ph.fit, conf.int=FALSE, ggtheme=theme_minimal(), 
           legend.labs=c(0:3), legend.title="ECOG Performance Score (0=good)",
           xlab="Days", ylab="Survival Probability")

# 콕스회귀모델의 비례위험가정 검정
cox.test <- cox.zph(cox)
cox.test

# [그림 12-18]
windows(width=5.5, height=7.0)
ggcoxzph(cox.test)
