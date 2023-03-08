
######################################
## R을 이용한 통계데이터분석(제2판) ##
##      (곽기영, 도서출판 청람)     ## 
######################################

#############################
## 제13장 구조방정식모델링 ##
#############################

####################
## 13.2 분석 절차 ##
####################

library(lavaan)
str(PoliticalDemocracy)

?PoliticalDemocracy

## 확인적 요인분석

cfa <- "ind60 =~ x1 + x2 + x3
        dem60 =~ y1 + y2 + y3 + y4
        dem65 =~ y5 + y6 + y7 + y8"

cfa(model=cfa, data=PoliticalDemocracy)

?lavOptions
lavOptions("std.lv")

fit <- cfa(model=cfa, data=PoliticalDemocracy)
summary(fit, fit.measures=TRUE, standardized=TRUE)

## 모수 추정치

parameterEstimates(fit, standardized=TRUE)
standardizedsolution(fit)

coef(fit)

library(dplyr) 
library(knitr)
options(knitr.kable.NA="")
parameterEstimates(fit, standardized=TRUE) %>% 
  filter(op=="=~") %>% 
  mutate(stars=ifelse(pvalue < 0.001, "***", 
                      ifelse(pvalue < 0.01, "**", 
                             ifelse(pvalue < 0.05, "*", "")))) %>%
  select("Latent Factor"=lhs, Indicator=rhs, B=est, SE=se, 
         Z=z, "p-value"=pvalue, Sig.=stars, Beta=std.all) %>% 
  kable(digits=3, format="pandoc", caption="Factor Loadings")

## 상관계수 잔차 행렬

residuals(fit, type="cor")$cov

resid.cor <- residuals(fit, type="cor")$cov
resid.cor[upper.tri(resid.cor, diag=TRUE)] <- NA
library(knitr)
options(knitr.kable.NA="")
kable(resid.cor, digits=2, format="pandoc", caption="Residual Correlations")

## 적합도 지표

fitMeasures(fit)

fitMeasures(fit, c("chisq", "df", "pvalue", "gfi", "rmsea", "cfi"))

## 수정 지표와 모델 개선

summary(fit, modindices=TRUE)
modindices(fit)

library(dplyr) 
library(knitr)
modindices(fit) %>%
  filter(op=="=~") %>%
  kable(digits=3, format="pandoc", 
        caption="Modification Indices for Factor Loadings")

modindices(fit, sort.=TRUE, minimum.value=3)

cfa2 <- "ind60 =~ x1 + x2 + x3
         dem60 =~ y1 + y2 + y3 + y4
         dem65 =~ y5 + y6 + y7 + y8
         y1 ~~ y5
         y2 ~~ y4 + y6
         y3 ~~ y7
         y4 ~~ y8
         y6 ~~ y8"
fit2 <- cfa(model=cfa2, data=PoliticalDemocracy)
fit2

anova(fit, fit2)

fitMeasures(fit2, c("chisq", "df", "pvalue", "gfi", "rmsea", "cfi"))

library(dplyr)
library(tibble)
library(magrittr)
contrastFit <- function(...) {
  m <- list(...)
  sapply(m, fitMeasures) %>% 
    set_colnames(paste0("Model", 1:length(m))) %>%
    as.data.frame() %>%
    rownames_to_column("Fit_Measures") %>% 
    slice(match(c("chisq", "df", "pvalue", 
                  "gfi", "rmsea", "cfi"), Fit_Measures)) %>%
    mutate(Fit_Measures=c("Chi-square", "df", "p-value",
                          "GFI", "RMSEA", "CFI"))}

library(stargazer)
contrastFit(fit, fit2) %>% 
  stargazer(type="text", title="Model Comparison", summary=FALSE,
            digits=3, digits.extra=0, rownames=FALSE)

library(semTools)
summary(compareFit(fit, fit2))

standardizedsolution(fit2)
library(dplyr)
library(stargazer)
standardizedsolution(fit2) %>% 
  filter(op=="=~") %>% 
  mutate(stars=ifelse(pvalue < 0.001, "***", 
                      ifelse(pvalue < 0.01, "**", 
                             ifelse(pvalue < 0.05, "*", "")))) %>%
  select(Construct=lhs, Item=rhs, "Factor Loading"=est.std, 
         Z=z, "p-value"=pvalue, Sig.=stars) %>% 
  stargazer(type="text", title="Convergent Validity: Factor Loadings", summary=FALSE,
            digits=3, digits.extra=0, rownames=FALSE)

## 측정모델 경로도

# [그림 13-5]
library(semPlot)
windows(width=7.0, height=5.5)
semPaths(fit2, what="std", layout="tree2", edge.label.cex=1, edge.color="darkgreen", 
         color=list(lat="orange", man="palegoldenrod"), fade=FALSE, 
         style="lisrel", rotation=4, curvature=2)

## 신뢰도

library(semTools)
reliability(fit2)

library(dplyr)
library(tibble)
library(stargazer)
reliability(fit2) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("Construct") %>%
  select(Construct, "Composite Reliability"=omega, 
         "Average Variance Extracted"=avevar, "Cronbach's alpha"=alpha) %>%
  stargazer(type="text", title="Convergent Validity and Reliability", 
            summary=FALSE, digits=3, digits.extra=0, rownames=FALSE)

## 타당도

lavInspect(fit2, what="cor.lv")

?lavInspect

library(semTools)
library(dplyr)
library(tibble)
library(stargazer)
lavInspect(fit2, what="cor.lv") %>%
  as.data.frame() %>%
  rownames_to_column("Construct") %>%
  cbind(Square_Root_of_AVE=sqrt(reliability(fit2)["avevar",])) %>%
  stargazer(type="text", title="Discriminant Validity", 
            summary=FALSE, digits=3, digits.extra=0, rownames=FALSE)

## 구조모델

sem <- "# measurement model
          ind60 =~ x1 + x2 + x3
          dem60 =~ y1 + y2 + y3 + y4
          dem65 =~ y5 + y6 + y7 + y8
        # residual correlations
          y1 ~~ y5
          y2 ~~ y4 + y6
          y3 ~~ y7
          y4 ~~ y8
          y6 ~~ y8
       # regressions
          dem60 ~ ind60
          dem65 ~ ind60 + dem60"
     
fit <- sem(model=sem, data=PoliticalDemocracy)

fitMeasures(fit, c("chisq", "df", "pvalue", "gfi", "rmsea", "cfi"))

library(dplyr)
library(stargazer)
standardizedsolution(fit) %>% 
  filter(op=="~") %>% 
  mutate(stars=ifelse(pvalue < 0.001, "***", 
                      ifelse(pvalue < 0.01, "**", 
                             ifelse(pvalue < 0.05, "*", "")))) %>%
  select(Dependent=lhs, Independent=rhs, Coefficient=est.std, 
         Z=z, "p-value"=pvalue, Sig.=stars) %>% 
  stargazer(type="text", title="Regression Coefficients", summary=FALSE,
            digits=3, digits.extra=0, rownames=FALSE)

lavInspect(fit, what="rsquare")

## 구조모델 경로도

# [그림 13-6]
library(semPlot)
windows(width=7.0, height=5.5)
semPaths(fit, what="std", layout="tree2", edge.label.cex=1, edge.color="royalblue", 
         color=list(lat="lightcoral", man="lavenderblush"), fade=FALSE, 
         style="lisrel", curvature=2)

######################################
## 13.3 매개효과분석과 조절효과분석 ##
######################################

## 매개효과분석

sem.med <- "# measurement model
              ind60 =~ x1 + x2 + x3
              dem60 =~ y1 + y2 + y3 + y4
              dem65 =~ y5 + y6 + y7 + y8
            # residual correlations
              y1 ~~ y5
              y2 ~~ y4 + y6
              y3 ~~ y7
              y4 ~~ y8
              y6 ~~ y8
            # regressions
              dem60 ~ a*ind60
              dem65 ~ cp*ind60 + b*dem60
            # indirect effect: ab
              ab := a*b
            # total effect: c
              c := cp + (a*b)"

library(lavaan)
set.seed(123)
fit.med <- sem(model=sem.med, data=PoliticalDemocracy, 
               se="bootstrap", bootstrap=500)

summary(fit.med, standardized=TRUE)

parameterEstimates(fit.med, standardized=TRUE)
library(dplyr)
library(stargazer)
parameterEstimates(fit.med, standardized=TRUE) %>% 
  filter(op=="~" | op==":=") %>% 
  mutate(stars=ifelse(pvalue < 0.001, "***", 
                      ifelse(pvalue < 0.01, "**", 
                             ifelse(pvalue < 0.05, "*", "")))) %>%
  select(LHS=lhs, RHS=rhs, Label=label, Estimate=est, 
         Z=z, "p-value"=pvalue, Sig.=stars) %>% 
  stargazer(type="text", title="Regression Coefficients and Defined Parameters", 
            summary=FALSE, digits=3, digits.extra=0, rownames=FALSE)

## 조절효과분석

library(lavaan)
library(semTools)
PoliticalDemocracy.mod <- indProd(PoliticalDemocracy, var1=c("x1", "x2", "x3"),
                                  var2=c("y1", "y2", "y3", "y4"), match=FALSE,
                                  meanC=TRUE, residualC=FALSE, doubleMC=TRUE)
names(PoliticalDemocracy.mod)

sem.mod <- "# measurement model
              ind60 =~ x1 + x2 + x3
              dem60 =~ y1 + y2 + y3 + y4
              dem65 =~ y5 + y6 + y7 + y8
            # interaction term
              ind60dem60 =~ x1.y1 + x1.y2 + x1.y3 + x1.y4 + 
                            x2.y1 + x2.y2 + x2.y3 + x2.y4 + 
                            x3.y1 + x3.y2 + x3.y3 + x3.y4
            # residual correlations
              y1 ~~ y5
              y2 ~~ y4 + y6
              y3 ~~ y7
              y4 ~~ y8
              y6 ~~ y8
            # regressions
              dem65 ~ ind60 + dem60 + ind60dem60"

fit.mod <- sem(model=sem.mod, data=PoliticalDemocracy.mod)
summary(fit.mod, standardized=TRUE)

parameterEstimates(fit.mod, standardized=TRUE)
library(dplyr)
library(stargazer)
parameterEstimates(fit.mod, standardized=TRUE) %>% 
  filter(op=="~") %>% 
  mutate(stars=ifelse(pvalue < 0.001, "***", 
                      ifelse(pvalue < 0.01, "**", 
                             ifelse(pvalue < 0.05, "*", "")))) %>%
  select(LHS=lhs, RHS=rhs, Coefficient=est, 
         Z=z, "p-value"=pvalue, Sig.=stars) %>% 
  stargazer(type="text", title="Regression Coefficients", summary=FALSE,
            digits=3, digits.extra=0, rownames=FALSE)

## 조절매개효과분석

library(lavaan)
set.seed(111)
PoliticalDemocracy$x4 <- 
  scale(rnorm(nrow(PoliticalDemocracy), mean=0.5, sd=0.1)*
          rowMeans(PoliticalDemocracy[, c("x1", "x2", "x3")]),
        center=TRUE, scale=FALSE)

library(semTools)
PoliticalDemocracy.modmed <- indProd(PoliticalDemocracy, var1=c("x1", "x2", "x3"),
                                     var2="x4", match=FALSE, meanC=TRUE, 
                                     residualC=FALSE, doubleMC=TRUE)
names(PoliticalDemocracy.modmed)

sem.modmed <- "# measurement model
                 ind60 =~ x1 + x2 + x3
                 dem60 =~ y1 + y2 + y3 + y4
                 dem65 =~ y5 + y6 + y7 + y8
                 edu =~ x4
               # interaction term
                 ind60edu =~ x1.x4 + x2.x4 + x3.x4
               # residual correlations
                 y1 ~~ y5
                 y2 ~~ y4 + y6
                 y3 ~~ y7
                 y4 ~~ y8
                 y6 ~~ y8
               # regressions
                 dem60 ~ a1*ind60 + a2*edu + a3*ind60edu
                 dem65 ~ cp*ind60 + b*dem60
               # mean and variance of moderator
                 edu ~ edu.mean*1
                 edu ~~ edu.var*edu
               # values of mean and sd of moderator
                 mean.edu := edu.mean
                 sd.edu := sqrt(edu.var)
               # indirect effect conditional on moderator: 
               # (a1 + a3*ModerationValue)*b
                 indirect.low := (a1 + a3*(edu.mean-sqrt(edu.var)))*b
                 indirect.high := (a1 + a3*(edu.mean+sqrt(edu.var)))*b
               # direct effect: cp
                 direct := cp
               # total effect: direct effect + indirect effect
                 total.low := direct + indirect.low
                 total.high := direct + indirect.high
               # index of moderated mediation
                 mod.med.a3b := a3*b"

set.seed(123)
fit.modmed <- sem(model=sem.modmed, data=PoliticalDemocracy.modmed, 
                  se="bootstrap", bootstrap=500)
summary(fit.modmed, standardized=TRUE)

library(dplyr)
library(stargazer)
parameterEstimates(fit.modmed, standardized=TRUE) %>% 
  filter(op=="~" | op==":=") %>% 
  mutate(stars=ifelse(pvalue < 0.001, "***", 
                      ifelse(pvalue < 0.01, "**", 
                             ifelse(pvalue < 0.05, "*", "")))) %>%
  select(LHS=lhs, RHS=rhs, Label=label, Estimate=est, 
         Z=z, "p-value"=pvalue, Sig.=stars) %>% 
  stargazer(type="text", title="Regression Coefficients and Defined Parameters", 
            summary=FALSE, digits=3, digits.extra=0, rownames=FALSE)

#######################
## 13.4 다중집단분석 ##
#######################

data(HS.data, package="sem")
names(HS.data)

sem <- "# measurement model
          speed =~ addition + code + counting + straight
          memory =~ wordr + numberr + figurer + object + numberf + figurew
          math =~  deduct + numeric + problemr + series + arithmet
        # regressions
          speed ~ math 
          memory ~ math + speed"

library(lavaan)
fit <- sem(model=sem, data=HS.data)
summary(fit, fit.measures=TRUE, standardized=TRUE)

library(dplyr)
library(stargazer)
parameterEstimates(fit) %>% 
  filter(op=="~") %>% 
  mutate(stars=ifelse(pvalue < 0.001, "***", 
                      ifelse(pvalue < 0.01, "**", 
                             ifelse(pvalue < 0.05, "*", "")))) %>%
  select(Dependent=lhs, Independent=rhs, Coefficient=est, 
         Z=z, "p-value"=pvalue, Sig.=stars) %>%
  stargazer(type="text", title="Regression Coefficients", summary=FALSE,
            digits=3, digits.extra=0, rownames=FALSE)

fit1 <- sem(model=sem, data=HS.data, group="Gender")
summary(fit1, fit.measures=TRUE, standardized=TRUE)

parameterEstimates(fit1) %>% 
  filter(op=="~") %>% 
  mutate(stars=ifelse(pvalue < 0.001, "***", 
                      ifelse(pvalue < 0.01, "**", 
                             ifelse(pvalue < 0.05, "*", "")))) %>%
  select(Group=group, Dependent=lhs, Independent=rhs, Coefficient=est, 
         Z=z, "p-value"=pvalue, Sig.=stars) %>%
  stargazer(type="text", title="Regression Coefficients", summary=FALSE,
            digits=3, digits.extra=0, rownames=FALSE)

sem.const <- "# measurement model
                speed =~ addition + code + counting + straight
                memory =~ wordr + numberr + figurer + object + numberf + figurew
                math =~  deduct + numeric + problemr + series + arithmet
              # regressions
                speed ~ math
                memory ~ math + c(b, b)*speed"

fit2 <- sem(model=sem.const, data=HS.data, group="Gender")
summary(fit2, fit.measures=TRUE, standardized=TRUE)
parameterEstimates(fit2) %>% 
  filter(op=="~") %>% 
  mutate(stars=ifelse(pvalue < 0.001, "***", 
                      ifelse(pvalue < 0.01, "**", 
                             ifelse(pvalue < 0.05, "*", "")))) %>%
  select(Group=group, Dependent=lhs, Independent=rhs, Coefficient=est, 
         Z=z, "p-value"=pvalue, Sig.=stars) %>%
  stargazer(type="text", title="Regression Coefficients", summary=FALSE,
            digits=3, digits.extra=0, rownames=FALSE)

anova(fit1, fit2)

library(dplyr)
library(tibble)
library(magrittr)
contrastFit <- function(...) {
  m <- list(...)
  sapply(m, fitMeasures) %>% 
    set_colnames(paste0("Model", 1:length(m))) %>%
    as.data.frame() %>%
    rownames_to_column("Fit_Measures") %>% 
    slice(match(c("chisq", "df", "pvalue", 
                  "gfi", "rmsea", "cfi"), Fit_Measures)) %>%
    mutate(Fit_Measures=c("Chi-square", "df", "p-value",
                          "GFI", "RMSEA", "CFI"))}
library(stargazer)
contrastFit(fit1, fit2) %>% 
  stargazer(type="text", title="Model Comparison", summary=FALSE,
            digits=3, digits.extra=0, rownames=FALSE)

fit3 <- sem(model=sem, data=HS.data, 
            group="Gender", group.equal=c("regressions"))

anova(fit1, fit3)

contrastFit(fit1, fit2, fit3) %>% 
  stargazer(type="text", title="Model Comparison", summary=FALSE,
            digits=3, digits.extra=0, rownames=FALSE)
