#####################################
## CHAPTER 6. Correlation Analysis ##
#####################################

#####################
## 6.1 Correlation ##
#####################

library(MASS)
str(cats)

# [Figure 6-1]
windows(width=7.0, height=5.5)
plot(cats$Hwt ~ cats$Bwt,
     col="forestgreen", pch=19,
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

install.packages("psych")
library(psych)
corr.test(iris[-5])

print(corr.test(iris[-5]), short=FALSE)

old.op <- options(digits=2)
cor(state.x77)
options(old.op)

# [Figure 6-2]
library(psych)
windows(width=7.0, height=7.0)
pairs.panels(state.x77, bg="red", pch=21, hist.col="gold", 
             main="Correlation Plot of US States Data")

# [Figure 6-3]
install.packages("corrgram")
library(corrgram)
windows(width=7.0, height=7.0)
corrgram(state.x77, order=TRUE, lower.panel=panel.shade, 
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of US States Data")

# [Figure 6-4]
library(corrgram)
cols <- colorRampPalette(c("darkgoldenrod4", "burlywood1", 
                           "darkkhaki", "darkgreen"))
windows(width=7.0, height=7.0)
corrgram(state.x77, order=FALSE, col.regions=cols,
         lower.panel=panel.pie, upper.panel=panel.conf, 
         text.panel=panel.txt, main="Corrgram of US States Data")

#############################
## 6.2 Partial Correlation ##
#############################

colnames(mtcars)
mtcars2 <- mtcars[, c("mpg", "cyl", "hp", "wt")]
cor(mtcars2)

install.packages("ggm")
library(ggm)
pcor(c(1, 3, 2, 4), cov(mtcars2))
pcor(c("mpg", "hp", "cyl", "wt"), cov(mtcars2))

pcor.test(pcor(c(1, 3, 2, 4), cov(mtcars2)), q=2, n=nrow(mtcars2))

install.packages("ppcor")
library(ppcor)
pcor(mtcars2)

pcor.test(mtcars2["mpg"], mtcars2["hp"], mtcars2[c("cyl", "wt")])

#/
search()
detach(package:ppcor)
detach(package:ggm)
#/
