###################################
## CHAPTER 9. Dimension Analysis ##
###################################

######################################
## 9.1 Principal Component Analysis ##
######################################

str(state.x77)
colnames(state.x77)

pca <- prcomp(state.x77, scale=TRUE)
summary(pca)

# [Figure 9-2]
windows(width=7.0, height=5.5)
plot(pca, type="l", pch=19, lwd=2, col="red", main="Scree Plot")

round(pca$rotation, 3)

round(scale(state.x77) %*% pca$rotation, 3)

round(pca$x, 3)

round(pca$x[,c(1, 2)], 3)

round(cor(pca$x), 3)

# [Figure 9-3]
windows(width=5.5, height=5.5)
par(mai=c(0.9,0.1,0.7,0.1))
biplot(pca, cex=c(0.5, 0.75), main="Biplot\n\n")

#########################
## 9.2 Factor Analysis ##
#########################

install.packages("ade4")
library(ade4)
data(olympic)
str(olympic)

library(psych)

# [Figure 9-5]
windows(width=7.0, height=5.5)
fa.parallel(olympic$tab, fm="ml", fa="fa", n.iter=100)

install.packages("nFactors")
library(nFactors)
nScree(olympic$tab)

eigen(cor(olympic$tab))

fa <- factanal(olympic$tab, factors=2, scores="regression")
fa

fa$loadings
print(fa$loadings, cutoff=0.001)
?loadings

round(fa$uniquenesses, 3)
round(1 - fa$uniquenesses, 3)

round(fa$loadings %*% t(fa$loadings), 3)

round(cor(olympic$tab), 3)
round(cor(olympic$tab) - 
        (fa$loadings %*% t(fa$loadings) + diag(fa$uniquenesses)), 3)

# [Figure 9-6]
windows(width=7.0, height=5.5)
factor.plot(fa, labels=colnames(olympic$tab), pch=20, pos=4, title="Factor Plot")

# [Figure 9-7]
windows(width=7.0, height=5.5)
library(gplots)
library(RColorBrewer)
heatmap.2(abs(fa$loadings), col=brewer.pal(9, "Blues"),trace="none", key=FALSE,
          dend="none", cexCol=1.2, main="\n\n\n\nFactor Loadings")

# [Figure 9-8]
windows(width=7.0, height=5.5)
install.packages("semPlot")
library(semPlot)
semPaths(fa, what="est", residuals=FALSE,
         cut=0.3, posCol=c("white", "darkgreen"), negCol=c("white", "red"),
         edge.label.cex=0.75, title=TRUE)

fa.scores <- fa$scores
fa.scores

# [Figure 9-9]
windows(width=7.0, height=7.0)
colnames(fa.scores) <- c("Run", "Throw")
heatmap.2(fa.scores, col=brewer.pal(9, "GnBu"),trace="none", key=FALSE, 
          dend="none", cexCol=1.2, main="\n\n\n\n\n\nFactor Scores by Athletes")

library(psych)
fa <- fa(olympic$tab, nfactors=2, rotate="varimax", fm="ml")
fa

fa$loadings
fa$scores
fa$weights

# [Figure 9-10]
windows(width=7.0, height=5.5)
fa.diagram(fa, simple=FALSE, cut=0.3, digits=2, col="blue", adj=2, 
           e.size=0.08, rsize=2)

###################################
## 9.3 Multidimensional Scaling ##
###################################

str(eurodist)
labels(eurodist)
as.matrix(eurodist)[1:5, 1:5]

eurocity.mds <- cmdscale(d=eurodist)
head(eurocity.mds)

# [Figure 9-11]
windows(width=7.0, height=5.5)
plot(eurocity.mds, type="n", main="Multidimensional Scaling Plot")
text(eurocity.mds, rownames(eurocity.mds), col="maroon", cex=0.7)

str(USJudgeRatings)

USJudgeRatings.dist <- dist(USJudgeRatings)
USJudgeRatings.mds <- cmdscale(USJudgeRatings.dist)

# [Figure 9-12]
windows(width=7.0, height=5.5)
plot(USJudgeRatings.mds, type="n", main="Multidimensional Scaling Plot")
text(USJudgeRatings.mds, rownames(USJudgeRatings), col="blue", cex=0.6)

str(mtcars)

library(cluster)
mtcars.dist <- daisy(mtcars, metric="gower")

library(MASS)
mtcars.mds <- isoMDS(mtcars.dist)
str(mtcars.mds)

# [Figure 9-13]
windows(width=7.0, height=5.5)
plot(mtcars.mds$points, type="n", main="Multidimensional Scaling Plot")
text(mtcars.mds$points, rownames(mtcars), col="purple", cex=0.7)
