
######################################
## R을 이용한 통계데이터분석(제2판) ##
##      (곽기영, 도서출판 청람)     ## 
######################################

#####################
## 제10장 군집분석 ##
#####################

######################
## 10.1 유사도 측정 ##
######################

library(flexclust)
data(nutrient)
head(nutrient, 5)

d <- dist(nutrient)
class(d)
labels(d)
as.matrix(d)[1:5, 1:5]
as.matrix(d)["HAMBURGER", "BEEF STEAK"]

library(MASS)
str(survey)

levels(survey$Sex)
levels(survey$Smoke)
survey.dummy <- survey[c("Sex", "Smoke")]
head(survey.dummy, 5)

library(fastDummies)
survey.dummy <- dummy_cols(survey.dummy, 
                           remove_selected_columns=TRUE,
                           remove_first_dummy=TRUE,
                           ignore_na=TRUE)

head(survey.dummy, 5)

d <- dist(survey.dummy, method="binary")
as.matrix(d)[1:5, 1:5]

library(cluster)
d <- daisy(survey, metric="gower")
as.matrix(d)[1:5, 1:5]

##########################
## 10.2 계층적 군집분석 ##
##########################

library(flexclust)
data(nutrient)
nutrition <- nutrient
row.names(nutrition) <- tolower(row.names(nutrition))
nutrition.scaled <- scale(nutrition)

d <- dist(nutrition.scaled)
clustering.hc <- hclust(d, method="average")

# [그림 10-2]
windows(width=7.0, height=5.5)
plot(clustering.hc, hang=-1, cex=0.9, col="darkgreen",
     xlab="Food", main="Hierarchical Clustering for Meat, Fish and Fowl")

library(NbClust)
nc <- NbClust(nutrition.scaled, distance="euclidean",
              min.nc=3, max.nc=15, method="average")

nc$Best.nc

table(nc$Best.nc[1,])

# [그림 10-3]
windows(width=7.0, height=5.5)
barplot(table(nc$Best.nc[1,]), las=1, col="mistyrose", 
        xlab="Number of Cluster", ylab="Number of Supporting Index",
        main="Number of Clusters Proposed by Indices")

clusters <- cutree(clustering.hc, k=5)
clusters

table(clusters)

# [그림 10-4]
windows(width=7.0, height=5.5)
plot(clustering.hc, hang=-1, cex=0.9, col="darkgreen",
     xlab="Food", main="Hierarchical Clustering for Meat, Fish and Fowl with 5 Clusters")
rect.hclust(clustering.hc, k=5)

aggregate(nutrition, by=list(cluster=clusters), mean)

a <- aggregate(nutrition.scaled, by=list(cluster=clusters), mean)
n <- as.vector(table(clusters))
cbind(a, n)

##########################
## 10.3 분할적 군집분석 ##
##########################

## k-평균 군집분석

head(state.x77)
state.scaled <- scale(state.x77)

library(NbClust)
set.seed(123)
nc <- NbClust(state.scaled, distance="euclidean", 
              min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.nc[1,])

# [그림 10-5]
windows(width=7.0, height=5.5)
barplot(table(nc$Best.nc[1,]), las=1, horiz=TRUE, col="lightsteelblue",
        xlab="Number of Supporting Index", ylab="Number of Cluster",
        main="Number of Clusters Proposed by Indices")

set.seed(123)
clustering.km <- kmeans(state.scaled, centers=3, nstart=25)

clustering.km$cluster
clustering.km$centers
clustering.km$size

aggregate(state.x77, by=list(cluster=clustering.km$cluster), mean)

# [그림 10-6]
windows(width=7.0, height=5.5)
library(cluster)
clusplot(x=state.x77, clus=clustering.km$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0, main="Cluster Plot for US State")

## PAM 군집분석

library(rattle)
head(wine)

library(cluster)
set.seed(123)
clustering.pam <- pam(wine[-1], k=3, stand=TRUE)

clustering.pam$clusinfo

clustering.pam$medoids
clustering.pam$id.med

clustering.pam$clustering
aggregate(wine[-1], by=list(cluster=clustering.pam$clustering), mean)

# [그림 10-7]
windows(width=7.0, height=5.5)
clusplot(clustering.pam, color=TRUE, shade=TRUE, labels=4, lines=0, 
         main="Cluster Plot for Italy Wine")

result.pam <- table(wine$Type, clustering.pam$clustering, 
                    dnn=c("Actual", "Clustered"))
result.pam
mean(wine$Type==clustering.pam$clustering)

library(flexclust)
randIndex(result.pam)
