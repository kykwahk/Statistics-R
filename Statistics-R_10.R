
###############################
## R을 이용한 통계데이터분석 ##
## (곽기영, 도서출판 청람)   ## 
###############################

#####################
## 제10장 군집분석 ##
#####################

######################
## 10.1 유사도 측정 ##
######################

install.packages("flexclust")
library(flexclust)
data(nutrient)
head(nutrient, 5)

d <- dist(nutrient)
class(d)
labels(d)
as.matrix(d)[1:5, 1:5]

library(MASS)
str(survey)
levels(survey$Sex)
levels(survey$Smoke)
survey.dummy <- survey[c("Sex", "Smoke")]
head(survey.dummy, 5)

install.packages("fastDummies")
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
clustering.average <- hclust(d, method="average")

# [그림 10-1]
windows(width=7.0, height=5.5)
plot(clustering.average, hang=-1, cex=0.9, col="darkgreen",
     xlab="Food", main="Hierarchical Clustering with Average Linkage")

install.packages("NbClust")
library(NbClust)
nc <- NbClust(nutrition.scaled, distance="euclidean",
              min.nc=3, max.nc=15, method="average")

str(nc)
nc$Best.nc

table(nc$Best.nc[1,])

# [그림 10-2]
windows(width=7.0, height=5.5)
barplot(table(nc$Best.nc[1,]), col="mistyrose", 
        xlab="Number of Cluster", ylab="Number of Supporting Index",
        main="Number of Clusters Proposed by Indices")

clusters <- cutree(clustering.average, k=5)
clusters

table(clusters)

# [그림 10-3]
windows(width=7.0, height=5.5)
plot(clustering.average, hang=-1, cex=0.9, col="darkgreen",
     xlab="Food", 
     main="Hierarchical Clustering with Average Linkage\n Five Clusters")
rect.hclust(clustering.average, k=5)

aggregate(nutrition, by=list(cluster=clusters), mean)

a <- aggregate(nutrition.scaled, by=list(cluster=clusters), mean)
n <- as.vector(table(clusters))
cbind(a, n)

##########################
## 10.3 분할적 군집분석 ##
##########################

head(state.x77)
state.scaled <- scale(state.x77)

library(NbClust)
set.seed(123)
nc <- NbClust(state.scaled, distance="euclidean", 
              min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.nc[1,])

# [그림 10-4]
windows(width=7.0, height=5.5)
barplot(table(nc$Best.nc[1,]), col="lightsteelblue",
        xlab="Number of Cluster", ylab="Number of Supporting Index",
        main="Number of Clusters Proposed by Indices")

set.seed(123)
clustering.km <- kmeans(state.scaled, centers=3, nstart=25)

str(clustering.km)

clustering.km$cluster
clustering.km$centers
clustering.km$size

aggregate(state.x77, by=list(cluster=clustering.km$cluster), mean)

# [그림 10-5]
windows(width=7.0, height=5.5)
library(cluster)
clusplot(x=state.x77, clus=clustering.km$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0, main="Cluster Plot")

install.packages("rattle")
library(rattle)
head(wine)

library(cluster)
set.seed(123)
clustering.pam <- pam(wine[-1], k=3, stand=TRUE)

str(clustering.pam) 

clustering.pam$clusinfo

clustering.pam$medoids
clustering.pam$id.med

clustering.pam$clustering
aggregate(wine[-1], by=list(cluster=clustering.pam$clustering), mean)

# [그림 10-6]
windows(width=7.0, height=5.5)
clusplot(clustering.pam, color=TRUE, shade=TRUE, labels=4, lines=0, 
         main="Cluster Plot")

result.pam <- table(wine$Type, clustering.pam$clustering, 
                    dnn=c("Actual", "Clustered"))
result.pam
mean(wine$Type==clustering.pam$clustering)

library(flexclust)
randIndex(result.pam)
