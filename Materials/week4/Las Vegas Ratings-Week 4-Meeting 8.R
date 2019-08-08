library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(car)

#setwd - set working directory or folder path
#notice the direction of the slashes
setwd("/Users/alfonsoberumen/Desktop/UCLA JESIE")

#load saved data
vegas_final<-readRDS("vegas_modified.rds")

#####################################
#LIMIT THE VARIABLES FOR CLUSTERING
#####################################
#LET'S ACTUALLY USE THE FULL DATASET
#IN TERMS OF OBSERVATIONS
cluster_data<-vegas_final %>%
  select(Nr..reviews,
         Nr..hotel.reviews,
         Helpful.votes,
         Score,
         Member.years)

library(corrplot)
corrplot(cor(cluster_data), type="upper", 
         method="ellipse", 
         tl.cex=0.9)

#scale the data
cluster_data_norm <- as.data.frame(scale(cluster_data))

summary(cluster_data)
summary(cluster_data_norm)
#mean = 0, sd = 1
sd(cluster_data_norm$Score)

#####################################
#####################################
#Cluster Analysis
#####################################
#####################################
set.seed(1234)
km.out=kmeans(cluster_data_norm,5,
              nstart=50)
km.out
km.out$tot.withinss
#try 2
km.out=kmeans(cluster_data_norm,2,
              nstart=50)
km.out
km.out$tot.withinss

#better way!
#install.packages("factoextra")
library(factoextra)
fviz_nbclust(cluster_data_norm, kmeans, method = "wss")
#fviz_nbclust(cluster_data_norm, kmeans, method = "gap_stat")
#ELBOW IS AT AROUND 3
km.out=kmeans(cluster_data_norm,3,
              nstart=50)
km.out
km.out$tot.withinss

#note that n-start will pick the best cluster based 
#on multiple initial configurations.  
#Comment on the cluster means 
#(i.e., what appears to be important to each group; 
#remember that means are going to take 
#on a value from 0 to 1 and the variables are 
#measures of stakeholder importance)

##################
#CREATE PLOTS
##################
plot(cluster_data_norm,col=km.out$cluster)
points(km.out$center,col=1:3,pch=8,cex=1)

#individual
plot(Nr..reviews~Nr..hotel.reviews,
     data=cluster_data_norm,col=c(1:3)[km.out$cluster])
with(cluster_data_norm, text(Nr..reviews~Nr..hotel.reviews, 
                    labels = km.out$cluster, pos = 4))

plot(Nr..reviews~Helpful.votes,
     data=cluster_data_norm,col=c(1:3)[km.out$cluster])
with(cluster_data_norm, text(Nr..reviews~Helpful.votes, 
                             labels = km.out$cluster, pos = 4))

plot(Nr..reviews~Score,
     data=cluster_data_norm,col=c(1:3)[km.out$cluster])
with(cluster_data_norm, text(Nr..reviews~Score, 
                             labels = km.out$cluster, pos = 4))

plot(Helpful.votes~Score,
     data=cluster_data_norm,col=c(1:3)[km.out$cluster])
with(cluster_data_norm, text(Helpful.votes~Score, 
                             labels = km.out$cluster, pos = 4))
##################
#heirarchal
##################
d <- dist(cluster_data_norm, method = "euclidean")
hcl <- hclust(d, method = "ward.D2" )

#Dissimilarity matrix
d <- dist(cluster_data_norm, method = "euclidean")
#Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )
plot(hc1, ylab='distance', xlab='', sub='', main='')

#number to extraxct
sub_grp<-cutree(hc1,k=5)
sub_grp<-cutree(hc1,k=3)

table(sub_grp)#number in each cluster

cluster_data_norm2<-cluster_data_norm  %>% 
  mutate(cluster = sub_grp)
head(cluster_data_norm2)
table(cluster_data_norm2$cluster)

#summary
library(psych)
#addtional check
psych::describeBy(cluster_data_norm2$Nr..reviews,
                  cluster_data_norm2$cluster, 
                  mat = TRUE) 
psych::describeBy(cluster_data_norm2$Nr..hotel.reviews,
                  cluster_data_norm2$cluster
                  mat = TRUE) 
psych::describeBy(cluster_data_norm2$Helpful.votes,
                  cluster_data_norm2$cluster, 
                  mat = TRUE) 
psych::describeBy(cluster_data_norm2$Score,
                  cluster_data_norm2$cluster,
                  mat = TRUE) 
psych::describeBy(cluster_data_norm2$Member.years,
                  cluster_data_norm2$cluster, 
                  mat = TRUE) 

#show it on the plot
plot(hc1, cex = 0.6)
rect.hclust(hc1, k = 3, border = 2:5)

summary(hcl)

#PCA
library(DataExplorer)
create_report(vegas_final)
# plot_prcomp(vegas_final, 
#             variance_cap = 0.9, 
#             nrow = 2L, 
#             ncol = 2L)