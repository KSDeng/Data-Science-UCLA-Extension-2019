#PSU: PLACES
#install.packages("caret")
rm(list = ls())
library(caret)
#setwd("/Users/alfonsoberumen/Desktop/UCLA JESIE")

places <- read.table("places.txt",sep="",
                   header=FALSE, 
                   col.names = c("climate",
                                 "housing",
                                 "health",
                                 "crime", 
                                 "trans",
                                 "educate",
                                 "arts",
                                 "recreate",
                                 "econ",
                                 "id"))


str(places)

#data explorer just for fun
#library(DataExplorer)
#create_report(places)

#nine columns excluding the id
places_numeric_temp <- places[1:9]
summary(places_numeric_temp)

places_numeric <- log10(places_numeric_temp)
summary(places_numeric)
#per tutorial, take the log form

#The rotation matrix provides the principal component loadings;
pp_pca_alt<-princomp(places_numeric,cor=TRUE)#standardize the data
names(pp_pca_alt)

pp_pca_alt$center#the means that were subtracted
pp_pca_alt$scale#the scalings applied to each variable.
pp_pca_alt$scores#the scores of the supplied data on the principal components

summary(pp_pca_alt)

#look at the laodings
pp_pca_alt$loadings
sum(pp_pca_alt$loadings[1,])

##################
#REPLICATION
##################
#standardize the data
centered<-as.data.frame(scale(places_numeric))
summary(centered)

#check score calculation
check<-(0.158*centered$climate+
                         0.384*centered$housing+
                         0.410*centered$health+
                         0.259*centered$crime+
                         0.375*centered$trans+
                         0.274*centered$educate+
                         0.474*centered$arts+
                         0.353*centered$recreate+
                         0.164*centered$econ)
head(check)
head(pp_pca_alt$scores)

#######################
#SOME USEFUL PLOTS
#######################
#first 5 loadings
loadings = pp_pca_alt$loadings[,1:5]
loadings <- as.data.frame(loadings)
head(loadings,n=9)
loadings$Variable <- row.names(loadings)
library(tidyr)#to use the gather function
loadings <- gather(loadings, "Component", "Weight", -Variable)
head(loadings)

##Plot of component loadings
loadings$Color = loadings$Weight > 0
ggplot(loadings, aes(x=Variable, y=Weight, fill=Color)) +
  geom_bar(stat='identity', position = "identity", width=.75) + 
  facet_grid(Component ~ ., scales='free_y') +
  guides(fill=FALSE)  +
  ylab('Component Loading') +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5))

#proportion of variance explained
std_dev <- pp_pca_alt$sdev
var <- std_dev^2
vare <- var/sum(var)
plot(vare, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

plot(cumsum(vare), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")