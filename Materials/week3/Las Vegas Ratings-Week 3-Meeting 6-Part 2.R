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

################################################
#partition data based on training and test
################################################
set.seed(1234)
#80/20 split
training_test <- c(rep(1,length=trunc((8/10)*nrow(vegas_final))),
                   rep(2,length=(nrow(vegas_final) - trunc((8/10)*nrow(vegas_final)))))

vegas_final$training_test <- sample(training_test) #random permutation 
vegas_final$training_test <- factor(vegas_final$training_test, 
                                       levels=c(1,2), labels=c("TRAIN","TEST"))
table(vegas_final$training_test)
#test~20%
95/472

#training
vegas_train <- subset(vegas_final, training_test == "TRAIN")
glimpse(vegas_train)
summary(vegas_train)
#test
vegas_test <- subset(vegas_final, training_test == "TEST")
glimpse(vegas_test)
summary(vegas_test)

######################
######################
#PREP THE DATA
######################
######################
#what are the locations?
table(vegas_final$Hotel.name)

#create data.frame
location<-data.frame(table(vegas_final$Hotel.name))
location$address<-
  c("3600 S Las Vegas Blvd, Las Vegas, NV 89109",
    "3570 S Las Vegas Blvd, Las Vegas, NV 89109",
    "2880 S Las Vegas Blvd, Las Vegas, NV 89109",
    "3121 S Las Vegas Blvd, Las Vegas, NV 89109",
    "3850 S Las Vegas Blvd, Las Vegas, NV 89109",
    "3575 S Las Vegas Blvd, Las Vegas, NV 89109",
    "2650 S Las Vegas Blvd, Las Vegas, NV 89109",
    "75 E Harmon Ave, Las Vegas, NV 89109",
    "3770 S Las Vegas Blvd, Las Vegas, NV 89109",
    "3655 S Las Vegas Blvd, Las Vegas, NV 89109",
    "3708 S Las Vegas Blvd, Las Vegas, NV 89109",
    "3595 S Las Vegas Blvd, Las Vegas, NV 89109",
    "3325 S Las Vegas Blvd, Las Vegas, NV 89109",
    "3355 S Las Vegas Blvd, Las Vegas, NV 89109",
    "160 E Flamingo Rd, Las Vegas, NV 89109",
    "3300 S Las Vegas Blvd, Las Vegas, NV 89109",
    "3801 S Las Vegas Blvd, Las Vegas, NV 89109",
    "2000 Fashion Show Dr, Las Vegas, NV 89109",
    "255 E Flamingo Rd, Las Vegas, NV 89169",
    "265 E Harmon Ave, Las Vegas, NV 89169",
    "3131 S Las Vegas Blvd, Las Vegas, NV 89109")

location$latitude<-c(36.112031,
                36.119567,
                36.138468,
                36.1302,
                36.098929,
                36.116755,
                36.140187,
                36.107626,
                36.104731,
                36.111937,
                36.110161,
                36.114993,
                36.122401,
                36.121523,
                36.115242,
                36.124547,
                36.09905,
                36.129568,
                36.112931,
                36.10739,
                36.129289)
location$longitude<-c(-115.17695,
                 -115.17336,
                 -115.167277,
                 -115.165152,
                 -115.176592,
                 -115.169103,
                 -115.161362,
                 -115.169513,
                 -115.175528,
                 -115.172119,
                 -115.174051,
                 -115.170609,
                 -115.168742,
                 -115.171682,
                 -115.165576,
                 -115.172347,
                 -115.170464,
                 -115.172647,
                 -115.161558,
                 -115.161423,
                 -115.161838)

#longitude and latitude
library(sp)
library(leaflet)

#coordinates(location) <- c('longitude', 'latitude')
#proj4string(location) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

pal = colorNumeric(palette = "inferno", 
                   domain = location$Freq)

leaflet(location) %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldStreetMap") %>%
  # addMarkers(lng = ~longitude, lat = ~latitude,
  #            popup = ~as.character(Var1)) %>%
  addCircles(col = ~pal(Freq), 
             opacity =1.0,
             lng = ~longitude, lat = ~latitude,
             popup = ~as.character(Var1),
             radius=50) %>%
  addLegend(pal = pal, values = ~Freq)

#Up next: mapping Score and regression results