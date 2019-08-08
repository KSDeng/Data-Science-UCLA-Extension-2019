#install.packages("DataExplorer") - install this package
library(DataExplorer)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)

#setwd - set working directory or folder path
#notice the direction of the slashes
#setwd("/Users/alfonsoberumen/Desktop/UCLA JESIE")

#import the CSV file
vegas <- read.csv("LasVegasTripAdvisorReviews-Dataset.csv", header=TRUE, sep=",") 
dplyr::glimpse(vegas)

#data profiling
#create_report(vegas)
#specific to the target we are studying
create_report(vegas,y="Score")

#take a step back and learn this
summary(vegas)

#EDA: our target
#boxplot
boxplot(vegas$Score)
score_boxplot<-ggplot(vegas, aes(x="", y=Score, 
                              color=Score)) +
  geom_boxplot()
score_boxplot

#frequency
#counts
score_frequency <- ggplot(vegas, aes(Score)) + 
  geom_bar()
score_frequency 

##############################
#EDA: categorical variables
##############################
#User country
country_plot<-ggplot(vegas, aes(User.country)) +
  geom_bar(color = "red",
          fill = "white")+
  coord_flip()

country_plot
table(vegas$User.country)

#continent
user_continent_plot<-ggplot(vegas, 
                            aes(x=User.continent, 
                                y=Score, 
                                color=User.continent)) +
  geom_boxplot()
user_continent_plot

user_continent_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                                        User.continent)) + 
  geom_bar()+
  geom_text(stat = "count",
            aes(label=..count..),
            position=position_stack())
user_continent_plot2

user_continent_plot3<-ggplot(vegas, aes(fill=as.factor(Score),
                                        User.continent)) + 
  geom_bar(stat = "count",position="fill")
user_continent_plot3
xtabs(~vegas$Score+vegas$User.continent)
#America
144/(144+86+39+20+6)
#South America
4/(4+2+1+0+0)

#addtional check
psych::describeBy(vegas$Score, 
                  vegas$User.continent, 
                  mat = TRUE)

#Period of stay
period_plot<-ggplot(vegas, 
                    aes(x=Period.of.stay, 
                        y=Score, 
                              color=Period.of.stay)) +
  geom_boxplot()
period_plot

period_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                                Period.of.stay)) + 
  geom_bar(stat = "count",position="fill")
period_plot2
xtabs(~vegas$Score+vegas$Period.of.stay)

#install.packages("psych")
library(psych)
#addtional check
psych::describeBy(vegas$Score, 
           vegas$Period.of.stay, 
           mat = TRUE) 

#Review month
month_plot<-ggplot(vegas, 
                   aes(x=Review.month, 
                       y=Score, 
                       color=Review.month)) +
  geom_boxplot()
month_plot

month_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                               Review.month)) + 
  geom_bar(stat = "count",position="fill")
month_plot2
#cross tab
xtabs(~vegas$Score+vegas$Review.month)

#addtional check
psych::describeBy(vegas$Score, 
                  vegas$Review.month, 
                  mat = TRUE)

#Review weekday
weekday_plot<-ggplot(vegas, 
                     aes(x=Review.weekday, 
                         y=Score, 
                         color=Review.weekday)) +
  geom_boxplot()
weekday_plot

weekday_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                                 Review.weekday)) + 
  geom_bar(stat = "count",position="fill")
weekday_plot2
#cross tab
xtabs(~vegas$Score+vegas$Review.weekday)

#addtional check
psych::describeBy(vegas$Score, 
                  vegas$Review.weekday, 
                  mat = TRUE)

#Traveler type
traveler_plot<-ggplot(vegas, 
                    aes(x=Traveler.type, 
                        y=Score, 
                        color=Traveler.type)) +
  geom_boxplot()
traveler_plot

traveler_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                                  Traveler.type)) + 
  geom_bar(stat = "count",position="fill")
traveler_plot2
xtabs(~vegas$Score+vegas$Traveler.type)

#addtional check
psych::describeBy(vegas$Score, 
                  vegas$Traveler.type, 
                  mat = TRUE) 

#Pool
pool_plot<-ggplot(vegas, 
                      aes(x=Pool, 
                          y=Score, 
                          color=Pool)) +
  geom_boxplot()
pool_plot

pool_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                              Pool)) + 
  geom_bar(stat = "count",position="fill")
pool_plot2
#cross tab
xtabs(~vegas$Score+vegas$Pool)

#addtional check
psych::describeBy(vegas$Score, 
                  vegas$Pool, 
                  mat = TRUE) 

#Gym
gym_plot<-ggplot(vegas, 
                  aes(x=Gym, 
                      y=Score, 
                      color=Gym)) +
  geom_boxplot()
gym_plot

gym_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                              Gym)) + 
  geom_bar(stat = "count",position="fill")
gym_plot2
#cross tab
xtabs(~vegas$Score+vegas$Gym)

#addtional check
psych::describeBy(vegas$Score, 
                  vegas$Gym, 
                  mat = TRUE) 

#Tennis court
tennis_plot<-ggplot(vegas, 
                 aes(x=Tennis.court, 
                     y=Score, 
                     color=Tennis.court)) +
  geom_boxplot()
tennis_plot

tennis_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                                Tennis.court)) + 
  geom_bar(stat = "count",position="fill")
tennis_plot2
#cross tab
xtabs(~vegas$Score+vegas$Tennis.court)

#addtional check
psych::describeBy(vegas$Score, 
                  vegas$Tennis.court, 
                  mat = TRUE)

#Spa
spa_plot<-ggplot(vegas, 
                    aes(x=Spa, 
                        y=Score, 
                        color=Spa)) +
  geom_boxplot()
spa_plot

spa_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                             Spa)) + 
  geom_bar(stat = "count",position="fill")
spa_plot2
#cross tab
xtabs(~vegas$Score+vegas$Spa)

#addtional check
psych::describeBy(vegas$Score, 
                  vegas$Spa, 
                  mat = TRUE)

#Casino
casino_plot<-ggplot(vegas, 
                 aes(x=Casino, 
                     y=Score, 
                     color=Casino)) +
  geom_boxplot()
casino_plot

casino_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                                Casino)) + 
  geom_bar(stat = "count",position="fill")
casino_plot2
#cross tab
xtabs(~vegas$Score+vegas$Casino)

#addtional check
psych::describeBy(vegas$Score, 
                  vegas$Casino, 
                  mat = TRUE)

#Free internet
internet_plot<-ggplot(vegas, 
                    aes(x=Free.internet, 
                        y=Score, 
                        color=Free.internet)) +
  geom_boxplot()
internet_plot

internet_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                                  Free.internet)) + 
  geom_bar(stat = "count",position="fill")
internet_plot2
#cross tab
xtabs(~vegas$Score+vegas$Free.internet)

#addtional check
psych::describeBy(vegas$Score, 
                  vegas$Free.internet, 
                  mat = TRUE)

#Hotel name
hotel_plot<-ggplot(vegas, 
                      aes(x=Hotel.name, 
                          y=Score, 
                          color=Hotel.name)) +
  geom_boxplot()
hotel_plot

hotel_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                                  Hotel.name)) + 
  geom_bar(stat = "count",position="fill")
hotel_plot2
#cross tab
#xtabs(~vegas$Score+vegas$Hotel.name)

#addtional check
psych::describeBy(vegas$Score, 
                  vegas$Hotel.name, 
                  mat = TRUE)

#Hotel stars
hotel_stars_plot<-ggplot(vegas, 
                   aes(x=Hotel.stars, 
                       y=Score, 
                       color=Hotel.stars)) +
  geom_boxplot()
hotel_stars_plot

hotel_stars_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                               Hotel.stars)) + 
  geom_bar(stat = "count",position="fill")
hotel_stars_plot2
#cross tab
xtabs(~vegas$Score+vegas$Hotel.stars)

#addtional check
psych::describeBy(vegas$Score, 
                  vegas$Hotel.stars, 
                  mat = TRUE)

#Up next: EDA: numeric variables