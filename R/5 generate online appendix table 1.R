#############################
# Michaael Strausz
# Replication files for chapter
# Generate online appendix table 1
# 6/10/2022 : 6/10/2022
#############################

#install and load necessary libraries-----------------------
library(readr)
library(tidyverse)
library(reshape2)
library (scales)
library(readxl)

#generate multiyear dataframe------------------------------
#make 2019
flab.summary.2019<-subset(x=HoC2019, subset = cand2019==1) #only look at candidates
flab.summary.2019<- HoC2019 %>%     # Start with the HoC2019 dataframe
  summarise(
    forlab.mean=mean(support.forlab.100, na.rm = TRUE),
    neutral.forlab=mean(neutral.forlab.100,na.rm=TRUE),
    oppose.forlab=mean(oppose.forlab.100, na.rm = TRUE))

#add year column
flab.summary.2019$year<-c(2019)

#make 2017
flab.summary.2017<- HoR2017 %>%
  summarise(
    forlab.mean=mean(support.forlab.100, na.rm = TRUE),
    neutral.forlab=mean(neutral.forlab.100,na.rm=TRUE),
    oppose.forlab=mean(oppose.forlab.100, na.rm = TRUE))
#add year column
flab.summary.2017$year<-c(2017)

#make 2014
flab.summary.2014<- HoR2014 %>%
  summarise(
    forlab.mean=mean(support.forlab.100, na.rm = TRUE),
    neutral.forlab=mean(neutral.forlab.100,na.rm=TRUE),
    oppose.forlab=mean(oppose.forlab.100, na.rm = TRUE))
#add year column
flab.summary.2014$year<-c(2014)

#make 2012
flab.summary.2012<- HoR2012 %>%
  summarise(
    forlab.mean=mean(support.forlab.100, na.rm = TRUE),
    neutral.forlab=mean(neutral.forlab.100,na.rm=TRUE),
    oppose.forlab=mean(oppose.forlab.100, na.rm = TRUE))
#add year column
flab.summary.2012$year<-c(2012)

#make 2009
flab.summary.2009<- HoR2009 %>%
  summarise(
    forlab.mean=mean(support.forlab.100, na.rm = TRUE),
    neutral.forlab=mean(neutral.forlab.100,na.rm=TRUE),
    oppose.forlab=mean(oppose.forlab.100, na.rm = TRUE))
#add year column
flab.summary.2009$year<-c(2009)

#make 2016
flab.summary.2016<-subset(x=HoC2016, subset = cand2016==1) #only look at candidates
flab.summary.2016<- flab.summary.2016 %>%     
  summarise(
    forlab.mean=mean(support.forlab.100, na.rm = TRUE),
    neutral.forlab=mean(neutral.forlab.100,na.rm=TRUE),
    oppose.forlab=mean(oppose.forlab.100, na.rm = TRUE))
#add year column
flab.summary.2016$year<-c(2016)

#make 2013
flab.summary.2013<-subset(x=HoC2013, subset = cand2013==1) #only look at candidates
flab.summary.2013<- flab.summary.2013 %>%
  summarise(
    forlab.mean=mean(support.forlab.100, na.rm = TRUE),
    neutral.forlab=mean(neutral.forlab.100,na.rm=TRUE),
    oppose.forlab=mean(oppose.forlab.100, na.rm = TRUE))

#add year column
flab.summary.2013$year<-c(2013)

#make 2010
flab.summary.2010<-subset(x=HoC2010, subset = cand2010==1) #only look at candidates
flab.summary.2010<- flab.summary.2010 %>%     # Start with the HoC2019 dataframe
  summarise(
    forlab.mean=mean(support.forlab.100, na.rm = TRUE),
    neutral.forlab=mean(neutral.forlab.100,na.rm=TRUE),
    oppose.forlab=mean(oppose.forlab.100, na.rm = TRUE))
#add year column
flab.summary.2010$year<-c(2010)

#merge all files-----------------------------------
flab.summary<-rbind(flab.summary.2019, flab.summary.2017, flab.summary.2016, 
                    flab.summary.2014, flab.summary.2013, flab.summary.2012,
                    flab.summary.2010, flab.summary.2009)

#remove the annual tables that I had merged
rm(flab.summary.2019, flab.summary.2017, flab.summary.2016, 
   flab.summary.2014, flab.summary.2013, flab.summary.2012,
   flab.summary.2010, flab.summary.2009)

#this tibble supports the claims that I made about changing candidate support
#for foreign labor over time
flab.summary
write.csv(flab.summary,"annualdata.csv", row.names = FALSE)
