#############################
# Michael Strausz
# Replication files for chapter
# On 2019 HoC election
# Generate Table 8.1
# 6/13/2022 : 3/11/2024
#############################

#install and load necessary libraries-----------------------
library(tidyverse)

#bring in data from 2013 to 2019 dataframe-------------------
#add a vector for 2019 candidates that also ran in 2013 ----------------------
HoC2013$cand1319<-HoC2013$NAME #generate a new vector for candidates in 13&19

#exclude candidates that weren't candidates in 2019 from candidate1319 vector
HoC2013 <- HoC2013 %>%
  mutate(cand1319 = ifelse(cand1319 %in% HoC2019$NAME, cand1319, ""))

#make a subset of 2013 that only includes those that were also candidates in 2019
HoC2013a <- HoC2013 %>%
  filter(cand1319!="")%>%
  group_by(cand1319) %>% 
  summarise(Q4_16=mean(Q4_16)
  )

#rename the columns in the new subset
HoC2013a<-HoC2013a %>% 
  rename(
    NAME=cand1319,
    FoRLab2013=Q4_16
  )

#merge the 2013 forlab variable variable into 2019
HoC2019<-merge(HoC2019, HoC2013a,by="NAME",all=TRUE)

#create a new vector for change between 2019 and 2013 (positive numbers means 
#became less supportive of foreign labor, neg numbers mean the reverse)
HoC2019$forlab.ch1913<-HoC2019$Q4_8-HoC2019$FoRLab2013

#add a vector for 2019 candidates that also ran in 2016-----------------
HoC2016$cand1619<-HoC2016$NAME #generate a new vector for candidates in 16&19

#exclude candidates that weren't candidates in 2019
HoC2016 <- HoC2016 %>%
  mutate(cand1619 = ifelse(cand1619 %in% HoC2019$NAME, cand1619, ""))

#make a subset of 2016 that only includes those that were also candidates in 2019
HoC2016a <- HoC2016 %>%
  filter(cand1619!="")%>%
  group_by(cand1619) %>% 
  summarise(Q3_15=mean(Q3_15)
  )

#rename the columns in the new subset
HoC2016a<-HoC2016a %>% 
  rename(
    NAME=cand1619,
    FoRLab2016=Q3_15
  )

#merge the 2016 forlab variable variable into 2019
HoC2019<-merge(HoC2019, HoC2016a,by="NAME",all=TRUE)

#create a new vector for change between 2019 and 2016 (positive numbers means 
#became less supportive of foreign labor, neg numbers mean the reverse)
HoC2019$forlab.ch1916<-HoC2019$Q4_8-HoC2019$FoRLab2016

#add a new variable for change since last election--------------------------
HoC2019$forlab.change<-HoC2019$forlab.ch1916

HoC2019 <- HoC2019 %>%
  mutate(forlab.change = ifelse(forlab.change %in% !is.na(forlab.ch1916), 
                                forlab.ch1916, forlab.ch1913))

#generate a dummy variable for candidate in 2016
HoC2019$cand2016.dummy<-HoC2019$FoRLab2016
HoC2019 <- HoC2019 %>%
  mutate(cand2016.dummy = ifelse(HoC2019$cand2016.dummy<6, 
                                 1, 0))
HoC2019$cand2016.dummy[is.na(HoC2019$cand2016.dummy)] <- 0 

#GENERATE A DATAFRAME OF PEOPLE THAT CHANGED MORE THAN 1----------------
big.changers <- subset(HoC2019, abs(forlab.change)>1, 
                       select=c(NAME, district.factor.english,party.factor.english,
                                incumb.dummy, female, forlab.change,
                                Q4_8.factor, AGE, FoRLab2013, FoRLab2016,
                                cand2019))

write.csv(big.changers,"bigchangers.csv", row.names = FALSE)