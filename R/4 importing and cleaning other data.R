#############################
# Michaael Strausz
# Replication files for chapter
# On 2019 HoC election
# Importing other data
# 5/17/2022 : 5/17/2022
#############################

#install and load necessary libraries-----------------------
library(readr)

#notes on sources of data-------------------------------------
#Prefectural population data was from 2020 Japan statistical yearbook, 
#https://www.stat.go.jp/data/nihon/index1.html

#2019 foreigners prefecture data was from the MoJ statistics. 
#在留外国人統計（旧登録外国人統計）統計表. I used 6/2019 data. 
#http://www.moj.go.jp/isa/policies/statistics/toukei_ichiran_touroku.html

#Job Seeker’s Ratio data from Ministry of Health, Labor, and Welfare (Japan). 2021. 
#“Ippan Shokugyō Shōkai Jōkyō [An Introduction to the State of the General Labor 
#Market]. http://www.mhlw.go.jp/toukei/list/114-1.html. I used the data from 
#June 2019 because the election was in July

#import other dataframes-------------------------
prefpop <- read_csv("Data/prefpop.csv")
jsratio2019 <- read_csv("Data/jsratio2019.csv")

#clean job seeker's ratio data--------------------
jsratio2019$prefec.factor<-as.factor(jsratio2019$pref.numb)
jsratio2019$prefec.factor.2019levels<-jsratio2019$prefec.factor
levels(jsratio2019$prefec.factor.2019levels)<-c("Hokkaido","Aomori","Iwate",
                                                "Miyagi",
                                                "Akita","Yamagata","Fukushima",
                                                "Ibaraki","Tochigi","Gunma",
                                                "Saitama","Chiba",
                                                "Tokyo","Kanagawa","Niigata",
                                                "Toyama","Ishikawa","Fukui",
                                                "Yamanashi","Nagano","Gifu",
                                                "Shizuoka","Aichi","Mie",
                                                "Shiga","Kyoto","Osaka","Hyogo",
                                                "Nara","Wagayama","Tottori/Shimane",
                                                "Tottori/Shimane",
                                                "Okayama","Hiroshima","Yamaguchi",
                                                "Tokushima/Kochi","Kagawa","Ehime",
                                                "Tokushima/Kochi","Fukuoka","Saga",
                                                "Nagasaki","Kumamoto","Oita",
                                                "Miyazaki","Kagoshima","Okinawa",
                                                "PR")


#filter the JS ratio data so that I am dealing with the 2019 prefectures
jsratio2019<-jsratio2019 %>% # From the js ratio dataframe
  group_by(prefec.factor.2019levels) %>% 
  summarise( 
    jsratio2019=mean(js.ratio.2019)
  ) 

#clean prefecture population data------------------------
#recode the prefecture variable
prefpop$prefec.factor<-as.factor(prefpop$pref)
prefpop$prefec.factor.2019levels<-prefpop$prefec.factor
levels(prefpop$prefec.factor.2019levels)<-c("Hokkaido","Aomori","Iwate",
                                            "Miyagi",
                                            "Akita","Yamagata","Fukushima",
                                            "Ibaraki","Tochigi","Gunma",
                                            "Saitama","Chiba",
                                            "Tokyo","Kanagawa","Niigata",
                                            "Toyama","Ishikawa","Fukui",
                                            "Yamanashi","Nagano","Gifu",
                                            "Shizuoka","Aichi","Mie",
                                            "Shiga","Kyoto","Osaka","Hyogo",
                                            "Nara","Wagayama","Tottori/Shimane",
                                            "Tottori/Shimane",
                                            "Okayama","Hiroshima","Yamaguchi",
                                            "Tokushima/Kochi","Kagawa","Ehime",
                                            "Tokushima/Kochi","Fukuoka","Saga",
                                            "Nagasaki","Kumamoto","Oita",
                                            "Miyazaki","Kagoshima","Okinawa",
                                            "PR")
#build my new variables
prefpop$prefforpct2019<-100*prefpop$forpop2019/prefpop$prefpop2018est
prefpop$prefforpct2010<-100*prefpop$forpop2010/prefpop$prefpop2010
prefpop$forpopchange.201910<-prefpop$prefforpct2019-prefpop$prefforpct2010
prefpop$popchange.201810<-prefpop$prefpop2018est-prefpop$prefpop2010

#make a dataframe with the variables I want and the 2019 prefectures
prefpop<-prefpop %>% # From the prefpop dataframe
  group_by(prefec.factor.2019levels) %>% 
  summarise( 
    prefforpct2019=mean(prefforpct2019),
    prefforpct2010=mean(prefforpct2010),
    forpopchange.201910=mean(forpopchange.201910),
    popchange.201810=mean(popchange.201810)
  ) 



