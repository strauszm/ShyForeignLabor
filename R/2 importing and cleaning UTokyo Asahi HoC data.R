#############################
# Michaael Strausz
# Replication files for chapter
# On 2019 HoC election
# Importing and cleaning UTokyo Asahi HoC data
# 5/15/2022 : 5/15/2022
#############################

#install and load necessary libraries-----------------------
library(readr)
library(tidyverse)
library(reshape2)
library (scales)

#import HoC datasets-------------------------
#These commands read in Taniguchi, Masaki. 2009-2021. “The UTokyo-Asahi Survey,” 
#conducted by Masaki Taniguchi of the Graduate Schools for Law and Politics, 
#the University of Tokyo and the Asahi Shimbun. 
#http://www.masaki.j.u-tokyo.ac.jp/utas/utasindex.html.
HoC2019 <- read_csv("Data/HoC2019.csv")
HoC2016 <- read_csv("Data/HoC2016.csv")
HoC2013 <- read_csv("Data/HoC2013.csv")
HoC2010 <- read_csv("Data/HoC2010.csv")

#clean datasets-------------------------------------
#Clean HoC2019--------------------------------------
#remove the 99s (didn't answer) for Q1_1
mean(HoC2019$Q1_1, na.rm = TRUE)
HoC2019$Q1_1[(HoC2019$Q1_1 %in% 1:16) == FALSE] <- NA
#Check the mean: 
mean(HoC2019$Q1_1, na.rm = TRUE)

#remove the 99s from the foreign labor question (Q4_8)
table(HoC2019[,"Q4_8"])
class(HoC2019[,"Q4_8"])
HoC2019[,"Q4_8"][(HoC2019[,"Q4_8"]>5)]<-NA

HoC2019$Q4_8.factor<-factor(x=HoC2019$Q4_8,ordered=TRUE)
table(HoC2019[,"Q4_8.factor"])

class(HoC2019$Q4_8.factor)
levels(HoC2019$Q4_8.factor)<-c("agree", "somewhat agree", 
                               "neither agree nor disagree", 
                               "somewhat disagree", "disagree") 
table(HoC2019[,"Q4_8.factor"])
levels(HoC2019$Q4_8.factor)

#recode the district variable
table(HoC2019$DISTRICT)
class(HoC2019$DISTRICT)
HoC2019$district.factor<-factor(x=HoC2019$DISTRICT,ordered=FALSE)
HoC2019$district.factor.japanese<-HoC2019$district.factor
levels(HoC2019$district.factor.japanese)<-c("北海道","青森","岩手","宮城",
                                            "秋田","山形","福島","茨城",
                                            "栃木","群馬","埼玉","千葉",
                                            "東京","神奈川","新潟","富山",
                                            "石川","福井","山梨","長野",
                                            "岐阜","静岡","愛知","三重",
                                            "滋賀","京都","大阪","兵庫",
                                            "奈良","和歌山","鳥取・島根",
                                            "岡山","広島","山口",
                                            "徳島・高知","香川","愛媛",
                                            "福岡","佐賀","長崎","熊本",
                                            "大分","宮崎","鹿児島","沖縄",
                                            "比例区")
HoC2019$district.factor.english<-HoC2019$district.factor
levels(HoC2019$district.factor.english)<-c("Hokkaido","Aomori","Iwate",
                                           "Miyagi","Akita","Yamagata",
                                           "Fukushima","Ibaraki",
                                           "Tochigi","Gunma","Saitama",
                                           "Chiba",
                                           "Tokyo","Kanagawa","Niigata",
                                           "Toyama",
                                           "Ishikawa","Fukui","Yamanashi",
                                           "Nagano",
                                           "Gifu","Shizuoka","Aichi","Mie",
                                           "Shiga","Kyoto","Osaka","Hyogo",
                                           "Nara","Wagayama",
                                           "Tottori/Shimane",
                                           "Okayama","Hiroshima",
                                           "Yamaguchi",
                                           "Tokushima/Kochi","Kagawa",
                                           "Ehime",
                                           "Fukuoka","Saga","Nagasaki",
                                           "Kumamoto",
                                           "Oita","Miyazaki","Kagoshima",
                                           "Okinawa",
                                           "PR")
levels(HoC2019$district.factor.english)

#recode the PARTY variable
table(HoC2019$PARTY)
class(HoC2019$PARTY)
HoC2019$party.factor<-factor(x=HoC2019$PARTY,ordered=FALSE)
HoC2019$party.factor.japanese<-HoC2019$party.factor
levels(HoC2019$party.factor.japanese)
levels(HoC2019$party.factor.japanese)<-c("自民党","立憲民主党",
                                         "国民民主党","公明党","共産党",
                                         "日本維新の会","社民党",
                                         "れいわ新選組","幸福実現党","諸派",
                                         "無所属候補","NHKから国民を守る党",
                                         "オリーブの木","労働者党",
                                         "安楽死制度を考える会")
HoC2019$party.factor.english<-HoC2019$party.factor
levels(HoC2019$party.factor.english)<-c("LDP","CDP","Kokumin",
                                        "Komeito","JCP",
                                        "Ishin","SDP",
                                        "Reiwa","Happiness",
                                        "Minor parties",
                                        "Unaffiliated","NHK",
                                        "Olive tree","Workers",
                                        "Euthanasia")
is.ordered(HoC2019$party.factor.english)
is.ordered(HoC2019$party.factor.japanese)

#recode the INCUMB variable into a dummy
table(HoC2019$INCUMB)
class(HoC2019$INCUMB)
HoC2019$incumb.dummy<-factor(x=HoC2019$INCUMB,ordered=FALSE)
levels(HoC2019$incumb.dummy)
levels(HoC2019$incumb.dummy)<-c("0","0","1")
HoC2019$incumb.dummy<-as.numeric(as.character(HoC2019$incumb.dummy))
table(HoC2019[,"incumb.dummy"])

#recode the SEX variable into a dummy
table(HoC2019$SEX)
class(HoC2019$SEX)
HoC2019$female<-factor(x=HoC2019$SEX,ordered=FALSE)
levels(HoC2019$female)
levels(HoC2019$female)<-c("0","1")
HoC2019$female<-as.numeric(as.character(HoC2019$female))
table(HoC2019$female)

#recode the RESULT variable into two dummies
#first dummy: RESULT
table(HoC2019$RESULT)
class(HoC2019$RESULT)
HoC2019$RESULT[(HoC2019$RESULT>1)]<-NA
#now 1 is won and 0 is lost

#second dummy: cand2019
HoC2019$cand2019<-HoC2019$RESULT
HoC2019$cand2019[(HoC2019$cand2019<2)]<-1
HoC2019$cand2019[is.na(HoC2019$cand2019)] = 0
table(HoC2019$cand2019)

#generate dummy variable for support for foreign labor
class(HoC2019$Q4_8)
table(HoC2019$Q4_8)
HoC2019$support.forlab.dummy<-HoC2019$Q4_8
table(HoC2019$support.forlab.dummy)
HoC2019$support.forlab.dummy[HoC2019$support.forlab.dummy < 3] <- 1
HoC2019$support.forlab.dummy[HoC2019$support.forlab.dummy > 2] <- 0
HoC2019$support.forlab.100<-HoC2019$support.forlab.dummy*100
table(HoC2019$support.forlab.100)

#now generate a dummy for oppose foreign labor
class(HoC2019$Q4_8)
table(HoC2019$Q4_8)
HoC2019$oppose.forlab.dummy<-HoC2019$Q4_8
table(HoC2019$oppose.forlab.dummy)
HoC2019$oppose.forlab.dummy[HoC2019$oppose.forlab.dummy < 4] <- 0
HoC2019$oppose.forlab.dummy[HoC2019$oppose.forlab.dummy > 3] <- 1
HoC2019$oppose.forlab.100<-HoC2019$oppose.forlab.dummy*100
table(HoC2019$oppose.forlab.100)

#now generate a dummy for neutral
class(HoC2019$Q4_8)
table(HoC2019$Q4_8)
HoC2019$neutral.forlab.dummy<-HoC2019$Q4_8
table(HoC2019$neutral.forlab.dummy)
HoC2019$neutral.forlab.dummy[HoC2019$neutral.forlab.dummy != 3] <- 0 
HoC2019$neutral.forlab.dummy[HoC2019$neutral.forlab.dummy == 3] <- 1
HoC2019$neutral.forlab.100<-HoC2019$neutral.forlab.dummy*100
table(HoC2019$neutral.forlab.100)

party.cand.agg.2019 <- HoC2019 %>%     
  group_by(party.factor.english) %>%    # Group by these variables
  summarise(
    forlab.mean=mean(support.forlab.100, na.rm = TRUE),
    neutral.forlab=mean(neutral.forlab.100,na.rm=TRUE),
    oppose.forlab=mean(oppose.forlab.100, na.rm = TRUE))

#Clean HoC2016--------------------------------------
#remove the 66s & 99s from the foreign labor question (Q3_15)
table(HoC2016$Q3_15)
class(HoC2016$Q3_15)
HoC2016$Q3_15[HoC2016$Q3_15>5]<-NA

HoC2016$Q3_15.factor<-factor(x=HoC2016$Q3_15,ordered=TRUE)
table(HoC2016$Q3_15)

class(HoC2016$Q3_15)
levels(HoC2016$Q3_15)<-c("agree", "somewhat agree", 
                         "neither agree nor disagree", 
                         "somewhat disagree", "disagree") 
table(HoC2016$Q3_15)
levels(HoC2016$Q3_15)

#create a variable for candidate/not candidate
HoC2016$cand2016<-HoC2016$NOELEC
HoC2016$cand2016<-as.factor(HoC2016$cand2016)
table(HoC2016$cand2016)
levels(HoC2016$cand2016)<-c("1","0")
HoC2016$cand2016<-as.numeric(as.character(HoC2016$cand2016)) 


#recode the district variable
table(HoC2016$DISTRICT)
class(HoC2016$DISTRICT)
HoC2016$district.factor<-factor(x=HoC2016$DISTRICT,ordered=FALSE)
HoC2016$district.factor.japanese<-HoC2016$district.factor
levels(HoC2016$district.factor.japanese)<-c("北海道","青森","岩手","宮城",
                                            "秋田","山形","福島","茨城",
                                            "栃木","群馬","埼玉","千葉",
                                            "東京","神奈川","新潟","富山",
                                            "石川","福井","山梨","長野",
                                            "岐阜","静岡","愛知","三重",
                                            "滋賀","京都","大阪","兵庫",
                                            "奈良","和歌山","鳥取","島根",
                                            "岡山","広島","山口","徳島",
                                            "香川","愛媛","高知","福岡",
                                            "佐賀","長崎","熊本","大分",
                                            "宮崎","鹿児島","沖縄",
                                            "鳥取・島根","徳島・高知",
                                            "比例区")

HoC2016$district.factor.english<-HoC2016$district.factor
levels(HoC2016$district.factor.english)<-c("Hokkaido","Aomori","Iwate","Miyagi",
                                           "Akita","Yamagata","Fukushima",
                                           "Ibaraki","Tochigi","Gunma",
                                           "Saitama","Chiba",
                                           "Tokyo","Kanagawa","Niigata",
                                           "Toyama","Ishikawa","Fukui",
                                           "Yamanashi","Nagano","Gifu",
                                           "Shizuoka","Aichi","Mie",
                                           "Shiga","Kyoto","Osaka","Hyogo",
                                           "Nara","Wagayama","Tottori",
                                           "Shimane",
                                           "Okayama","Hiroshima","Yamaguchi",
                                           "Tokushima","Kagawa","Ehime",
                                           "Kochi","Fukuoka","Saga",
                                           "Nagasaki","Kumamoto","Oita",
                                           "Miyazaki","Kagoshima","Okinawa",
                                           "Tottori/Shimane","Tokushima/Kochi",
                                           "PR")
HoC2016$district.factor.2019levels<-HoC2016$district.factor
levels(HoC2016$district.factor.2019levels)<-c("Hokkaido","Aomori","Iwate","Miyagi",
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
                                              "Tottori/Shimane","Tokushima/Kochi",
                                              "PR")

#recode the PARTY variable
table(HoC2016$PARTY)
class(HoC2019$PARTY)
HoC2016$party.factor<-factor(x=HoC2016$PARTY,ordered=FALSE)

HoC2016$party.factor.japanese<-HoC2016$party.factor
levels(HoC2016$party.factor.japanese)
levels(HoC2016$party.factor.japanese)<-c("自民党","民進党","公明党","共産党",
                                         "おおさか維新の会","社民党","生活の党と山本太郎となかまたち",
                                         "日本のこころを大切にする党","日本を元気にする会","新党改革","幸福実現党",
                                         "国民怒りの声", "諸派","無所属候補のうち、野党統一候補",
                                         "その他の無所属候補（非改選議員については、無所属議員）")
HoC2016$party.factor.english<-HoC2016$party.factor
levels(HoC2016$party.factor.english)<-c("LDP","DP","Komeito","JCP",
                                        "Osaka Ishin","SDP","Seikatsu",
                                        "Kokoro","Genki","NRP",
                                        "Happiness","Voice","Minor Parties",
                                        "Unaffiliated (unified opp. cand.)",
                                        "Unaffiliated")
is.ordered(HoC2016$party.factor.english)
is.ordered(HoC2016$party.factor.japanese)

#recode the INCUMB variable into a dummy
table(HoC2016$INCUMB)
class(HoC2016$INCUMB)
HoC2016$incumb.dummy<-factor(x=HoC2016$INCUMB,ordered=TRUE)
levels(HoC2016$incumb.dummy)
levels(HoC2016$incumb.dummy)<-c("0","0","1")
HoC2016$incumb.dummy<-as.numeric(as.character(HoC2016$incumb.dummy))
table(HoC2016[,"incumb.dummy"])

#recode the SEX variable into a dummy
table(HoC2016$SEX)
class(HoC2016$SEX)
HoC2016$female<-factor(x=HoC2016$SEX,ordered=FALSE)
levels(HoC2016$female)
levels(HoC2016$female)<-c("0","1")
HoC2016$female<-as.numeric(as.character(HoC2016$female))
table(HoC2016$female)

#recode the RESULT variable into two dummies
#first dummy: RESULT
table(HoC2016$RESULT)
class(HoC2016$RESULT)
HoC2016$RESULT[(HoC2016$RESULT>1)]<-NA
#now 1 is won and 0 is lost

#generate dummy variable for support for foreign labor
class(HoC2016$Q3_15)
table(HoC2016$Q3_15)
HoC2016$support.forlab.dummy<-HoC2016$Q3_15
table(HoC2016$support.forlab.dummy)
HoC2016$support.forlab.dummy[HoC2016$support.forlab.dummy < 3] <- 1
HoC2016$support.forlab.dummy[HoC2016$support.forlab.dummy > 2] <- 0
HoC2016$support.forlab.100<-HoC2016$support.forlab.dummy*100
table(HoC2016$support.forlab.100)

#now generate a dummy for oppose foreign labor
class(HoC2016$Q3_15)
table(HoC2016$Q3_15)
HoC2016$oppose.forlab.dummy<-HoC2016$Q3_15
table(HoC2016$oppose.forlab.dummy)
HoC2016$oppose.forlab.dummy[HoC2016$oppose.forlab.dummy < 4] <- 0
HoC2016$oppose.forlab.dummy[HoC2016$oppose.forlab.dummy > 3] <- 1
HoC2016$oppose.forlab.100<-HoC2016$oppose.forlab.dummy*100
table(HoC2016$oppose.forlab.100)

#now generate a dummy for neutral
class(HoC2016$Q3_15)
table(HoC2016$Q3_15)
HoC2016$neutral.forlab.dummy<-HoC2016$Q3_15
table(HoC2016$neutral.forlab.dummy)
HoC2016$neutral.forlab.dummy[HoC2016$neutral.forlab.dummy != 3] <- 0 
HoC2016$neutral.forlab.dummy[HoC2016$neutral.forlab.dummy == 3] <- 1
HoC2016$neutral.forlab.100<-HoC2016$neutral.forlab.dummy*100
table(HoC2016$neutral.forlab.100)

#add a variable for those that ran in 2019
HoC2016$cand1619<-HoC2016$NAME #generate a new vector for candidates in 16&19

#exclude candidates that weren't candidates in 2019
HoC2016 <- HoC2016 %>%
  mutate(cand1619 = ifelse(cand1619 %in% HoC2019$NAMEnoEQ, cand1619, ""))

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

#rename the columns in HoC2019
HoC2019<-HoC2019 %>% 
  rename(
    NAME=NAMEnoEQ,
  )

#merge the 2016 forlab variable variable into 2019
HoC2019<-merge(HoC2019, HoC2016a,by="NAME",all=TRUE)

#create a new vector for change between 2019 and 2016 (positive numbers means 
#became more supportive of foreign labor, neg numbers mean the reverse)
HoC2019$forlab.ch1916<-HoC2019$Q4_8-HoC2019$FoRLab2016

party.cand.agg.2016 <- HoC2016 %>%     
  group_by(party.factor.english) %>%    # Group by these variables
  summarise(
    forlab.mean=mean(support.forlab.100, na.rm = TRUE),
    neutral.forlab=mean(neutral.forlab.100,na.rm=TRUE),
    oppose.forlab=mean(oppose.forlab.100, na.rm = TRUE))

#Clean HoC2013--------------------------------------
#remove the 66s & 99s from the foreign labor question (Q4_16)
table(HoC2013$Q4_16)
class(HoC2013$Q4_16)
HoC2013$Q4_16[HoC2013$Q4_16>5]<-NA

HoC2013$Q4_16.factor<-factor(x=HoC2013$Q4_16,ordered=TRUE)
table(HoC2013$Q4_16)

class(HoC2013$Q4_16.factor)
levels(HoC2013$Q4_16.factor)<-c("agree", "somewhat agree", 
                                "neither agree nor disagree", 
                                "somewhat disagree", "disagree") 
table(HoC2013$Q4_16.factor)
levels(HoC2013$Q4_16.factor)

#create a variable for candidate/not candidate
HoC2013$cand2013<-HoC2013$NOELEC
HoC2013$cand2013<-as.factor(HoC2013$cand2013)
table(HoC2013$cand2013)
levels(HoC2013$cand2013)<-c("1","0")
HoC2013$cand2013<-as.numeric(as.character(HoC2013$cand2013)) 

#recode the district variable
table(HoC2013$DISTRICT)
class(HoC2013$DISTRICT)
HoC2013$district.factor<-factor(x=HoC2013$DISTRICT,ordered=FALSE)
HoC2013$district.factor.japanese<-HoC2013$district.factor
levels(HoC2013$district.factor.japanese)<-c("北海道","青森","岩手","宮城",
                                            "秋田","山形","福島","茨城",
                                            "栃木","群馬","埼玉","千葉",
                                            "東京","神奈川","新潟","富山",
                                            "石川","福井","山梨","長野",
                                            "岐阜","静岡","愛知","三重",
                                            "滋賀","京都","大阪","兵庫",
                                            "奈良","和歌山","鳥取","島根",
                                            "岡山","広島","山口","徳島",
                                            "香川","愛媛","高知","福岡",
                                            "佐賀","長崎","熊本","大分",
                                            "宮崎","鹿児島","沖縄","比例区")

HoC2013$district.factor.english<-HoC2013$district.factor
levels(HoC2013$district.factor.english)<-c("Hokkaido","Aomori","Iwate","Miyagi",
                                           "Akita","Yamagata","Fukushima",
                                           "Ibaraki","Tochigi","Gunma",
                                           "Saitama","Chiba",
                                           "Tokyo","Kanagawa","Niigata",
                                           "Toyama","Ishikawa","Fukui",
                                           "Yamanashi","Nagano","Gifu",
                                           "Shizuoka","Aichi","Mie",
                                           "Shiga","Kyoto","Osaka","Hyogo",
                                           "Nara","Wagayama","Tottori",
                                           "Shimane",
                                           "Okayama","Hiroshima","Yamaguchi",
                                           "Tokushima","Kagawa","Ehime",
                                           "Kochi","Fukuoka","Saga",
                                           "Nagasaki","Kumamoto","Oita",
                                           "Miyazaki","Kagoshima",
                                           "Okinawa","PR")

HoC2013$district.factor.2019levels<-HoC2013$district.factor
levels(HoC2013$district.factor.2019levels)<-c("Hokkaido","Aomori","Iwate","Miyagi",
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

#recode the PARTY variable
table(HoC2013$PARTY)
class(HoC2013$PARTY)
HoC2013$party.factor<-factor(x=HoC2013$PARTY,ordered=FALSE)

HoC2013$party.factor.japanese<-HoC2013$party.factor
levels(HoC2013$party.factor.japanese)
levels(HoC2013$party.factor.japanese)<-c("自民党","民主党","日本維新の会",
                                         "公明党","みんなの党","共産党",
                                         "生活の党","社民党","みどりの風",
                                         "新党大地","新党改革","幸福実現党",
                                         "緑の党","維新政党・新風","減税日本",
                                         "諸派","無所属")

HoC2013$party.factor.english<-HoC2013$party.factor
levels(HoC2013$party.factor.english)<-c("LDP","DPJ","Ishin",
                                        "Komeito","YP","CPJ",
                                        "Seikatsu","SDP","Green Wind",
                                        "Daichi","NRP","Happiness",
                                        "Greens","New Wind","Tax Cuts",
                                        "Minor parties","Unaffiliated")

is.ordered(HoC2013$party.factor.english)
is.ordered(HoC2013$party.factor.japanese)

#recode the INCUMB variable into a dummy
table(HoC2013$INCUMB)
class(HoC2013$INCUMB)
HoC2013$incumb.dummy<-factor(x=HoC2013$INCUMB,ordered=TRUE)
levels(HoC2013$incumb.dummy)
levels(HoC2013$incumb.dummy)<-c("0","0","0","1","1")
HoC2013$incumb.dummy<-as.numeric(as.character(HoC2013$incumb.dummy))
table(HoC2013[,"incumb.dummy"])

#recode the SEX variable into a dummy
table(HoC2013$SEX)
class(HoC2013$SEX)
HoC2013$female<-factor(x=HoC2013$SEX,ordered=FALSE)
levels(HoC2013$female)
levels(HoC2013$female)<-c("0","1")
HoC2013$female<-as.numeric(as.character(HoC2013$female))
table(HoC2013$female)

#recode the RESULT variable into a dummy
#first dummy: RESULT
table(HoC2013$RESULT)
class(HoC2013$RESULT)
HoC2013$RESULT[(HoC2013$RESULT>1)]<-NA
#now 1 is won and 0 is lost

#generate dummy variable for support for foreign labor
class(HoC2013$Q4_16)
table(HoC2013$Q4_16)
HoC2013$support.forlab.dummy<-HoC2013$Q4_16
table(HoC2013$support.forlab.dummy)
HoC2013$support.forlab.dummy[HoC2013$support.forlab.dummy < 3] <- 1
HoC2013$support.forlab.dummy[HoC2013$support.forlab.dummy > 2] <- 0
HoC2013$support.forlab.100<-HoC2013$support.forlab.dummy*100
table(HoC2013$support.forlab.100)

#now generate a dummy for oppose foreign labor
class(HoC2013$Q4_16)
table(HoC2013$Q4_16)
HoC2013$oppose.forlab.dummy<-HoC2013$Q4_16
table(HoC2013$oppose.forlab.dummy)
HoC2013$oppose.forlab.dummy[HoC2013$oppose.forlab.dummy < 4] <- 0
HoC2013$oppose.forlab.dummy[HoC2013$oppose.forlab.dummy > 3] <- 1
HoC2013$oppose.forlab.100<-HoC2013$oppose.forlab.dummy*100
table(HoC2013$oppose.forlab.100)

#now generate a dummy for neutral
class(HoC2013$Q4_16)
table(HoC2013$Q4_16)
HoC2013$neutral.forlab.dummy<-HoC2013$Q4_16
table(HoC2013$neutral.forlab.dummy)
HoC2013$neutral.forlab.dummy[HoC2013$neutral.forlab.dummy != 3] <- 0 
HoC2013$neutral.forlab.dummy[HoC2013$neutral.forlab.dummy == 3] <- 1
HoC2013$neutral.forlab.100<-HoC2013$neutral.forlab.dummy*100
table(HoC2013$neutral.forlab.100)

party.cand.agg.2013 <- HoC2013 %>%     
  group_by(party.factor.english) %>%    # Group by these variables
  summarise(
    forlab.mean=mean(support.forlab.100, na.rm = TRUE),
    neutral.forlab=mean(neutral.forlab.100,na.rm=TRUE),
    oppose.forlab=mean(oppose.forlab.100, na.rm = TRUE))

#Clean HoC2010--------------------------------------
#remove the 66s & 99s from the foreign labor question (Q7_21)
table(HoC2010$Q7_21)
class(HoC2010$Q7_21)
any(is.na(HoC2010$Q7_21))
#NAs were already included in this case because it came from SPSS

HoC2010$Q7_21.factor<-factor(x=HoC2010$Q7_21,ordered=TRUE)
table(HoC2010$Q7_21)

class(HoC2010$Q7_21.factor)
levels(HoC2010$Q7_21.factor)<-c("agree", "somewhat agree", 
                                "neither agree nor disagree", 
                                "somewhat disagree", "disagree") 
table(HoC2010$Q7_21.factor)
levels(HoC2010$Q7_21.factor)

#create a variable for candidate/not candidate
HoC2010$cand2010<-HoC2010$NOELEC
HoC2010$cand2010<-as.factor(HoC2010$cand2010)
table(HoC2010$cand2010)
levels(HoC2010$cand2010)<-c("1","0")
HoC2010$cand2010<-as.numeric(as.character(HoC2010$cand2010)) 

#recode the district variable
table(HoC2010$DISTRICT)
class(HoC2010$DISTRICT)
HoC2010$district.factor<-factor(x=HoC2010$DISTRICT,ordered=FALSE)
HoC2010$district.factor.japanese<-HoC2010$district.factor
levels(HoC2010$district.factor.japanese)<-c("北海道","青森","岩手","宮城",
                                            "秋田","山形","福島","茨城",
                                            "栃木","群馬","埼玉","千葉",
                                            "東京","神奈川","新潟","富山",
                                            "石川","福井","山梨","長野",
                                            "岐阜","静岡","愛知","三重",
                                            "滋賀","京都","大阪","兵庫",
                                            "奈良","和歌山","鳥取","島根",
                                            "岡山","広島","山口","徳島",
                                            "香川","愛媛","高知","福岡",
                                            "佐賀","長崎","熊本","大分",
                                            "宮崎","鹿児島","沖縄","比例区")

HoC2010$district.factor.english<-HoC2010$district.factor
levels(HoC2010$district.factor.english)<-c("Hokkaido","Aomori","Iwate","Miyagi",
                                           "Akita","Yamagata","Fukushima",
                                           "Ibaraki","Tochigi","Gunma",
                                           "Saitama","Chiba",
                                           "Tokyo","Kanagawa","Niigata",
                                           "Toyama","Ishikawa","Fukui",
                                           "Yamanashi","Nagano","Gifu",
                                           "Shizuoka","Aichi","Mie",
                                           "Shiga","Kyoto","Osaka","Hyogo",
                                           "Nara","Wagayama","Tottori",
                                           "Shimane",
                                           "Okayama","Hiroshima","Yamaguchi",
                                           "Tokushima","Kagawa","Ehime",
                                           "Kochi","Fukuoka","Saga",
                                           "Nagasaki","Kumamoto","Oita",
                                           "Miyazaki","Kagoshima",
                                           "Okinawa","PR")

HoC2010$district.factor.2019levels<-HoC2010$district.factor
levels(HoC2010$district.factor.2019levels)<-c("Hokkaido","Aomori","Iwate","Miyagi",
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

#recode the PARTY variable
table(HoC2010$PARTY)
class(HoC2010$PARTY)
HoC2010$party.factor<-factor(x=HoC2010$PARTY,ordered=FALSE)

HoC2010$party.factor.japanese<-HoC2010$party.factor
levels(HoC2010$party.factor.japanese)
levels(HoC2010$party.factor.japanese)<-c("民主党","自民党","公明党",
                                         "共産党","社民党","国民新党",
                                         "みんなの党","新党改革",
                                         "たちあがれ日本","幸福実現党",
                                         "日本創新党","諸派","無所属")

HoC2010$party.factor.english<-HoC2010$party.factor
levels(HoC2010$party.factor.english)<-c("DPJ","LDP","Komeito",
                                        "JCP","SDP","Kokumin",
                                        "YP","NRP",
                                        "Sunrise","Happiness",
                                        "Spirit","Minor parties",
                                        "Unaffiliated")

is.ordered(HoC2010$party.factor.english)
is.ordered(HoC2010$party.factor.japanese)

#recode the INCUMB variable into a dummy
table(HoC2010$INCUMB)
class(HoC2010$INCUMB)
HoC2010$incumb.dummy<-factor(x=HoC2010$INCUMB,ordered=TRUE)
levels(HoC2010$incumb.dummy)
levels(HoC2010$incumb.dummy)<-c("0","0","0","1")
HoC2010$incumb.dummy<-as.numeric(as.character(HoC2010$incumb.dummy))
table(HoC2010[,"incumb.dummy"])

#recode the SEX variable into a dummy
table(HoC2010$SEX)
class(HoC2010$SEX)
HoC2010$female<-factor(x=HoC2010$SEX,ordered=FALSE)
levels(HoC2010$female)
levels(HoC2010$female)<-c("0","1")
HoC2010$female<-as.numeric(as.character(HoC2010$female))
table(HoC2010$female)

#recode the RESULT variable into a dummy
#first dummy: RESULT
table(HoC2010$RESULT)
class(HoC2010$RESULT)
HoC2010$RESULT[(HoC2010$RESULT>1)]<-NA
#now 1 is won and 0 is lost

#generate dummy variable for support for foreign labor
class(HoC2010$Q7_21)
table(HoC2010$Q7_21)
HoC2010$support.forlab.dummy<-HoC2010$Q7_21
table(HoC2010$support.forlab.dummy)
HoC2010$support.forlab.dummy[HoC2010$support.forlab.dummy < 3] <- 1
HoC2010$support.forlab.dummy[HoC2010$support.forlab.dummy > 2] <- 0
HoC2010$support.forlab.100<-HoC2010$support.forlab.dummy*100
table(HoC2010$support.forlab.100)

#now generate a dummy for oppose foreign labor
class(HoC2010$Q7_21)
table(HoC2010$Q7_21)
HoC2010$oppose.forlab.dummy<-HoC2010$Q7_21
table(HoC2010$oppose.forlab.dummy)
HoC2010$oppose.forlab.dummy[HoC2010$oppose.forlab.dummy < 4] <- 0
HoC2010$oppose.forlab.dummy[HoC2010$oppose.forlab.dummy > 3] <- 1
HoC2010$oppose.forlab.100<-HoC2010$oppose.forlab.dummy*100
table(HoC2010$oppose.forlab.100)

#now generate a dummy for neutral
class(HoC2010$Q7_21)
table(HoC2010$Q7_21)
HoC2010$neutral.forlab.dummy<-HoC2010$Q7_21
table(HoC2010$neutral.forlab.dummy)
HoC2010$neutral.forlab.dummy[HoC2010$neutral.forlab.dummy != 3] <- 0 
HoC2010$neutral.forlab.dummy[HoC2010$neutral.forlab.dummy == 3] <- 1
HoC2010$neutral.forlab.100<-HoC2010$neutral.forlab.dummy*100
table(HoC2010$neutral.forlab.100)

party.cand.agg.2010 <- HoC2010 %>%     
  group_by(party.factor.english) %>%    # Group by these variables
  summarise(
    forlab.mean=mean(support.forlab.100, na.rm = TRUE),
    neutral.forlab=mean(neutral.forlab.100,na.rm=TRUE),
    oppose.forlab=mean(oppose.forlab.100, na.rm = TRUE))

