#############################
# Michaael Strausz
# Replication files for chapter
# On 2019 HoC election
# Importing and cleaning UTokyo Asahi HoR data
# 5/15/2022 : 5/15/2022
#############################

#install and load necessary libraries-----------------------
library(readr)
library(tidyverse)
library(reshape2)
library (scales)
library(readxl)

#import HoC datasets-------------------------
#These commands read in Taniguchi, Masaki. 2009-2021. “The UTokyo-Asahi Survey,” 
#conducted by Masaki Taniguchi of the Graduate Schools for Law and Politics, 
#the University of Tokyo and the Asahi Shimbun. 
#http://www.masaki.j.u-tokyo.ac.jp/utas/utasindex.html.
HoR2021 <- read_excel("Data/HoR2021.xlsx") #I generated this dataframe by looking 
                                          #at Asahi's online candidate search feature
                                          #before and after this election
HoR2017 <- read_csv("Data/HoR2017.csv")
HoR2014 <- read_csv("Data/HoR2014.csv")
HoR2012 <- read_csv("Data/HoR2012.csv")
HoR2009 <- read_csv("Data/HoR2009.csv")

#Import public opinion data-----------------------------------
#this data also comes from Taniguchi, Masaki. 2009-2021. “The UTokyo-Asahi Survey,” 
#conducted by Masaki Taniguchi of the Graduate Schools for Law and Politics, 
#the University of Tokyo and the Asahi Shimbun. 
#http://www.masaki.j.u-tokyo.ac.jp/utas/utasindex.html.
PubOp2017 <- read_csv("Data/PubOp2017.csv")
PubOp2014 <- read_csv("Data/PubOp2014.csv")


#clean datasets-------------------------------------
#Clean HoR2021-------------------------
#remove NAs
table(HoR2021$forlab)
HoR2021$forlab[HoR2021$forlab==9]<-NA
table(HoR2021$forlab)

#add in some variables to make this work
HoR2021$party.factor.english<-HoR2021$party
HoR2021$party.factor.english<-as.factor(HoR2021$party.factor.english)

#generate dummy variable for support for foreign labor-------------
class(HoR2021$forlab)
table(HoR2021$forlab)
HoR2021$support.forlab.dummy<-HoR2021$forlab
table(HoR2021$support.forlab.dummy)
HoR2021$support.forlab.dummy[HoR2021$support.forlab.dummy < 3] <- 1
HoR2021$support.forlab.dummy[HoR2021$support.forlab.dummy > 2] <- 0
HoR2021$support.forlab.100<-HoR2021$support.forlab.dummy*100
table(HoR2021$support.forlab.100)

#now generate a dummy for oppose foreign labor
class(HoR2021$forlab)
table(HoR2021$forlab)
HoR2021$oppose.forlab.dummy<-HoR2021$forlab
table(HoR2021$oppose.forlab.dummy)
HoR2021$oppose.forlab.dummy[HoR2021$oppose.forlab.dummy < 4] <- 0
HoR2021$oppose.forlab.dummy[HoR2021$oppose.forlab.dummy > 3] <- 1
HoR2021$oppose.forlab.100<-HoR2021$oppose.forlab.dummy*100
table(HoR2021$oppose.forlab.100)

#now generate a dummy for neutral
class(HoR2021$forlab)
table(HoR2021$forlab)
HoR2021$neutral.forlab.dummy<-HoR2021$forlab
table(HoR2021$neutral.forlab.dummy)
HoR2021$neutral.forlab.dummy[HoR2021$neutral.forlab.dummy != 3] <- 0 
HoR2021$neutral.forlab.dummy[HoR2021$neutral.forlab.dummy == 3] <- 1
HoR2021$neutral.forlab.100<-HoR2021$neutral.forlab.dummy*100
table(HoR2021$neutral.forlab.100)

#make party.cand.agg dataframe
party.cand.agg.2021 <- HoR2021 %>%     
  group_by(party.factor.english) %>%    # Group by these variables
  summarise(
    forlab.mean=mean(support.forlab.100, na.rm = TRUE),
    neutral.forlab=mean(neutral.forlab.100,na.rm=TRUE),
    oppose.forlab=mean(oppose.forlab.100, na.rm = TRUE))
View(party.cand.agg.2021)

#make my parties bar graph
party.cand.agg.2021.melt<-melt(party.cand.agg.2021, id.vars = "party.factor.english")
ggplot(party.cand.agg.2021.melt, aes(x = party.factor.english, y = value, 
                                     fill = variable)) + 
  geom_bar(stat = "identity") +
  scale_fill_discrete(labels = c("agree","neither","disagree"))+
  coord_flip()+
  labs(x = NULL, y=NULL, fill="Views on increasing\n foreign labor")

#Clean HoR2017--------------------------------------
#remove the 99s from the foreign labor question (Q4_12)
table(HoR2017$Q4_12)
class(HoR2017$Q4_12)
HoR2017$Q4_12[HoR2017$Q4_12>5]<-NA
any(is.na(HoR2017$Q4_12))

HoR2017$Q4_12.factor<-factor(x=HoR2017$Q4_12,ordered=TRUE)
table(HoR2017$Q4_12)

class(HoR2017$Q4_12.factor)
levels(HoR2017$Q4_12.factor)<-c("agree", "somewhat agree", 
                                "neither agree nor disagree", 
                                "somewhat disagree", "disagree") 
table(HoR2017$Q4_12.factor)
levels(HoR2017$Q4_12.factor)



#recode the PREFEC variable
table(HoR2017$PREFEC)
class(HoR2017$PREFEC)
HoR2017$prefec.factor<-factor(x=HoR2017$PREFEC,ordered=FALSE)
HoR2017$prefec.factor.japanese<-HoR2017$prefec.factor
levels(HoR2017$prefec.factor.japanese)<-c("北海道","青森","岩手","宮城",
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

HoR2017$prefec.factor.english<-HoR2017$prefec.factor
levels(HoR2017$prefec.factor.english)<-c("Hokkaido","Aomori","Iwate","Miyagi",
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

HoR2017$prefec.factor.2019levels<-HoR2017$prefec.factor
levels(HoR2017$prefec.factor.2019levels)<-c("Hokkaido","Aomori","Iwate","Miyagi",
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
#recode the PR Block variable
table(HoR2017$PRBLOCK)
class(HoR2017$PRBLOCK)
HoR2017$prblock.factor<-factor(x=HoR2017$PRBLOCK,ordered=FALSE)
HoR2017$prblock.factor.japanese<-HoR2017$prblock.factor
levels(HoR2017$prblock.factor.japanese)<-c("北海道","東北","北関東","南関東",
                                           "東京","北陸・信越","東海","近畿",
                                           "中国","四国","九州","非該当")
HoR2017$prblock.factor.english<-HoR2017$prblock.factor
levels(HoR2017$prblock.factor.english)<-c("Hokkaido","Tohoku","Kita Kanto",
                                          "Minami Kanto","Tokyo",
                                          "Hokuriku/Shinetsu","Tokai",
                                          "Kinki","Chugoku","Shikoku",
                                          "Kyushu","Not PR")

#recode the PARTY variable
table(HoR2017$PARTY)
class(HoR2017$PARTY)
HoR2017$party.factor<-factor(x=HoR2017$PARTY,ordered=FALSE)

HoR2017$party.factor.japanese<-HoR2017$party.factor
levels(HoR2017$party.factor.japanese)
levels(HoR2017$party.factor.japanese)<-c("自民党","公明党","共産党",
                                         "日本維新の会","社民党","日本のこころ","希望の党","立憲民主党","幸福実現党",
                                         "諸派","無所属候補")


HoR2017$party.factor.english<-HoR2017$party.factor
levels(HoR2017$party.factor.english)<-c("LDP","Komeito","CPJ",
                                        "Ishin","SDP","Kokoro","Hope","CDP",
                                        "Happiness","Minor parties",
                                        "Unaffiliated")
is.ordered(HoR2017$party.factor.english)
is.ordered(HoR2017$party.factor.japanese)

#recode the INCUMB variable into a dummy
table(HoR2017$INCUMB)
class(HoR2017$INCUMB)
HoR2017$incumb.dummy<-factor(x=HoR2017$INCUMB,ordered=TRUE)
levels(HoR2017$incumb.dummy)
levels(HoR2017$incumb.dummy)<-c("0","0","1")
HoR2017$incumb.dummy<-as.numeric(as.character(HoR2017$incumb.dummy))
table(HoR2017[,"incumb.dummy"])

#recode the SEX variable into a dummy
table(HoR2017$SEX)
class(HoR2017$SEX)
HoR2017$female<-factor(x=HoR2017$SEX,ordered=FALSE)
levels(HoR2017$female)
levels(HoR2017$female)<-c("0","1")
HoR2017$female<-as.numeric(as.character(HoR2017$female))
table(HoR2017$female)

#recode the RESULT variable
table(HoR2017$RESULT)
class(HoR2017$RESULT)
HoR2017$result.factor<-factor(x=HoR2017$RESULT,ordered=FALSE)
levels(HoR2017$result.factor)<-c("lost","won SMD", "zombie", "won PR only")
levels(HoR2017$result.factor)

#generate dummy variable for support for foreign labor
class(HoR2017$Q4_12)
table(HoR2017$Q4_12)
HoR2017$support.forlab.dummy<-HoR2017$Q4_12
table(HoR2017$support.forlab.dummy)
HoR2017$support.forlab.dummy[HoR2017$support.forlab.dummy < 3] <- 1
HoR2017$support.forlab.dummy[HoR2017$support.forlab.dummy > 2] <- 0
HoR2017$support.forlab.100<-HoR2017$support.forlab.dummy*100
table(HoR2017$support.forlab.100)

#now generate a dummy for oppose foreign labor
class(HoR2017$Q4_12)
table(HoR2017$Q4_12)
HoR2017$oppose.forlab.dummy<-HoR2017$Q4_12
table(HoR2017$oppose.forlab.dummy)
HoR2017$oppose.forlab.dummy[HoR2017$oppose.forlab.dummy < 4] <- 0
HoR2017$oppose.forlab.dummy[HoR2017$oppose.forlab.dummy > 3] <- 1
HoR2017$oppose.forlab.100<-HoR2017$oppose.forlab.dummy*100
table(HoR2017$oppose.forlab.100)

#now generate a dummy for neutral
class(HoR2017$Q4_12)
table(HoR2017$Q4_12)
HoR2017$neutral.forlab.dummy<-HoR2017$Q4_12
table(HoR2017$neutral.forlab.dummy)
HoR2017$neutral.forlab.dummy[HoR2017$neutral.forlab.dummy != 3] <- 0 
HoR2017$neutral.forlab.dummy[HoR2017$neutral.forlab.dummy == 3] <- 1
HoR2017$neutral.forlab.100<-HoR2017$neutral.forlab.dummy*100
table(HoR2017$neutral.forlab.100)

party.cand.agg.2017 <- HoR2017 %>%     
  group_by(party.factor.english) %>%    # Group by these variables
  summarise(
    forlab.mean=mean(support.forlab.100, na.rm = TRUE),
    neutral.forlab=mean(neutral.forlab.100,na.rm=TRUE),
    oppose.forlab=mean(oppose.forlab.100, na.rm = TRUE))

#Clean HoR2014--------------------------------------

#remove the 99s from the foreign labor question (Q6_13)
table(HoR2014$Q6_13)
class(HoR2014$Q6_13)
HoR2014$Q6_13[HoR2014$Q6_13>5]<-NA
any(is.na(HoR2014$Q6_13))

HoR2014$Q6_13.factor<-factor(x=HoR2014$Q6_13,ordered=TRUE)
table(HoR2014$Q6_13)

class(HoR2014$Q6_13.factor)
levels(HoR2014$Q6_13.factor)<-c("agree", "somewhat agree", 
                                "neither agree nor disagree", 
                                "somewhat disagree", "disagree") 
table(HoR2014$Q6_13.factor)
levels(HoR2014$Q6_13.factor)


#recode the PREFEC variable
table(HoR2014$PREFEC)
class(HoR2014$PREFEC)
HoR2014$prefec.factor<-factor(x=HoR2014$PREFEC,ordered=FALSE)
HoR2014$prefec.factor.japanese<-HoR2014$prefec.factor
levels(HoR2014$prefec.factor.japanese)<-c("北海道","青森","岩手","宮城",
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

HoR2014$prefec.factor.english<-HoR2014$prefec.factor
levels(HoR2014$prefec.factor.english)<-c("Hokkaido","Aomori","Iwate","Miyagi",
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

HoR2014$prefec.factor.2019levels<-HoR2014$prefec.factor
levels(HoR2014$prefec.factor.2019levels)<-c("Hokkaido","Aomori","Iwate","Miyagi",
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
#recode the PR Block variable
table(HoR2014$PRBLOCK)
class(HoR2014$PRBLOCK)
HoR2014$prblock.factor<-factor(x=HoR2014$PRBLOCK,ordered=FALSE)
HoR2014$prblock.factor.japanese<-HoR2014$prblock.factor
levels(HoR2014$prblock.factor.japanese)<-c("北海道","東北","北関東","南関東",
                                           "東京","北陸・信越","東海","近畿",
                                           "中国","四国","九州","非該当")
HoR2014$prblock.factor.english<-HoR2014$prblock.factor
levels(HoR2014$prblock.factor.english)<-c("Hokkaido","Tohoku","Kita Kanto",
                                          "Minami Kanto","Tokyo",
                                          "Hokuriku/Shinetsu","Tokai",
                                          "Kinki","Chugoku","Shikoku",
                                          "Kyushu","Not PR")
#recode the INCUMB variable into a dummy
table(HoR2014$INCUMB)
class(HoR2014$INCUMB)
HoR2014$incumb.dummy<-factor(x=HoR2014$INCUMB,ordered=TRUE)
levels(HoR2014$incumb.dummy)
levels(HoR2014$incumb.dummy)<-c("0","0","1")
HoR2014$incumb.dummy<-as.numeric(as.character(HoR2014$incumb.dummy))
table(HoR2014[,"incumb.dummy"])

#recode the PARTY variable
table(HoR2014$PARTY)
class(HoR2014$PARTY)
HoR2014$party.factor<-factor(x=HoR2014$PARTY,ordered=FALSE)

HoR2014$party.factor.japanese<-HoR2014$party.factor
levels(HoR2014$party.factor.japanese)
levels(HoR2014$party.factor.japanese)<-c("自民党","民主党","維新の党","公明党",
                                         "次世代の党","共産党","生活の党",
                                         "社民党","新党改革","幸福実現党",
                                         "支持政党なし","諸派","無所属")


HoR2014$party.factor.english<-HoR2014$party.factor
levels(HoR2014$party.factor.english)<-c("LDP","DPJ","Ishin","Komeito",
                                        "Jisedai","JCP","Seikatsu",
                                        "SDP","NRP","Happiness",
                                        "No Party Party","Minor parties",
                                        "Unaffiliated")
is.ordered(HoR2014$party.factor.english)
is.ordered(HoR2014$party.factor.japanese)

#recode the SEX variable into a dummy
table(HoR2014$SEX)
class(HoR2014$SEX)
HoR2014$female<-factor(x=HoR2014$SEX,ordered=FALSE)
levels(HoR2014$female)
levels(HoR2014$female)<-c("0","1")
HoR2014$female<-as.numeric(as.character(HoR2014$female))
table(HoR2014$female)

#recode the RESULT variable
table(HoR2014$RESULT)
class(HoR2014$RESULT)
HoR2014$result.factor<-factor(x=HoR2014$RESULT,ordered=FALSE)
levels(HoR2014$result.factor)<-c("lost","won SMD", "zombie", "won PR only")
levels(HoR2014$result.factor)

#generate dummy variable for support for foreign labor
class(HoR2014$Q6_13)
table(HoR2014$Q6_13)
HoR2014$support.forlab.dummy<-HoR2014$Q6_13
table(HoR2014$support.forlab.dummy)
HoR2014$support.forlab.dummy[HoR2014$support.forlab.dummy < 3] <- 1
HoR2014$support.forlab.dummy[HoR2014$support.forlab.dummy > 2] <- 0
HoR2014$support.forlab.100<-HoR2014$support.forlab.dummy*100
table(HoR2014$support.forlab.100)

#now generate a dummy for oppose foreign labor
class(HoR2014$Q6_13)
table(HoR2014$Q6_13)
HoR2014$oppose.forlab.dummy<-HoR2014$Q6_13
table(HoR2014$oppose.forlab.dummy)
HoR2014$oppose.forlab.dummy[HoR2014$oppose.forlab.dummy < 4] <- 0
HoR2014$oppose.forlab.dummy[HoR2014$oppose.forlab.dummy > 3] <- 1
HoR2014$oppose.forlab.100<-HoR2014$oppose.forlab.dummy*100
table(HoR2014$oppose.forlab.100)

#now generate a dummy for neutral
class(HoR2014$Q6_13)
table(HoR2014$Q6_13)
HoR2014$neutral.forlab.dummy<-HoR2014$Q6_13
table(HoR2014$neutral.forlab.dummy)
HoR2014$neutral.forlab.dummy[HoR2014$neutral.forlab.dummy != 3] <- 0 
HoR2014$neutral.forlab.dummy[HoR2014$neutral.forlab.dummy == 3] <- 1
HoR2014$neutral.forlab.100<-HoR2014$neutral.forlab.dummy*100
table(HoR2014$neutral.forlab.100)

party.cand.agg.2014 <- HoR2014 %>%     
  group_by(party.factor.english) %>%    # Group by these variables
  summarise(
    forlab.mean=mean(support.forlab.100, na.rm = TRUE),
    neutral.forlab=mean(neutral.forlab.100,na.rm=TRUE),
    oppose.forlab=mean(oppose.forlab.100, na.rm = TRUE))


#Clean HoR2012--------------------------------------
#remove the 99s from the foreign labor question (Q5_17)
table(HoR2012$Q5_17)
class(HoR2012$Q5_17)
HoR2012$Q5_17[HoR2012$Q5_17>5]<-NA
any(is.na(HoR2012$Q5_17))

HoR2012$Q5_17.factor<-factor(x=HoR2012$Q5_17,ordered=TRUE)
table(HoR2012$Q5_17)

class(HoR2012$Q5_17.factor)
levels(HoR2012$Q5_17.factor)<-c("agree", "somewhat agree", 
                                "neither agree nor disagree", 
                                "somewhat disagree", "disagree") 
table(HoR2012$Q5_17.factor)
levels(HoR2012$Q5_17.factor)


#recode the PREFEC variable
table(HoR2012$PREFEC)
class(HoR2012$PREFEC)
HoR2012$prefec.factor<-factor(x=HoR2012$PREFEC,ordered=FALSE)
HoR2012$prefec.factor.japanese<-HoR2012$prefec.factor
levels(HoR2012$prefec.factor.japanese)<-c("北海道","青森","岩手","宮城",
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

HoR2012$prefec.factor.english<-HoR2012$prefec.factor
levels(HoR2012$prefec.factor.english)<-c("Hokkaido","Aomori","Iwate","Miyagi",
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

HoR2012$prefec.factor.2019levels<-HoR2012$prefec.factor
levels(HoR2012$prefec.factor.2019levels)<-c("Hokkaido","Aomori","Iwate","Miyagi",
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
#recode the PR Block variable
table(HoR2012$PRBLOCK)
class(HoR2012$PRBLOCK)
HoR2012$prblock.factor<-factor(x=HoR2012$PRBLOCK,ordered=FALSE)
HoR2012$prblock.factor.japanese<-HoR2012$prblock.factor
levels(HoR2012$prblock.factor.japanese)<-c("北海道","東北","北関東","南関東",
                                           "東京","北陸・信越","東海","近畿",
                                           "中国","四国","九州","非該当")
HoR2012$prblock.factor.english<-HoR2012$prblock.factor
levels(HoR2012$prblock.factor.english)<-c("Hokkaido","Tohoku","Kita Kanto",
                                          "Minami Kanto","Tokyo",
                                          "Hokuriku/Shinetsu","Tokai",
                                          "Kinki","Chugoku","Shikoku",
                                          "Kyushu","Not PR")
#recode the INCUMB variable into a dummy
table(HoR2012$INCUMB)
class(HoR2012$INCUMB)
HoR2012$incumb.dummy<-factor(x=HoR2012$INCUMB,ordered=TRUE)
levels(HoR2012$incumb.dummy)
levels(HoR2012$incumb.dummy)<-c("0","0","1")
HoR2012$incumb.dummy<-as.numeric(as.character(HoR2012$incumb.dummy))
table(HoR2012[,"incumb.dummy"])

#recode the PARTY variable
table(HoR2012$PARTY)
class(HoR2012$PARTY)
HoR2012$party.factor<-factor(x=HoR2012$PARTY,ordered=FALSE)

HoR2012$party.factor.japanese<-HoR2012$party.factor
levels(HoR2012$party.factor.japanese)
levels(HoR2012$party.factor.japanese)<-c("民主党","自民党","未来の党","公明党",
                                         "日本維新の会","共産党","みんなの党",
                                         "社民党","新党大地","国民新党","新党日本",
                                         "新党改革","諸派","無所属")

HoR2012$party.factor.english<-HoR2012$party.factor
levels(HoR2012$party.factor.english)<-c("DPJ","LDP","Tomorrow","Komeito",
                                        "Ishin","JCP","YP",
                                        "SDP","Shinto Daiichi","Kokoumin",
                                        "New Party Nippon",
                                        "NRP","Minor parties","Unaffiliated")
is.ordered(HoR2012$party.factor.english)
is.ordered(HoR2012$party.factor.japanese)

#recode the SEX variable into a dummy
table(HoR2012$SEX)
class(HoR2012$SEX)
HoR2012$female<-factor(x=HoR2012$SEX,ordered=FALSE)
levels(HoR2012$female)
levels(HoR2012$female)<-c("0","1")
HoR2012$female<-as.numeric(as.character(HoR2012$female))
table(HoR2012$female)

#recode the RESULT variable
table(HoR2012$RESULT)
class(HoR2012$RESULT)
HoR2012$result.factor<-factor(x=HoR2012$RESULT,ordered=FALSE)
levels(HoR2012$result.factor)<-c("lost","won SMD", "zombie", "won PR only")
levels(HoR2012$result.factor)

#generate dummy variable for support for foreign labor
class(HoR2012$Q5_17)
table(HoR2012$Q5_17)
HoR2012$support.forlab.dummy<-HoR2012$Q5_17
table(HoR2012$support.forlab.dummy)
HoR2012$support.forlab.dummy[HoR2012$support.forlab.dummy < 3] <- 1
HoR2012$support.forlab.dummy[HoR2012$support.forlab.dummy > 2] <- 0
HoR2012$support.forlab.100<-HoR2012$support.forlab.dummy*100
table(HoR2012$support.forlab.100)

#now generate a dummy for oppose foreign labor
class(HoR2012$Q5_17)
table(HoR2012$Q5_17)
HoR2012$oppose.forlab.dummy<-HoR2012$Q5_17
table(HoR2012$oppose.forlab.dummy)
HoR2012$oppose.forlab.dummy[HoR2012$oppose.forlab.dummy < 4] <- 0
HoR2012$oppose.forlab.dummy[HoR2012$oppose.forlab.dummy > 3] <- 1
HoR2012$oppose.forlab.100<-HoR2012$oppose.forlab.dummy*100
table(HoR2012$oppose.forlab.100)

#now generate a dummy for neutral
class(HoR2012$Q5_17)
table(HoR2012$Q5_17)
HoR2012$neutral.forlab.dummy<-HoR2012$Q5_17
table(HoR2012$neutral.forlab.dummy)
HoR2012$neutral.forlab.dummy[HoR2012$neutral.forlab.dummy != 3] <- 0 
HoR2012$neutral.forlab.dummy[HoR2012$neutral.forlab.dummy == 3] <- 1
HoR2012$neutral.forlab.100<-HoR2012$neutral.forlab.dummy*100
table(HoR2012$neutral.forlab.100)

party.cand.agg.2012 <- HoR2012 %>%     
  group_by(party.factor.english) %>%    # Group by these variables
  summarise(
    forlab.mean=mean(support.forlab.100, na.rm = TRUE),
    neutral.forlab=mean(neutral.forlab.100,na.rm=TRUE),
    oppose.forlab=mean(oppose.forlab.100, na.rm = TRUE))


#Clean HoR2009--------------------------------------
#remove the 99s from the foreign labor question (Q9_17)
table(HoR2009$Q9_17)
class(HoR2009$Q9_17)
HoR2009$Q9_17[HoR2009$Q9_17>5]<-NA
any(is.na(HoR2009$Q9_17))

HoR2009$Q9_17.factor<-factor(x=HoR2009$Q9_17,ordered=TRUE)
table(HoR2009$Q9_17)

class(HoR2009$Q9_17.factor)
levels(HoR2009$Q9_17.factor)<-c("agree", "somewhat agree", 
                                "neither agree nor disagree", 
                                "somewhat disagree", "disagree") 
table(HoR2009$Q9_17.factor)
levels(HoR2009$Q9_17.factor)


#recode the PREFEC variable
any(is.na(HoR2009$PREFEC))
#since this variable has NA I will remove the PR only values

table(HoR2009$PREFEC)
class(HoR2009$PREFEC)
HoR2009$prefec.factor<-factor(x=HoR2009$PREFEC,ordered=FALSE)
HoR2009$prefec.factor.japanese<-HoR2009$prefec.factor
levels(HoR2009$prefec.factor.japanese)<-c("北海道","青森","岩手","宮城",
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
                                          "宮崎","鹿児島","沖縄")

HoR2009$prefec.factor.english<-HoR2009$prefec.factor
levels(HoR2009$prefec.factor.english)<-c("Hokkaido","Aomori","Iwate","Miyagi",
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
                                         "Okinawa")

HoR2009$prefec.factor.2019levels<-HoR2009$prefec.factor
levels(HoR2009$prefec.factor.2019levels)<-c("Hokkaido","Aomori","Iwate","Miyagi",
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
                                            "Miyazaki","Kagoshima","Okinawa")
#recode the PR Block variable
any(is.na(HoR2009$PRBLOCK))
#since this variable has NA I will remove the PR only values
table(HoR2009$PRBLOCK)
class(HoR2009$PRBLOCK)
HoR2009$prblock.factor<-factor(x=HoR2009$PRBLOCK,ordered=FALSE)
HoR2009$prblock.factor.japanese<-HoR2009$prblock.factor
levels(HoR2009$prblock.factor.japanese)<-c("北海道","東北","北関東","南関東",
                                           "東京","北陸・信越","東海","近畿",
                                           "中国","四国","九州")
HoR2009$prblock.factor.english<-HoR2009$prblock.factor
levels(HoR2009$prblock.factor.english)<-c("Hokkaido","Tohoku","Kita Kanto",
                                          "Minami Kanto","Tokyo",
                                          "Hokuriku/Shinetsu","Tokai",
                                          "Kinki","Chugoku","Shikoku",
                                          "Kyushu")
#recode the INCUMB variable into a dummy
table(HoR2009$INCUMB)
class(HoR2009$INCUMB)
HoR2009$incumb.dummy<-factor(x=HoR2009$INCUMB,ordered=TRUE)
levels(HoR2009$incumb.dummy)
levels(HoR2009$incumb.dummy)<-c("0","0","1")
HoR2009$incumb.dummy<-as.numeric(as.character(HoR2009$incumb.dummy))
table(HoR2009[,"incumb.dummy"])

#recode the PARTY variable
table(HoR2009$PARTY)
class(HoR2009$PARTY)
HoR2009$party.factor<-factor(x=HoR2009$PARTY,ordered=FALSE)

HoR2009$party.factor.japanese<-HoR2009$party.factor
levels(HoR2009$party.factor.japanese)
levels(HoR2009$party.factor.japanese)<-c("自民党","民主党","公明党","共産党",
                                         "社民党","国民新党","みんなの党",
                                         "新党日本","新党大地","改革クラブ",
                                         "諸派","無所属")

HoR2009$party.factor.english<-HoR2009$party.factor
levels(HoR2009$party.factor.english)<-c("LDP","DPJ","Komeito","JCP",
                                        "SDP","Kokumin","YP",
                                        "New Party Nippon","Shinto Daiichi",
                                        "Reform Club","Minor parties",
                                        "Unaffiliated")

is.ordered(HoR2009$party.factor.english)
is.ordered(HoR2009$party.factor.japanese)

#recode the SEX variable into a dummy
table(HoR2009$SEX)
class(HoR2009$SEX)
HoR2009$female<-factor(x=HoR2009$SEX,ordered=FALSE)
levels(HoR2009$female)
levels(HoR2009$female)<-c("0","1")
HoR2009$female<-as.numeric(as.character(HoR2009$female))
table(HoR2009$female)

#recode the RESULT variable
table(HoR2009$RESULT)
class(HoR2009$RESULT)
HoR2009$result.factor<-factor(x=HoR2009$RESULT,ordered=FALSE)
levels(HoR2009$result.factor)<-c("won SMD", "won PR only", "zombie", "lost")
levels(HoR2009$result.factor)

#generate dummy variable for support for foreign labor
class(HoR2009$Q9_17)
table(HoR2009$Q9_17)
HoR2009$support.forlab.dummy<-HoR2009$Q9_17
table(HoR2009$support.forlab.dummy)
HoR2009$support.forlab.dummy[HoR2009$support.forlab.dummy < 3] <- 1
HoR2009$support.forlab.dummy[HoR2009$support.forlab.dummy > 2] <- 0
HoR2009$support.forlab.100<-HoR2009$support.forlab.dummy*100
table(HoR2009$support.forlab.100)

#now generate a dummy for oppose foreign labor
class(HoR2009$Q9_17)
table(HoR2009$Q9_17)
HoR2009$oppose.forlab.dummy<-HoR2009$Q9_17
table(HoR2009$oppose.forlab.dummy)
HoR2009$oppose.forlab.dummy[HoR2009$oppose.forlab.dummy < 4] <- 0
HoR2009$oppose.forlab.dummy[HoR2009$oppose.forlab.dummy > 3] <- 1
HoR2009$oppose.forlab.100<-HoR2009$oppose.forlab.dummy*100
table(HoR2009$oppose.forlab.100)

#now generate a dummy for neutral
class(HoR2009$Q9_17)
table(HoR2009$Q9_17)
HoR2009$neutral.forlab.dummy<-HoR2009$Q9_17
table(HoR2009$neutral.forlab.dummy)
HoR2009$neutral.forlab.dummy[HoR2009$neutral.forlab.dummy != 3] <- 0 
HoR2009$neutral.forlab.dummy[HoR2009$neutral.forlab.dummy == 3] <- 1
HoR2009$neutral.forlab.100<-HoR2009$neutral.forlab.dummy*100
table(HoR2009$neutral.forlab.100)

party.cand.agg.2009 <- HoR2009 %>%     
  group_by(party.factor.english) %>%    # Group by these variables
  summarise(
    forlab.mean=mean(support.forlab.100, na.rm = TRUE),
    neutral.forlab=mean(neutral.forlab.100,na.rm=TRUE),
    oppose.forlab=mean(oppose.forlab.100, na.rm = TRUE))

#clean PubOp2017-----------------------------
#remove the 99s from the foreign labor question (Q23_12)
PubOp2017$Q23_12[PubOp2017$Q23_12>5]<-NA
any(is.na(PubOp2017$Q23_12))
PubOp2017$for.lab.2017<-PubOp2017$Q23_12


#recode the PREFEC variable
PubOp2017$prefec.factor<-factor(x=PubOp2017$PREFEC,ordered=FALSE)
PubOp2017$prefec.factor.2019levels<-PubOp2017$prefec.factor
levels(PubOp2017$prefec.factor.2019levels)<-c("Hokkaido","Aomori","Iwate","Miyagi",
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

#make aggregated public opinion variable
prefs.2019.pubop.2017<-PubOp2017 %>%
  group_by(prefec.factor.2019levels)%>%
  summarise( 
    for.lab.2017.mean=mean(for.lab.2017, na.rm=TRUE))

#clean PubOp2014-----------------------------
#remove the 99s from the foreign labor question (W1Q16_13)
PubOp2014$W1Q16_13[PubOp2014$W1Q16_13>5]<-NA
any(is.na(PubOp2014$W1Q16_13))
PubOp2014$for.lab.2014<-PubOp2014$W1Q16_13


#recode the PREFEC variable
PubOp2014$prefec.factor<-factor(x=PubOp2014$PREFEC,ordered=FALSE)
PubOp2014$prefec.factor.2019levels<-PubOp2014$prefec.factor
levels(PubOp2014$prefec.factor.2019levels)<-c("Hokkaido","Aomori","Iwate","Miyagi",
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

#make aggregated public opinion variable
prefs.2019.pubop.2014<-PubOp2014 %>%
  group_by(prefec.factor.2019levels)%>%
  summarise( 
    for.lab.2014.mean=mean(for.lab.2014, na.rm=TRUE))
prefs.2019.pubop.2014<-na.omit(prefs.2019.pubop.2014)
View(prefs.2019.pubop.2014)

#merge public opinion dataframes----------------------------------------------------
prefs.2019.pubop<-merge(prefs.2019.pubop.2017,prefs.2019.pubop.2014, 
                        by="prefec.factor.2019levels")
#look for correlation
cor.test(prefs.2019.pubop$for.lab.2017.mean,prefs.2019.pubop$for.lab.2014.mean)
#relationship isn't significant. This suggests that this data is not picking up
#continuities between prefectures betweeen 2014 and 2017, which may reflect
#that the sample size in many prefectures is too small to pick this up.
#when looking at public opinion, I will focus on the 2017 data because it is 
#closest to the 2019 election that is the focus of my analysis. The poll taken 
#after the 2019 election did not include an immigration question
