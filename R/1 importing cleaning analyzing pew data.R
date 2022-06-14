#############################
# Michaael Strausz
# Replication files for chapter
# On 2019 HoC election
# Importing, cleaning, analyzing pew data
# 5/14/2022 : 5/14/2022
#############################

#install and load necessary libraries-----------------------
library(readr)
library(tidyverse)
library(survey)
library(srvyr)
library(reshape2)
library (scales)

#import datasets-------------------------
pew2018 <- read_csv("Data/pew2018.csv")
#this data is from Pew Research Center. 2018. “Global Attitudes and Trends: 
#Spring 2018 Survey Data.” 
#https://www.pewresearch.org/global/dataset/spring-2018-survey-data/.

#clean pew2018--------------------
pew2018$COUNTRY<-as.factor(pew2018$COUNTRY)
levels(pew2018$COUNTRY)<-c("Argentina", "Australia", "Brazil", "Canada", 
                           "France", "Germany", "Greece", "Hungary", "India",
                           "Indonesia", "Israel", "Italy", "Japan", "Kenya", 
                           "Mexico", "Netherlands", "Nigeria", "Philippines",
                           "Poland", "Russia", "South Africa", "South Korea",
                           "Spain", "Sweden", "Tunisia", "United Kingdom",
                           "United States")
#make a subset looking only at the more immigration variable
country.means <- pew2018 %>%
  select(ID, COUNTRY,
         weight,
         immig_moreless)

#about the immig_moreless variable
#Q52. In your opinion, should we allow more immigrants to move to our country, fewer immigrants, or about the same as we do now?
#  1	More
#  2	Fewer
#  3	About the same
#  4	No immigrants at all (don’t read)
#  8		Don’t know
#  9		Refused

#remove the NAs
country.means$immig_moreless[country.means$immig_moreless>4] <- NA

#recode the more immigration variable into four dummies
#first generate dummy variable for support increasing imm
class(country.means$immig_moreless)
table(country.means$immig_moreless)
country.means$more.imm<-country.means$immig_moreless
country.means$more.imm[country.means$more.imm!=1]<-0
country.means$more.imm[country.means$more.imm==1]<-1

#Second, generate dummy for about the same
country.means$same.imm<-country.means$immig_moreless
country.means$same.imm[country.means$same.imm!=3]<-0
country.means$same.imm[country.means$same.imm==3]<-1

#third, generate dummy for less
country.means$less.imm<-country.means$immig_moreless
country.means$less.imm[country.means$less.imm!=2]<-0
country.means$less.imm[country.means$less.imm==2]<-1

#forth, generate dummy for none
country.means$no.imm<-country.means$immig_moreless
country.means$no.imm[country.means$no.imm!=4]<-0
country.means$no.imm[country.means$no.imm==4]<-1

#now make all those dummies into 0/100 variables for easier percents
country.means$more.imm.100<-100*country.means$more.imm
country.means$same.imm.100<-100*country.means$same.imm
country.means$less.imm.100<-100*country.means$less.imm
country.means$no.imm.100<-100*country.means$no.imm

#generate an aggregate dataset of weighted percents
country.means.agg <- country.means
country.means.agg<-country.means.agg %>%  
  as_survey(weights = c(weight)) %>%
  group_by(COUNTRY) %>%
  summarize(
    more.imm=mean(more.imm.100, na.rm=TRUE),
    same.imm=mean(same.imm.100, na.rm=TRUE),
    less.imm=mean(less.imm.100, na.rm=TRUE),
    no.imm=mean(no.imm.100, na.rm=TRUE)
  )


#make the country means variable into a factor ordered by % supporting more imm
country.means.agg<-country.means.agg %>% 
  mutate(COUNTRY = fct_reorder(COUNTRY, more.imm, .desc = TRUE))


#this show the point that I make in the chapter about JApan having the 3rd 
#highest % suporting more imm 
country.means.agg %>% 
  arrange(desc(more.imm))

#this is a stacked bar graph that I ended up having to cut from the paper
country.means.agg.melt<-melt(country.means.agg, id.vars = "COUNTRY")
ggplot(country.means.agg.melt, aes(x = COUNTRY, y = value, 
                                   fill = variable)) + 
  geom_bar(stat = "identity", color="black", 
           position = position_stack(reverse = TRUE)) +
  scale_fill_brewer(labels = c("More immigration","No change","Less immigration",
                               "no immigration"), 
                    palette = "Blues")+
  scale_x_discrete(limits = rev)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  coord_flip()+
  labs(y = "Public opinion, 2018", x=NULL, 
       fill="Best immigration policy")
