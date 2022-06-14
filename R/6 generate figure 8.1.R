#############################
# Michael Strausz
# Replication files for chapter
# On 2019 HoC election
# Generate figure 8.1
# 5/15/2022 : 3/11/2024
#############################

#install and load necessary libraries-----------------------
library(tidyverse)
library(reshape2)
library (scales)

#look at overall
overall.agg<-subset(x=HoC2019, subset = cand2019==1) #only look at candidates
overall.agg <- overall.agg %>%     # Start with the HoC2019 dataframe
  summarise(
    forlab.mean=mean(support.forlab.100, na.rm = TRUE),
    neutral.forlab=mean(neutral.forlab.100,na.rm=TRUE),
    oppose.forlab=mean(oppose.forlab.100, na.rm = TRUE))
overall.agg$psai<-c("all candidates")#create a new vector to label this in the graph
                                    #named psai for party sex age incumbent

#reorder the dataframe to match the other dataframes
overall.agg1<-overall.agg[,c(4,1,2,3)] 
overall.agg<-overall.agg1
remove(overall.agg1)

#make psai into a factor
overall.agg$psai<-as.factor(overall.agg$psai)


#Bring in the parties that one at least one seat-----------------------
party.cand.agg1<-subset(x=HoC2019, subset = cand2019==1) #only look at candidates
party.cand.agg1 <- party.cand.agg1 %>%     
  group_by(party.factor.english) %>%    # Group by these variables
  summarise(
    forlab.mean=mean(support.forlab.100, na.rm = TRUE),
    neutral.forlab=mean(neutral.forlab.100,na.rm=TRUE),
    oppose.forlab=mean(oppose.forlab.100, na.rm = TRUE),
    seats=sum(RESULT, na.rm = TRUE)
  )

#second, I filter out the parties that won n seats or more
party.cand.agg1<-subset(x=party.cand.agg1, subset = seats>=7) #I set n to be 7

#third, I filter out the unaffiliated variable
party.cand.agg1<-party.cand.agg1[party.cand.agg1$party.factor.english!=
                                   "Unaffiliated",]

#fourth, I sort by the won seats variable
party.cand.agg1 <- party.cand.agg1[order
                                   (party.cand.agg1$seats, 
                                     decreasing = TRUE),]

#fifth, I remove the won seats variable
party.cand.agg1<-party.cand.agg1[,1:4]

#rename the variable that will be the datalabels as psai for party sex age incumbency
names(party.cand.agg1)[names(party.cand.agg1) == 
                         'party.factor.english'] <- 'psai'

#bring in gender---------------
HoC2019$female.labeled<-factor(x=HoC2019$female,ordered=FALSE)
levels(HoC2019$female.labeled)<-c("male","female")
gender.agg<-subset(x=HoC2019, subset = cand2019==1) #only look at candidates
gender.agg <- gender.agg %>%
  group_by(female.labeled) %>%    # Group by these variables
  summarise(
    forlab.mean=mean(support.forlab.100, na.rm = TRUE),
    neutral.forlab=mean(neutral.forlab.100,na.rm=TRUE),
    oppose.forlab=mean(oppose.forlab.100, na.rm = TRUE))

#rename the variable that will be the datalabels as psai for party sex age incumbency
names(gender.agg)[names(gender.agg) == 
                    'female.labeled'] <- 'psai'

#bring in age--------------------
HoC2019$age.groups[HoC2019$AGE < 40] <- "30s"
HoC2019$age.groups[HoC2019$AGE > 39 & HoC2019$AGE < 50] <- "40s"
HoC2019$age.groups[HoC2019$AGE > 49 & HoC2019$AGE < 60] <- "50s"
HoC2019$age.groups[HoC2019$AGE > 59 & HoC2019$AGE < 70] <- "60s"
HoC2019$age.groups[HoC2019$AGE > 69 & HoC2019$AGE < 80] <- "70s"
HoC2019$age.groups[HoC2019$AGE > 79] <- "80s"
age.agg1<-subset(x=HoC2019, subset = cand2019==1) #only look at candidates
age.agg1<- age.agg1 %>%     # Start with the HoC2019 dataframe
  group_by(age.groups) %>%    # Group by these variables
  summarise(
    forlab.mean=mean(support.forlab.100, na.rm = TRUE),
    neutral.forlab=mean(neutral.forlab.100,na.rm=TRUE),
    oppose.forlab=mean(oppose.forlab.100, na.rm = TRUE))

#rename the variable that will be the datalabels as psai for party sex age incumbency
names(age.agg1)[names(age.agg1)=='age.groups']<-'psai'

#bring in challenger/incumbent-------------
HoC2019$incumb.labeled<-factor(x=HoC2019$incumb.dummy,ordered=FALSE)
levels(HoC2019$incumb.labeled)<-c("challenger","incumbent")
incumb.agg1<-subset(x=HoC2019, subset = cand2019==1) #only look at candidates
incumb.agg1<- incumb.agg1 %>%     # Start with the HoC2019 dataframe
  group_by(incumb.labeled) %>%    # Group by these variables
  summarise(
    forlab.mean=mean(support.forlab.100, na.rm = TRUE),
    neutral.forlab=mean(neutral.forlab.100,na.rm=TRUE),
    oppose.forlab=mean(oppose.forlab.100, na.rm = TRUE))

#rename the variable that will be the datalabels as psai for party sex age incumbency
names(incumb.agg1)[names(incumb.agg1)=='incumb.labeled']<-'psai'

#merge data---------------
agg2019a <- rbind(overall.agg,party.cand.agg1,gender.agg,age.agg1,incumb.agg1)
agg2019a.melt<-melt(agg2019a, id.vars = "psai")

#generate the graphic-------------------
ggplot(agg2019a.melt, aes(x = psai, y = value, 
                          fill = variable)) + 
  geom_bar(stat = "identity", color="black", 
           position = position_stack(reverse = TRUE)) +
  scale_fill_brewer(labels = c("agree","neither","disagree"), 
                    palette = "Greys")+
  scale_x_discrete(limits = rev)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.caption = element_text(face = "italic"))+
  coord_flip()+
  labs(y = "Percent of 2019 candidates", x=NULL, 
       fill="Views on increasing\n foreign labor", caption="Figure generated by author with data from Taniguchi 2019")
ggsave("Figure 8.1.tiff", units="in", dpi=300, compression = 'lzw')
