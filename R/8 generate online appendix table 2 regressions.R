#############################
# Michaael Strausz
# Replication files for chapter
# On 2019 HoC election
# generate online appendix table 2
# regressions with 2019 candidate position as DV
# 6/10/2022 : 6/10/2022
#############################

#install and load necessary libraries-----------------------
library(tidyverse)
library(stargazer)

HoC2019 <- HoC2019 %>%
  mutate(LDP.dummy=recode_factor(party.factor.english, 'LDP'=1,.default=0)) %>% 
  mutate(LDP.dummy = relevel(LDP.dummy, ref = "0")) %>% 
  mutate(female.labeled=relevel(female.labeled, ref="male"))

forlabreg2019_1<-lm(formula = Q4_8 ~ cand2019, data=HoC2019)
forlabreg2019_2<-lm(formula = Q4_8 ~ cand2019+
                      female.labeled, data=HoC2019)
forlabreg2019_3<-lm(formula = Q4_8 ~ AGE+female.labeled+LDP.dummy+cand2019, data=HoC2019)
stargazer(forlabreg2019_1, forlabreg2019_2, forlabreg2019_3, type="html", out="test.html",
          column.labels = c("Model 1", "Model 2", "Model 3"), 
          intercept.bottom = FALSE, 
          single.row=FALSE,     
          notes.append = FALSE, 
          header=FALSE) 