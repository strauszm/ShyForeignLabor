#############################
# Michaael Strausz
# Replication files for chapter
# On 2019 HoC election
# Regression with 2019 pref opinion
# as DV
# 6/6/2022 : 6/6/2022
#############################

#install and load necessary libraries-----------------------
library(tidyverse)
library(stargazer)


prefs.reg<-lm(formula = for.lab.2019.c.mean ~ percent.neutral.2019, data=prefs.summary)

stargazer(prefs.reg, type="html", out="test.html",
          column.labels = c("Model 1"), 
          intercept.bottom = FALSE, 
          single.row=FALSE,     
          notes.append = FALSE, 
          header=FALSE) 

