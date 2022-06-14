#############################
# Michaael Strausz
# Replication files for chapter
# On 2019 HoC election
# Prefectural correlations
# 6/9/2022 : 6/9/2022
#############################

df <- data.frame(matrix(ncol = 3, nrow = 5))
df["X1"][df["Column Name"] == "Old Value"] <- "New Value"

df[1, ]<-c("% of candidates with no position", 
           cor.test(prefs.summary$for.lab.2019.c.mean,prefs.summary$percent.neutral.2019)$estimate,
           cor.test(prefs.summary$for.lab.2019.c.mean,prefs.summary$percent.neutral.2019)$p.value)
df[2,]<-c("job seeker's ratio",
          cor.test(prefs.summary$for.lab.2019.c.mean,prefs.summary$jsratio2019)$estimate,
          cor.test(prefs.summary$for.lab.2019.c.mean,prefs.summary$jsratio2019)$p.value)
df[3,]<-c("population change",
          cor.test(prefs.summary$for.lab.2019.c.mean,prefs.summary$popchange.201810)$estimate,
          cor.test(prefs.summary$for.lab.2019.c.mean,prefs.summary$popchange.201810)$p.value)
df[4,]<-c("foreign population change",
          cor.test(prefs.summary$for.lab.2019.c.mean,prefs.summary$forpopchange.201910)$estimate,
          cor.test(prefs.summary$for.lab.2019.c.mean,prefs.summary$forpopchange.201910)$p.value)
df[5,]<-c("public opinion",
          cor.test(prefs.summary$for.lab.2019.c.mean,prefs.summary$for.lab.2017.mean)$estimate,
          cor.test(prefs.summary$for.lab.2019.c.mean,prefs.summary$for.lab.2017.mean)$p.value)    
colnames(df)<-c("independent variable","Pearson's R","p-value")
df[,2]<-as.numeric(df[,2])
df[,3]<-as.numeric(df[,3])
write.csv(df,"prefcorrelations.csv", row.names = FALSE)
rm(df)
