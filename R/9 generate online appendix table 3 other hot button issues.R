#############################
# Michaael Strausz
# Replication files for chapter
# On 2019 HoC election
# Generate online appendix
# table 3 other 
# hot button issues, 2019
# 5/24/2022 : 6/10/2022
#############################

#install and load necessary libraries-----------------------
library(tidyverse)

plotting.data<-HoC2019 %>%
  select(party.factor.english, Q4_10,Q4_11,Q8_1, Q4_8, cand2019) %>%
  filter(party.factor.english=="LDP") %>% 
  filter(cand2019==1) %>% 
  mutate_at(vars(starts_with("Q")), ~replace(., (. == 99), NA)) %>% 
  select(-party.factor.english) %>%
  select(-cand2019) %>% 
  summarise(
    LastNames=sum(Q4_10<3, na.rm = TRUE),
    n_LastNames=sum(!is.na(Q4_10)),
    GayMar=sum(Q4_11<3, na.rm = TRUE),
    n_GayMar=sum(!is.na(Q4_11)),
    Empress=sum(Q8_1<3, na.rm = TRUE),
    n_Empress=sum(!is.na(Q8_1)),
    Imm=sum(Q4_8<3, na.rm = TRUE),
    n_Imm=sum(!is.na(Q4_8))) %>% 
  mutate(p_Imm=100*Imm/n_Imm) %>% 
  mutate(p_LastNames=100*LastNames/n_LastNames) %>% 
  mutate(p_GayMar=100*GayMar/n_GayMar) %>% 
  mutate(p_Empress=100*Empress/n_Empress) %>% 
  select(starts_with("p_"))

plotting.data<-as.data.frame(t(as.matrix(plotting.data)))
plotting.data <- cbind(rownames(plotting.data), data.frame(plotting.data, row.names=NULL))
colnames(plotting.data)<-c("question","percent")
plotting.data$question <- factor(plotting.data$question, 
                                 levels = c("p_Imm", "p_Empress", "p_LastNames", "p_GayMar"))

levels(plotting.data$question)<-c("Admitting foreign labor", "Having Female Emperor","Permitting different surnames",
                                  "Permitting gay marriage")
#graphing LDP only---------------------
ggplot(plotting.data, aes(x = question, y = percent)) +
  geom_col(fill="dark green")+
  geom_text(aes(label = round(percent, digits=1)), vjust = 1.5, 
            colour="white")+
  labs(x=NULL, 
       y="Percent of LDP Diet candidates who support each issue, 2019")+
  labs(caption="Figure generated by author with data from Taniguchi 2019")

#comparing parties on hot button issues-------------------
plotting.data<-HoC2019 %>%
  select(party.factor.english, Q4_10,Q4_11,Q8_1, Q4_8, cand2019) %>%
  filter(cand2019==1) %>% 
  mutate_at(vars(starts_with("Q")), ~replace(., (. == 99), NA)) %>% 
  select(-cand2019) %>% 
  group_by(party.factor.english) %>% 
  summarise(
    n=n(),
    LastNames=sum(Q4_10<3, na.rm = TRUE),
    n_LastNames=sum(!is.na(Q4_10)),
    GayMar=sum(Q4_11<3, na.rm = TRUE),
    n_GayMar=sum(!is.na(Q4_11)),
    Empress=sum(Q8_1<3, na.rm = TRUE),
    n_Empress=sum(!is.na(Q8_1)),
    Imm=sum(Q4_8<3, na.rm = TRUE),
    n_Imm=sum(!is.na(Q4_8))) %>% 
  mutate(p_Imm=100*Imm/n_Imm) %>% 
  mutate(p_LastNames=100*LastNames/n_LastNames) %>% 
  mutate(p_GayMar=100*GayMar/n_GayMar) %>% 
  mutate(p_Empress=100*Empress/n_Empress) %>% 
  select(n | starts_with("p"))
write.csv(plotting.data,"multipartymultiissue.csv", row.names = FALSE)
