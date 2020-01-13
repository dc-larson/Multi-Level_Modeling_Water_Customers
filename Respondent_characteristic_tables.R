## 4/15/19

## Calculating characteristics for Place Attitudes in New table construction.

require(dplyr)
setwd("C:/Users/dlars/Dropbox/R_Clackamas/Dissertation/Ch.1/Working")
data<-read.csv('Definitely_prepanel_WTP_QAQC14_AllFactors_Monoton_Wide.csv')

# Look at data #

summary(data$Pl_Dependence_rc)

data2<-data %>% filter(!is.na(Pl_Dependence_rc))

#ELSE <- TRUE
attitudes <- data2 %>% mutate(AttitudeSummary1 = 
                               case_when( Pl_Dependence_rc >= 0 &
                                            Pl_Dependence_rc < 2 ~ "Low",
                                          Pl_Dependence_rc >= 2 & 
                                            Pl_Dependence_rc < 3 ~ "Medium",
                                          Pl_Dependence_rc >= 4 ~ "High")) %>% 
  select(Pl_Dependence_rc, AttitudeSummary1)
attp <- attitudes %>% group_by(AttitudeSummary1) %>% summarise(n = n()) %>% ungroup() %>% 
  mutate(total = sum(n), share = n/total)
print(attp)

## Now for place attachment #

summary(data$Pl_Attachment_rc)

data3<-data %>% filter(!is.na(Pl_Attachment_rc))

#ELSE <- TRUE
attitudes2 <- data3 %>% mutate(AttitudeSummary2 = 
                                case_when( Pl_Attachment_rc >= 0 &
                                             Pl_Attachment_rc < 2 ~ "Low",
                                           Pl_Attachment_rc >= 2 & 
                                             Pl_Attachment_rc < 3 ~ "Medium",
                                           Pl_Attachment_rc >= 4 ~ "High")) %>% 
  select(Pl_Attachment_rc, AttitudeSummary2)
attp2 <- attitudes2 %>% group_by(AttitudeSummary2) %>% summarise(n = n()) %>% ungroup() %>% 
  mutate(total = sum(n), share = n/total)
print(attp2)

##Place Identity##

summary(data$Pl_Identity_rc)

data4<-data %>% filter(!is.na(Pl_Identity_rc))

#ELSE <- TRUE
attitudes3 <- data4 %>% mutate(AttitudeSummary3 = 
                                 case_when( Pl_Identity_rc >= 0 &
                                              Pl_Identity_rc < 2 ~ "Low",
                                            Pl_Identity_rc >= 2 & 
                                              Pl_Identity_rc < 3 ~ "Medium",
                                            Pl_Identity_rc >= 4 ~ "High")) %>% 
  select(Pl_Identity_rc, AttitudeSummary3)
attp3<- attitudes3 %>% group_by(AttitudeSummary3) %>% summarise(n = n()) %>% ungroup() %>% 
  mutate(total = sum(n), share = n/total)
print(attp3)