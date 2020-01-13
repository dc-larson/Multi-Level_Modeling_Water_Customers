## 2/18/2019#

## ---------> Making final long dataset <-----------------#

require(dplyr)
require(tidyr)
setwd("C:/Users/dlars/Dropbox/R_Clackamas/Dissertation/Ch.1/Working")

def<-read.csv('Definitely_prepanel_WTP_QAQC14_AllFactors_Monoton_Wide.csv')
def = def[,-c(1:6)]

def2 = def

# Working on Panel

def3<- gather(def2, "Scen", "WTP", 97:114)
def3 %>%  group_by(Scen, WTP) %>% summarise(N = n())
def3 %>% filter(ID.x == 1)
def3 <- arrange(def3, ID.x)

##Attempting to Separate##

def4 <- separate(def3, Scen, c("Scenario", "Bid"))
def5<- separate(def3, Scen, c("Scenario", "Bid"), sep = "SWP")

#test = def5[,c(135:137)]

## Remove weird periods ##

Bids<-gsub("^.*?\\.","", def5$Bid)
Bids<-as.data.frame(Bids)
def5<-bind_cols(def5,Bids)
##Cool. This did it!

#Try for scenarios#
Scenarios<-gsub("\\..*","", def5$Scenario)
Scenarios<-as.data.frame(Scenarios)
def5<-bind_cols(def5,Scenarios)

##Cool this worked. Now drop irrelevant columns and save##
#test = def5[,c(135:139)]

def5<-def5[,-c(135,136)]

#setwd("C:/Users/dlars/Dropbox/R_Clackamas/Dissertation/Ch.1/Working")
write.csv(def5, file = "FINAL_Panel_Def_QAQC_Monoton_Long7_current.csv")

setwd("C:/Users/dlars/Dropbox/R_Clackamas/Dissertation/Ch.1/Modeling")
write.csv(def5, file = "FINAL_Panel_Def_QAQC_Monoton_Long7_current.csv")
