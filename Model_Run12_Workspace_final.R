## 02/23/19 ###

#-----------> Model 12 Scratch Workspace <--------------

##Leeeetttttt'sss Try this again

require(pacman)
pacman::p_load(fmsb, car, dplyr, sjPlot, tidyr, ggplot2,lme4)
setwd("C:/Users/dlars/Dropbox/R_Clackamas/Dissertation/Ch.1/Modeling")
def = read.csv("FINAL_Panel_Def_QAQC_Monoton_Long8_current.csv")

def3<-def[,c(5,6:8,9,11,24,25,69,79,80:82,87,95,98,99,112,133:140)]
mydata = def3

##Key Out variables and remember to center and scale##
# water provider
mydata$provider <- factor(mydata$WP)
levels(mydata$provider) <- c('_Clackamas River Water','_Oswego','_SFWB','_OakLodgeGladstoneEstacada','_Sunrise')
summary(mydata$provider)

# age
mydata$age <- mydata$Age1
summary(mydata$age)
hist(mydata$age,col='light blue')
mydata$cage = scale(mydata$age, scale=F) # center age at average
mydata$scage = scale(mydata$age, scale=T)

# education
mydata$education <- mydata$Education1
summary(mydata$education)
barplot(table(mydata$education))
mydata$ceducation = scale(mydata$education, scale=F)
mydata$sceducation = scale(mydata$education, scale=T)

# political attitude
mydata$conservative_attitude <- mydata$Political.Attitudes1
summary(mydata$conservative_attitude)
barplot(table(mydata$conservative_attitude))
mydata$cconservative_attitude = scale(mydata$conservative_attitude, scale=F)
mydata$scconservative_attitude = scale(mydata$conservative_attitude, scale=T)

# knowledge of climate
mydata$climate_knowledge <- mydata$Climate.Know
summary(mydata$climate_knowledge)
barplot(table(mydata$climate_knowledge))
mydata$cclimate_knowledge = scale(mydata$climate_knowledge, scale=F)
mydata$scclimate_knowledge = scale(mydata$climate_knowledge, scale=T)

# place importance. Changed the Pl_Attachement fatcor on 2/23/2019 by dropping Place.Ident from the index
mydata$Pl_Dependence <- mydata$Pl_Dependence_rc 
#summary(mydata$watershed_importance)
#hist(mydata$watershed_importance)
mydata$cPl_Dependence = scale(mydata$Pl_Dependence, scale=F)
mydata$scPl_Dependence = scale(mydata$Pl_Dependence, scale=T)
mydata$Pl_Attachment <- mydata$Pl_Attachment_rc2 
mydata$cPl_Attachment = scale(mydata$Pl_Attachment, scale=F)
mydata$scPl_Attachment = scale(mydata$Pl_Attachment, scale=T)
mydata$Pl_Identity <- mydata$Pl_Identity_rc 
mydata$cPl_Identity = scale(mydata$Pl_Identity, scale=F)
mydata$scPl_Identity = scale(mydata$Pl_Identity, scale=T)



#Now add aggregated SOP

mydata$SOP_index = mydata$SOP
mydata$cSOP_index = scale(mydata$SOP_index, scale=F)
mydata$scSOP_index = scale(mydata$SOP_index, scale=T)

# frequency of watershed use
mydata$watershed_use_freq <- mydata$logWatershedUse2
summary(mydata$watershed_use_freq)
hist(mydata$watershed_use_freq) # <- looks like you have an outlier here... also very skewed. I have since normalize it using the log.
mydata$cwatershed_use_freq = scale(mydata$watershed_use_freq, scale=F)
mydata$scwatershed_use_freq = scale(mydata$watershed_use_freq, scale=T)

# income 
mydata$income <- mydata$inc_val
summary(mydata$income)
barplot(table(mydata$income))
mydata$cincome = scale(mydata$income, scale=F)
mydata$scincome = scale(mydata$income, scale=T)

##climate beliefs##
mydata$climate_bel_control<-mydata$Climate.Control1
#summary(mydata$climate_beliefs)
#barplot(table(mydata$climate_beliefs))
mydata$cclimate_bel_control = scale(mydata$climate_bel_control, scale=F)
mydata$scclimate_bel_control = scale(mydata$climate_bel_control, scale=T)

mydata$climate_bel_deny<-mydata$Climate.Deny1
mydata$cclimate_bel_deny = scale(mydata$climate_bel_deny, scale=F)
mydata$scclimate_bel_deny = scale(mydata$climate_bel_deny, scale=T)

mydata$climate_bel_conseq<-mydata$Climate.Consequence1
mydata$cclimate_bel_conseq = scale(mydata$climate_bel_conseq, scale=F)
mydata$scclimate_bel_conseq = scale(mydata$climate_bel_conseq, scale=T)

#Now add aggregated climate beliefs
mydata$climate_bel_index<-mydata$ClimateBeliefIndex
mydata$cclimate_bel_index = scale(mydata$climate_bel_index, scale=F)
mydata$scclimate_bel_index = scale(mydata$climate_bel_index, scale=T)

##PNW Climate Worsening##
mydata$pnw_climate_worsening<-mydata$PNWClimateIndex
summary(mydata$pnw_climate_worsening)
mydata$cpnw_climate_worsening = scale(mydata$pnw_climate_worsening, scale=F)
mydata$scpnw_climate_worsening = scale(mydata$pnw_climate_worsening, scale=T)

##Climate Change Impact Beliefs##
mydata$climate_change_impacts<-mydata$CCImpactIndex_rc
summary(mydata$climate_change_impacts)
mydata$cclimate_change_impacts = scale(mydata$climate_change_impacts, scale=F)
mydata$scclimate_change_impacts = scale(mydata$climate_change_impacts, scale=T)

#Clackamas County Home#
# double check that this gets included. Probably remove this.
mydata$clackamas_county_home<-mydata$CC.home
summary(mydata$clackamas_county_home)
dim(mydata$clackamas_county_home)
barplot(mydata$clackamas_county_home)
mydata$cclackamas_county_home = scale(mydata$clackamas_county_home, scale=F)
mydata$scclackamas_county_home = scale(mydata$clackamas_county_home, scale=T)

# cost to participate
mydata$participation_cost <- mydata$Bids
summary(mydata$participation_cost)
mydata$cparticipation_cost = scale(mydata$participation_cost, scale=F)
mydata$scparticipation_cost = scale(mydata$participation_cost, scale=T)

# willingness to participate
mydata$participation_willingness <- factor(mydata$WTP)
summary(mydata$participation_willingness)
mydata$cparticipation_willingness = scale(mydata$participation_willingness, scale=F)

# scenario 
mydata$participation_scenario <- factor(mydata$Scenarios)
levels(mydata$participation_scenario) <- c('Private','USFS','Water Treatment Plant') # <- descriptive labels for each scenario
summary(mydata$participation_scenario)

#respondent
# recode into the same data frame # jss. added 
#respondent<-mydata$ID.x
mydata$respondent = mydata$ID.x
summary(mydata$respondent)

# step 4: build the model
##Going to be using the nonlinear function and respondnents are going to be my random intercept.##

nullmod = glmer(participation_willingness ~ 1+(1|respondent),
                data = mydata, na.action=na.omit, family=binomial(link=logit))

set.seed(42)

##Begin Modeling Runs ####


mod1 = glmer(participation_willingness ~ participation_cost*participation_scenario +climate_knowledge+(1|respondent),
              data = mydata, na.action=na.omit, family=binomial(link=logit), 
              control=glmerControl(optimizer='Nelder_Mead',calc.derivs = F))

mod2 = update(mod1, .~. + cclimate_bel_index)


mod3 = update(mod2, .~. + cclimate_change_impacts)
mod4 = update(mod3, .~. + cpnw_climate_worsening)

mod5 = update(mod4, .~. + cwatershed_use_freq)
mod6 = update(mod5, .~. + cPl_Identity+cPl_Dependence+cPl_Attachment)

mod7 = update(mod6, .~. + cage)
mod8 = update(mod7, .~. + ceducation)
mod9 = update(mod8, .~. + cconservative_attitude)
mod10 = update(mod9, .~. + cincome)
summary(mod10)
mod11 = update(mod10, .~. -cclimate_bel_index)
summary(mod11)
summary(mod7)
mod12 = update(mod9, .~. + log(income))

mod13 = update(mod10, .~. -cconservative_attitude)

## So I think mod 10 is the one I want

summary(mod10)

summary(mod11)


## Yup. Mod10 is the one I want. Don't want to do any p-hacking at this point

confint(mod10)

##Print up table##

# Display results in a human understandable format JSS
cbind('est'=coef(summary(mod10))[,1],
      'odds'=exp(coef(summary(mod10))[,1]),
      'prob'=1/(1+exp(-coef(summary(mod10))[,1])),
      'z'=coef(summary(mod10))[,3])
knitr::kable(
  cbind('est'=coef(summary(mod10))[,1],
        'odds'=exp(coef(summary(mod10))[,1]),
        'prob'=1/(1+exp(-coef(summary(mod10))[,1])),
        'z'=coef(summary(mod10))[,3]),
  digits=3
)

## Interpret results
##This is just for my own edification.
mod14 = update(mod10, .~. -climate_knowledge -cclimate_bel_index)

library(margins)
marginal_effects(mod10)
summary(mod10)

exp.pred = 
  
## 04/19/19 #
  
## Calculating MEAR based on Joels Suggestions ##
  
#  'odds'=exp(coef(summary(mod10))  
  
odds.md = exp(coef(summary(mod10))

prob.md = odds.md/1+odds.md

## Kk so that's not working.

cf = fixef(mod10)

## Calculating Marginal Effects at Relative Means (MERMS) and building respondent profiles.

wtp = mydata$participation_willingness
pc = mydata$participation_cost
ps = mydata$participation_scenario
ck = mydata$climate_knowledge
resp = mydata$respondent
cbel = mydata$cclimate_bel_index
ccimp = mydata$cclimate_change_impacts
pnw = mydata$cpnw_climate_worsening
use = mydata$cwatershed_use_freq
pli = mydata$cPl_Identity
pld= mydata$cPl_Dependence
pla = mydata$cPl_Attachment
age = mydata$cage
edu = mydata$ceducation
polit = mydata$cconservative_attitude
inc = mydata$cincome

mydata2 = def3

inc2 = mean(mydata2$cincome, na.rm = TRUE)


Av_wtp = cbind(1,participation_willingness,participation_cost*participation_scenario,0,climate_knowledge+(1|respondent),cclimate_bel_index,
               cclimate_change_impacts,cpnw_climate_worsening,cwatershed_use_freq,cPl_Identity,cPl_Dependence,cPl_Attachment,
               cage,ceducation,cconservative_attitude,cincome)%*%cf


Av_wtp = cbind(1,wtp,pc*ps,ck+(1|resp),cbel,ccimp,pnw,use,pli,pld,pla,age,edu,polit, inc)%*%cf
#               
#               

#age,edu,polit, inc)%*%cf

## Need to take means of all data first

# Select numeric columns
data.numcols <- mydata[, sapply(mydata, is.numeric)]

# Using apply
all.means <- apply(data.numcols, 2, mean)

# Using colMeans
all.means <- colMeans(data.numcols, na.rm = TRUE)

## Try again#
mydata = na.omit(mydata)
ck = mydata$climate_knowledge
ck <- mean(ck)

resp = mydata$respondent

cbel = mydata$cclimate_bel_index
cbel <- mean(cbel)

ccimp = mydata$cclimate_change_impacts
ccimp <- mean(ccimp)

pnw = mydata$cpnw_climate_worsening
pnw <-mean(pnw)

use = mydata$cwatershed_use_freq
use <-mean(use)

pli = mydata$cPl_Identity
pli<-mean(pli)

pld= mydata$cPl_Dependence
pld<-mean(pld)

pla = mydata$cPl_Attachment
pla <-mean(pla)

age = mydata$cage
age<-mean(age)

edu = mydata$ceducation
edu<-mean(edu)

polit = mydata$cconservative_attitude
polit<-mean(polit)

inc = mydata$cincome
inc<-mean(inc)

cost = mydata$participation_cost
cost <-mean(cost)

scen = mydata$participation_scenario
scen <- mean(scen)

## Now that I have done this construct Joels equation

p1A = cbind(1,1,0,0,ck,cbel,ccimp,pnw,use,pli,pld,pla,age,edu,polit,inc,cost,0,0)%*%cf

## Now comput odds

oddsp1A = exp(p1A)

#Now do probability

probp1A = oddsp1A/1+oddsp1A

require(margins)
x = margins(mod10)

cplot(mod10, "participation_cost")
exp.pred = predict()


#save to rda file
save(mydata, mod10,cf, file = "MEAR_issue.rda")
