## Calculating MEAR based on Joel's code

# 4/22/19

setwd("C:/Users/dlars/Dropbox/R_Clackamas/Dissertation/Ch.1/Modeling")
load(file = "MEAR_issue.rda")

## Computing Variables

pc = mean(mydata$participation_cost, na.rm = TRUE)
#ps = mean(mydata$participation_scenario, na.rm = TRUE) # this is a factor, you will have to choose a level explicitly
ck = mean(mydata$climate_knowledge, na.rm = TRUE)
resp = mean(mydata$respondent, na.rm = TRUE)
cbel = mean(mydata$cclimate_bel_index, na.rm = TRUE)
ccimp = mean(mydata$cclimate_change_impacts, na.rm = TRUE)
pnw = mean(mydata$cpnw_climate_worsening, na.rm = TRUE)
use = mean(mydata$cwatershed_use_freq, na.rm = TRUE)
pli = mean(mydata$cPl_Identity, na.rm = TRUE)
pld= mean(mydata$cPl_Dependence, na.rm = TRUE)
pla = mean(mydata$cPl_Attachment, na.rm = TRUE)
age = mean(mydata$cage, na.rm = TRUE)
edu = mean(mydata$ceducation, na.rm = TRUE)
polit = mean(mydata$cconservative_attitude, na.rm = TRUE)
inc = mean(mydata$cincome, na.rm = TRUE)

library(lme4) # jss: needed for fixef() function
cf = fixef(mod10)

### BEGIN: JSS code below 22nd April 2019
# we are going to construct a vector to reflect the following
example_profile_js = 
  c(1,1,
    # [1] "(Intercept)"                 "participation_cost"                                            
    0,0,
    # [3] "participation_scenarioUSFS"  "participation_scenarioWater Treatment Plant"                   
    ck,cbel,
    # [5] "climate_knowledge"           "cclimate_bel_index"                                            
    ccimp,pnw,
    # [7] "cclimate_change_impacts"     "cpnw_climate_worsening"                                        
    use,pli,
    # [9] "cwatershed_use_freq"         "cPl_Identity"                                                  
    pld,pla,
    # [11] "cPl_Dependence"             "cPl_Attachment"                                                
    age,edu,
    # [13] "cage"                       "ceducation"                                                    
    polit,inc,
    # [15] "cconservative_attitude"     "cincome"                                                       
    0,0)
# [17] "participation_cost:participation_scenarioUSFS" "participation_cost:participation_scenarioWater Treatment Plant"
names(example_profile_js) = names(cf)

# default for intercept term, which is private land (NO INTERACTION)
P1A_js = example_profile_js %*% cf
P1A_js # one dollar on private land (Log-odds)
odds1dollarprivate = exp(P1A_js)
odds1dollarprivate
prob1dollarprivate = exp(P1A_js)/(1+exp(P1A_js))
prob1dollarprivate

##1) So the default for WTP $1 on Private land is 96%

# Let's see how it changes when we up the price to 5 dollars
example_profile_js['participation_cost'] = 5
P5A_js = example_profile_js %*% cf
P5A_js # one dollar on private land (Log-odds)
odds5dollarprivate = exp(P5A_js)
odds5dollarprivate
prob5dollarprivate = exp(P5A_js)/(1+exp(P5A_js))
prob5dollarprivate

##2) So the probability someone is willing to pay $5 on private land is 45%

# Let's compare these price points for public land (YES INTERACTION)
# one dollar
example_profile_js['participation_cost'] = 1 # dollar
example_profile_js['participation_scenarioUSFS'] = 1
# interaction
example_profile_js['participation_cost:participation_scenarioUSFS'] = 
  example_profile_js[['participation_cost']] * example_profile_js[['participation_scenarioUSFS']]
Pub1A_js = example_profile_js %*% cf
Pub1A_js
odds1dollarpub = exp(Pub1A_js)
odds1dollarpub
prob1dollarpub = exp(Pub1A_js)/(1+exp(Pub1A_js))
prob1dollarpub

##3) The probability that someone is WTP $1 on Public land is 96%

# five dollars
example_profile_js['participation_cost'] = 5 # dollar
example_profile_js['participation_scenarioUSFS'] = 1
# interaction
example_profile_js['participation_cost:participation_scenarioUSFS'] = 
  example_profile_js[['participation_cost']] * example_profile_js[['participation_scenarioUSFS']]

Pub5A_js = example_profile_js %*% cf
Pub5A_js
odds5dollarpub = exp(Pub5A_js)
odds5dollarpub
prob5dollarpub = exp(Pub5A_js)/(1+exp(Pub5A_js))
prob5dollarpub

##4) Probability that someone is WTP $5 on public land is 6%

# Let's check 1 dollar across scenarios
# now we check for Water Treatment Plant (YES INTERACTION)
example_profile_js['participation_scenarioUSFS'] = 0 # reset prior scenario
example_profile_js['participation_cost:participation_scenarioUSFS'] = 0 # reset prior interaction

example_profile_js['participation_cost'] = 1 # dollar
example_profile_js['participation_scenarioWater Treatment Plant'] = 1
# interaction
example_profile_js['participation_cost:participation_scenarioWater Treatment Plant'] = 
  example_profile_js[['participation_cost']] * example_profile_js[['participation_scenarioWater Treatment Plant']]

WT1A_js = example_profile_js %*% cf
WT1A_js
odds1dollarwt = exp(WT1A_js)
odds1dollarwt
prob1dollarwt = exp(WT1A_js)/(1+exp(WT1A_js))
prob1dollarwt

##5) Probability for paying $1 for Water Treatment is 97%

# five dollars for water treatment
example_profile_js['participation_cost'] = 5 # dollar
example_profile_js['participation_scenarioWater Treatment Plant'] = 1
# interaction
example_profile_js['participation_cost:participation_scenarioWater Treatment Plant'] = 
  example_profile_js['participation_cost'] * example_profile_js['participation_scenarioWater Treatment Plant']

WT5A_js = example_profile_js %*% cf
WT5A_js
odds5dollarwt = exp(WT5A_js)
odds5dollarwt
prob5dollarwt = exp(WT5A_js)/(1+exp(WT5A_js))
prob5dollarwt

##6) 4% WTP for $5 at a water treatment plant.


# display
table_1_dollar = 
  rbind('Private'=c('est'=P1A_js,'odds'=odds1dollarprivate,'prob'=prob1dollarprivate),
        'Public' =c('est'=Pub1A_js, 'odds'=odds1dollarpub, 'prob'=prob1dollarpub),
        'Water Treatment'=c('est'=WT1A_js, 'odds'=odds1dollarwt,'prob'=prob1dollarwt))
table_1_dollar

table_5_dollar = 
  rbind('Private'=c('est'=P5A_js,'odds'=odds5dollarprivate,'prob'=prob5dollarprivate),
        'Public' =c('est'=Pub5A_js, 'odds'=odds5dollarpub, 'prob'=prob5dollarpub),
        'Water Treatment'=c('est'=WT5A_js, 'odds'=odds5dollarwt,'prob'=prob5dollarwt))
table_5_dollar
# one dollar vs 5 dollars
cbind(table_1_dollar[,-1],table_5_dollar[,-1])

# We reset to a dollar, and look at differences in education for private land use.
edu
edu_sd = sd(mydata$ceducation, na.rm=T)
# reset the profile
example_profile_js['participation_scenarioUSFS'] = 0
example_profile_js['participation_scenarioWater Treatment Plant'] = 0
example_profile_js['participation_cost:participation_scenarioUSFS'] = 0 
example_profile_js['participation_cost:participation_scenarioWater Treatment Plant'] = 0
example_profile_js['participation_cost'] = 1 # dollar

# -1sd edu
example_profile_js['ceducation'] = edu-edu_sd
edu_1below = example_profile_js %*% cf
odds_edu1below = exp(edu_1below)
odds_edu1below
prob_edu1below = exp(edu_1below)/(1+exp(edu_1below))
prob_edu1below

##7) Reset to one dollar looking at Private land, 1 SD below from Mean education results in 93% WTP for SWPP on Private land

# +1sd edu
example_profile_js['ceducation'] = edu+edu_sd
edu_1above = example_profile_js %*% cf
odds_edu1above = exp(edu_1above)
odds_edu1above
prob_edu1above = exp(edu_1above)/(1+exp(edu_1above))
prob_edu1above

##8) Reset to one dollar looking at Private land, 1 SD above from Mean education results in 98% WTP for SWPP on Private land

ed_comp = rbind('1sd below'=c('est'=edu_1below,'odds'=odds_edu1below,'prob'=prob_edu1below),
                'mean'=c('est'=P1A_js,'odds'=odds1dollarprivate,'prob'=prob1dollarprivate),
                '1sd above'=c('est'=edu_1above,'odds'=odds_edu1above,'prob'=prob_edu1above))
ed_comp

## political attitudes
polit
polit_sd = sd(mydata$cconservative_attitude,na.rm=TRUE)
# reset example profile (this could easily be put into a function)
example_profile_js['ceducation'] = edu
# -1sd politics
example_profile_js['cconservative_attitude']=polit-polit_sd
polit_1below = example_profile_js %*% cf
# +1sd politics
example_profile_js['cconservative_attitude']=polit+polit_sd
polit_1above = example_profile_js %*% cf

polit_comp = rbind('1sd below'=c('est'=polit_1below,
                                 'odds'=exp(polit_1below),
                                 'prob'=exp(polit_1below)/(1+exp(polit_1below))),
                   'mean'=c('est'=P1A_js,'odds'=odds1dollarprivate,'prob'=prob1dollarprivate),
                   '1sd above'=c('est'=polit_1above,
                                 'odds'=exp(polit_1above),
                                 'prob'=exp(polit_1above)/(1+exp(polit_1above))))
polit_comp

polit_mean_raw = mean(mydata$Political.Attitudes1, na.rm = T)
polit_mean_raw
polit_sd_raw = sd(mydata$Political.Attitudes1, na.rm = T)
polit_sd_raw


##9) As a respondent moves 1 SD below the mean of politics (More Liberal), they are 98% WTP at $1 on Private land
##### As a respondent moves 1 SD above the mean of politics (More Conservative), they are 92% WTP at $1 on Private land.

### Another check on price
seq_costs = seq(1,5,.25)
example_dat_js = 
  cbind(1,seq_costs,
        0,0,
        ck,cbel,
        ccimp,pnw,
        use,pli,
        pld,pla,
        age,edu,
        polit,inc,
        0,0)
example_costs_est = example_dat_js %*% cf
cost_tab = cbind(seq_costs,
                 example_costs_est,
                 exp(example_costs_est),
                 exp(example_costs_est)/(1+exp(example_costs_est)))
colnames(cost_tab) = c('Participation cost','Estimate','Odds','Probability')
cost_tab
### END: JSS code below 22nd April 2019

## Begin Dan Code 4/22/19 ##

## Try Joels Cost Equation Above but look only at $1 increments

### Another check on price --> DL
seq_costs2 = seq(1,5,1)
example_dat_js = 
  cbind(1,seq_costs2,
        0,0,
        ck,cbel,
        ccimp,pnw,
        use,pli,
        pld,pla,
        age,edu,
        polit,inc,
        0,0)
example_costs_est = example_dat_js %*% cf
cost_tab = cbind(seq_costs2,
                 example_costs_est,
                 exp(example_costs_est),
                 exp(example_costs_est)/(1+exp(example_costs_est)))
colnames(cost_tab) = c('Participation cost','Estimate','Odds','Probability')
cost_tab

## So the variables to look at are participation cost (already done), climate change impacts, watershed use frequency,
### Place identity, Age, Politics( Already done), Income

## Climate Change Impacts

ccimp_sd = sd(mydata$cclimate_change_impacts, na.rm=T)
# reset the profile
example_profile_js['participation_scenarioUSFS'] = 0
example_profile_js['participation_scenarioWater Treatment Plant'] = 0
example_profile_js['participation_cost:participation_scenarioUSFS'] = 0 
example_profile_js['participation_cost:participation_scenarioWater Treatment Plant'] = 0
example_profile_js['participation_cost'] = 1 # dollar

# -1sd ccimp
example_profile_js['cclimate_change_impacts'] = ccimp-ccimp_sd
ccimp_1below = example_profile_js %*% cf
odds_ccimp1below = exp(ccimp_1below)
odds_ccimp1below
prob_ccimp1below = exp(ccimp_1below)/(1+exp(ccimp_1below))
prob_ccimp1below



# +1sd ccimp
example_profile_js['cclimate_change_impacts'] = ccimp+((ccimp_sd)*2)
ccimp_1above = example_profile_js %*% cf
odds_ccimp1above = exp(ccimp_1above)
odds_ccimp1above
prob_ccimp1above = exp(ccimp_1above)/(1+exp(ccimp_1above))
prob_ccimp1above



ccimp_comp = rbind('1sd below'=c('est'=ccimp_1below,'odds'=odds_ccimp1below,'prob'=prob_ccimp1below),
                'mean'=c('est'=P1A_js,'odds'=odds1dollarprivate,'prob'=prob1dollarprivate),
                '1sd above'=c('est'=ccimp_1above,'odds'=odds_ccimp1above,'prob'=prob_ccimp1above))
ccimp_comp

ccimp_sd_raw = sd(mydata$climate_change_impacts, na.rm=T)
ccimp_mean_raw = mean(mydata$climate_change_impacts, na.rm=T)
ccimp_mean_raw
ccimp_sd_raw
##  watershed use frequency

# reset the profile
example_profile_js['participation_scenarioUSFS'] = 0
example_profile_js['participation_scenarioWater Treatment Plant'] = 0
example_profile_js['participation_cost:participation_scenarioUSFS'] = 0 
example_profile_js['participation_cost:participation_scenarioWater Treatment Plant'] = 0
example_profile_js['participation_cost'] = 1 # dollar

# -1sd watershed use
use_sd = sd(mydata$cwatershed_use_freq, na.rm=T)
example_profile_js['cwatershed_use_freq'] = use-use_sd
use_1below = example_profile_js %*% cf
odds_use1below = exp(use_1below)
odds_use1below
prob_use1below = exp(use_1below)/(1+exp(use_1below))
prob_use1below



# +1sd Watershed use
example_profile_js['cwatershed_use_freq'] = use+use_sd
use_1above = example_profile_js %*% cf
odds_use1above = exp(use_1above)
odds_use1above
prob_use1above = exp(use_1above)/(1+exp(use_1above))
prob_use1above



use_comp = rbind('1sd below'=c('est'=use_1below,'odds'=odds_use1below,'prob'=prob_use1below),
                   'mean'=c('est'=P1A_js,'odds'=odds1dollarprivate,'prob'=prob1dollarprivate),
                   '1sd above'=c('est'=use_1above,'odds'=odds_use1above,'prob'=prob_use1above))
use_comp
library(psych)
describe(mydata$watershed_use_freq)
head(mydata$watershed_use_freq)

# Place identity

# reset the profile
example_profile_js['participation_scenarioUSFS'] = 0
example_profile_js['participation_scenarioWater Treatment Plant'] = 0
example_profile_js['participation_cost:participation_scenarioUSFS'] = 0 
example_profile_js['participation_cost:participation_scenarioWater Treatment Plant'] = 0
example_profile_js['participation_cost'] = 1 # dollar

# -1sd place identity
pli_sd = sd(mydata$cPl_Identity, na.rm=T)
example_profile_js['cPl_Identity'] = pli-pli_sd
pli_1below = example_profile_js %*% cf
odds_pli1below = exp(pli_1below)
odds_pli1below
prob_pli1below = exp(pli_1below)/(1+exp(pli_1below))
prob_pli1below



# +1sd Place Identity
example_profile_js['cPl_Identity'] = pli+pli_sd
pli_1above = example_profile_js %*% cf
odds_pli1above = exp(pli_1above)
odds_pli1above
prob_pli1above = exp(pli_1above)/(1+exp(pli_1above))
prob_pli1above



pli_comp = rbind('1sd below'=c('est'=pli_1below,'odds'=odds_pli1below,'prob'=prob_pli1below),
                   'mean'=c('est'=P1A_js,'odds'=odds1dollarprivate,'prob'=prob1dollarprivate),
                   '1sd above'=c('est'=pli_1above,'odds'=odds_pli1above,'prob'=prob_pli1above))
pli_comp

# Mean and SD for Place
mean_pli = mean(mydata$Pl_Identity, na.rm= T)
mean_pli
sd_pli = sd(mydata$Pl_Identity, na.rm = T)
sd_pli

## Age

# reset the profile
example_profile_js['participation_scenarioUSFS'] = 0
example_profile_js['participation_scenarioWater Treatment Plant'] = 0
example_profile_js['participation_cost:participation_scenarioUSFS'] = 0 
example_profile_js['participation_cost:participation_scenarioWater Treatment Plant'] = 0
example_profile_js['participation_cost'] = 1 # dollar

# -1sd Age
age_sd = sd(mydata$cage, na.rm=T)
example_profile_js['cage'] = age-age_sd
age_1below = example_profile_js %*% cf
odds_age1below = exp(age_1below)
odds_age1below
prob_age1below = exp(age_1below)/(1+exp(age_1below))
prob_age1below



# +1sd Age
example_profile_js['cage'] = age+age_sd
age_1above = example_profile_js %*% cf
odds_age1above = exp(age_1above)
odds_age1above
prob_age1above = exp(age_1above)/(1+exp(age_1above))
prob_age1above



age_comp = rbind('1sd below'=c('est'=age_1below,'odds'=odds_age1below,'prob'=prob_age1below),
                 'mean'=c('est'=P1A_js,'odds'=odds1dollarprivate,'prob'=prob1dollarprivate),
                 '1sd above'=c('est'=age_1above,'odds'=odds_age1above,'prob'=prob_age1above))
age_comp

mean(mydata$Age1, na.rm = T)
sd(mydata$Age1, na.rm = T)

## Age not working, move on to income.

## Income

# reset the profile
example_profile_js['participation_scenarioUSFS'] = 0
example_profile_js['participation_scenarioWater Treatment Plant'] = 0
example_profile_js['participation_cost:participation_scenarioUSFS'] = 0 
example_profile_js['participation_cost:participation_scenarioWater Treatment Plant'] = 0
example_profile_js['participation_cost'] = 1 # dollar

# -1sd Income
inc_sd = sd(mydata$cincome, na.rm=T)
example_profile_js['cincome'] = inc-inc_sd
inc_1below = example_profile_js %*% cf
odds_inc1below = exp(inc_1below)
odds_inc1below
prob_inc1below = exp(inc_1below)/(1+exp(inc_1below))
prob_inc1below



# +1sd Income
example_profile_js['cage'] = inc-inc_sd
inc_1above = example_profile_js %*% cf
odds_inc1above = exp(inc_1above)
odds_inc1above
prob_inc1above = exp(inc_1above)/(1+exp(inc_1above))
prob_inc1above



inc_comp = rbind('1sd below'=c('est'=inc_1below,'odds'=odds_inc1below,'prob'=prob_inc1below),
                 'mean'=c('est'=P1A_js,'odds'=odds1dollarprivate,'prob'=prob1dollarprivate),
                 '1sd above'=c('est'=inc_1above,'odds'=odds_inc1above,'prob'=prob_inc1above))
inc_comp