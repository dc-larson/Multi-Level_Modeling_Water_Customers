load(file = "MEAR_issue.rda")

## Replicating my analysis so you can see where I get tripped up.##

##Had to remove NA values in order to compute means. The lme4 model just handles them for you
###This could be a potential issue.

wtp = mean(mydata$participation_willingness, na.rm = TRUE)
pc = mean(mydata$participation_cost, na.rm = TRUE)
ps = mean(mydata$participation_scenario, na.rm = TRUE)
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

# Now that I have computed means for my scaled covariates I can extract model fixed effects.

cf = fixef(mod10)

## Now that I have done this construct Joels equation

p1A = cbind(1,1,0,0,ck,cbel,ccimp,pnw,use,pli,pld,pla,age,edu,polit,inc,0,0)%*%cf

## So for some reason it is returning an NA value for p1A

print(cf)

## If you look at the fixed effects, I have mapped everything on correctly...I think?

###The only thing I can't seem to address are the interaction effects between participation_cost:participaion_scenario

## Are these also given a value of "0" since we are only interested at looking at Private Forest Land?
### My next step was to compute the equation below but it won't return anything useful.




## Now comput odds

oddsp1A = exp(p1A)

#Now do probability

probp1A = oddsp1A/1+oddsp1A