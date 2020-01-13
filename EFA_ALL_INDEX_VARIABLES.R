##NEW EFA FOR ALL INDEX Variables###
###8/15/2018#####
library(psych)
library(GPArotation)
##Starting with SOP###
setwd("C:/Users/dlars/Dropbox/R_Clackamas/Dissertation/Ch.1/Exploratory_Anal")
def<-read.csv('Definitely_prepanel_WTP_QAQC12_AllFactors_Monoton_Wide.csv')

sop<-def[ ,c(123:131)]
sop<-na.omit(sop)
#sop<-sop[,-c(7)]
#how many factors?##
##Assess Additivity##

correl = cor(sop, use = "pairwise.complete.obs")
symnum(correl)
correl

##correlation adequcy bartlett's test##
cortest.bartlett(correl, n=nrow(sop))

nofactors = fa.parallel(sop, fm="ml", fa="fa")
sum(nofactors$fa.values > 1.0)##Old Kaiser Criterion
sum(nofactors$fa.values > 0.7)##New Kaiser Criterion

#sampling adequacy KMO Test#

KMO(correl)

## 2 Factor Model##

round1 = fa(sop, nfactors=2, rotate = "varimax", fm = "ml")
round1

### 1 Factor Model##

round2 = fa(sop[,-c(9)], nfactors=1, rotate = "varimax", fm = "ml")
round2

##So it appears that everything loaded on one factor minus place enjoy.

##Check the Reliability for the round 6 1 Factor Model##

factor1 = c(1:8)


alpha(sop[,factor1], check.keys = TRUE)

##Now try running polychor EFA##

placemodel = fa(sop[,-c(9)], nfactors=1, rotate = "varimax", cor = "poly")
placemodel

##I think this is good. Now move on to Climate Beliefs.##

###CLIMATE BELIEFS INDEX###
clbel<-def[ ,c( 82,83:84)]
clbel<-na.omit(clbel)

##Assess Additivity##

correl = cor(clbel, use = "pairwise.complete.obs")
symnum(correl)
correl

##correlation adequcy bartlett's test##
cortest.bartlett(correl, n=nrow(clbel))

#sampling adequacy KMO Test#

KMO(correl)

#how many factors?##
nofactors = fa.parallel(clbel, fm="ml", fa="fa")
sum(nofactors$fa.values > 1.0)##Old Kaiser Criterion
sum(nofactors$fa.values > 0.7)##New Kaiser Criterion

round1 = fa(clbel, nfactors=1, rotate = "varimax", fm = "ml")
round1

##So when I do this they all load on 1 Factor.

##Check the Alpha Reliability of the 1 Factor Model##

alpha(clbel, check.keys = TRUE)

#Calculate CFI#
1-((round1$STATISTIC-round1$dof)/(round1$null.chisq-round1$null.dof))

##Don't think the above worked##

##Run the polychoric analysis ##

clbelmodel = fa(clbel, nfactors=1, rotate = "varimax", cor = "poly")
clbelmodel

##I think this is good too##

##CLIMATE IMPACT INDEX###

climp<-def[ ,c( 72:81)]
climp<-na.omit(climp)

##Assess Additivity##

correl = cor(climp, use = "pairwise.complete.obs")
symnum(correl)
correl

##correlation adequcy bartlett's test##
cortest.bartlett(correl, n=nrow(climp))

#sampling adequacy KMO Test#

KMO(correl)

#how many factors?##
nofactors = fa.parallel(climp, fm="ml", fa="fa")
sum(nofactors$fa.values > 1.0)##Old Kaiser Criterion
sum(nofactors$fa.values > 0.7)##New Kaiser Criterion

round1 = fa(climp, nfactors=1, rotate = "varimax", fm = "ml")
round1

#The climate move variables are below the threshold. Remove these and go again.#

climp2<-climp[,-c(2,5, 9,10)]## Joel told me to remove Improve 1 and Living
round2 = fa(climp2, nfactors=1, rotate = "varimax", fm = "ml")
round2

##Check the Alpha Reliability of the 1 Factor Model##

alpha(climp2, check.keys = TRUE)

##Run the polychoric analysis ##

climpmodel = fa(climp2, nfactors=1, rotate = "varimax", cor = "poly")
climpmodel

##I think this is good.##

##PNW CLIMATE INDEX###

pnw<-def[ ,c( 85:88)]
pnw<-na.omit(pnw)
##Assess Additivity##

correl = cor(pnw, use = "pairwise.complete.obs")
symnum(correl)
correl

##correlation adequcy bartlett's test##
cortest.bartlett(correl, n=nrow(pnw))

#sampling adequacy KMO Test#

KMO(correl)

#how many factors?##
nofactors = fa.parallel(pnw, fm="ml", fa="fa")
sum(nofactors$fa.values > 1.0)##Old Kaiser Criterion
sum(nofactors$fa.values > 0.7)##New Kaiser Criterion

round1 = fa(pnw, nfactors=1, rotate = "varimax", fm = "ml")
round1

##Polychor Model##

pnwmodel = fa(pnw, nfactors=1, rotate = "varimax", cor = "poly")
pnwmodel

##Check the Alpha Reliability of the 1 Factor Model##

alpha(pnw, check.keys = TRUE)


###SO I THINK THIS IS A PRETTY THOROUGH RUN OF ALL THE VARIABLES USING EFA. JUST GET Dr. Steele's input##
