#R Project 3 Github Copy

#########################################################################################
# Questions for Rojas:                                                        
# 1. Can we compare AIC from different models?-i.e. linear vs. poissson                                                                                    
#                                                                                       
#                                                                                           
#                                                                                       
#                                                                                       
#########################################################################################

library(pscl)
library(car)
library(AER)
library(lmtest)
library(sandwich)
library(data.table)
require(aod)
library(tis)
library(MASS)
library(fitdistrplus)
library(Quandl)
library(reshape2)
library(ggplot2)
require(msm)

setwd("C:/Users/markb/Desktop/Stats")
dfile.na<-read.csv("Assignments/Project 3/german_healthcare_usage.csv", header=T)
dim(dfile.na)
names(dfile.na)
#Dataing cleaning and Structure:  
   
  #Remove missing observations;
    dfile<-na.omit(dfile.na)
    head(dfile,2)
    dim(dfile)
    nrow(dfile.na)-nrow(dfile) #number of NA's in original file
    dfile<-dfile[,c(1:2,4:27,29:43)]#Year Variable Dropped as Dummy Vars are used instead;
                                    #Drop HSAT also as we have NEWHSAT
    #Check DOCTOR Variable, may be too correlated with yi
    #Check TI And FAMHIST-
    head(dfile,2) 
    names(dfile)
   #Basic Characteristics  
    dim(dfile)
    head(dfile,2)
    names(dfile)
    length(names(dfile))
    str(dfile) 
    unique(dfile$HAUPTS)
    
    
#Subsetting and Sorting Data
  #Figure out FAMHIST
  qual=dfile[,c(1:2,4,6,8,10:20,23:25,27:35,38,40)]
  quant=dfile[,-c(1:2,4,6,8,10:20,23:25,27:35,38,40)]
  colnames(qual)<-colnames(dfile[c(1:2,4,6,8,10:20,23:25,27:35,38,40)])
  colnames(quant)<-colnames(dfile[-c(1:2,4,6,8,10:20,23:25,27:35,38,40)])
  head(quant,2)
  head(qual,2)
  dim(qual)
  dim(quant)
  length(unique(quant$EDUC))
  quant$EDUC
  temp<-dfile[,dfile$EDUC==11.5]
  table(dfile$EDUC)
  head(quant,2)

#Sorting quant data by countinous vs. discrete:
dim(quant)

#Dropped the TI variable at this stage
quant.d<-cbind(quant$AGE, quant$EDUC, quant$DOCVIS, quant$HOSPVIS, quant$NUMOBS, quant$PRESCRIP)
quant.c<-cbind(quant$ALC, quant$HANDPER, quant$LOGINC, quant$HHNINC)
qd.names<-c("Age", "Educ", "DocVis", "HospVis", "NumObs", "Prescrip")
qc.names<-c("Alc", "HandPer", "LogInc", "HHINC")
colnames(quant.d)<-qd.names
colnames(quant.c)<-qc.names


#Cullen and Frey for Discrete Quantitative Variables;
dim(quant.d)
names(quant.d)
  for (i in 1:6){
   descdist(quant.d[,i], discrete = TRUE, boot=1000)
  }

no.ed<-quant.d[,c(1, 3:6)]

#Compare AIC.
no.ed<-quant.d[,c(1, 3:6)]
#^education removed b/c we can't fit to Pois


############################
#                          #
#Check Out this Loop Matt  #
#                          #  
############################
for (i in 1:5 ){
  aic.pois<-list()
  aic.gem<-list()
  fp<-list()
  fg<-list()
fp[[i]]<- fitdist(no.ed[,i], "pois");
fg[[i]]<-fitdist(no.ed[,i], "geom")
aic.pois[[i]]<-summary(fp[[i]])
aic.geom[[i]]<-summary(fg[[i]])
}
aic.pois
aic.geom
#^What these to list AIC/Summaries for all columns



truehist(dfile$DOCVIS) 
#Cullen and Frey for Continuous Quantitative Variables;
dim(quant.c)
for (i in 1:4){
  descdist(quant.c[,i], discrete = TRUE, boot=1000)
}



#Histogram for Quantitive Variables:

for (i in 1:14){
  truehist(quant[,i],col="skyblue3",ylab="Counts",xlab="Values")
}
  



#barplots for qualitative Variables  
t.1<-table(qual[,2])
barplot(t.1)
qual.nme<-names(qual)
for (i in 2:30){
  t.tab<-table(qual[,i])
  barplot(t.tab,col="skyblue3",ylab=,xlab="Values", main= paste("Barplot for",qual.nme[i]))
}

#^Figure out better plots display/add legends
                   
 
####################
#Regression Models #
####################
  head(dfile,2)
  unique(dfile.na$YEAR)
  names(dfile)
  dim(dfile)
  y<-dfile$DOCVIS
  Xmat<-as.matrix(dfile[,c(2:20,22:34,36,38:41)]) #matrix of all other variables
  reg.full<-lm(y~Xmat)
  summary(reg.full)
  AIC(reg.full)
  
  
  #Drop Loginc,HAUPTS, FAMHIST, Reals, FACHHS, ABITUR, Doctor as too reepetative
  #Drop all employment variables except "Working"
  colnames(Xmat)
  drops <- c("LOGINC","FAMHIST", "HAUPTS", "REALS", "ABITUR", "UNIV", "YEAR1984", "HOSPITAL","HANDDUM","FACHHS","WHITEC", "BLUEC","SELF", "BEAMT", "UNEMPLOY", "DOCTOR" )
  X.red<-Xmat[ ,!(colnames(Xmat) %in% drops)]
  X1<-Xmat[ ,!(colnames(Xmat) %in% drops)]
  #Dropping YEAR1984 so as to make it our "base" year w.r.t other year dummy variables

###############################################
# Some code for Looking at Employment Dummies #
###############################################  
  tot<-length(Xmat[,1])
  num.emp<-sum(Xmat[,"WORKING"])
  num.unemp<-sum(Xmat[,"UNEMPLOY"])
  num.wc<-sum(Xmat[,"BLUEC"])
  num.bc<-sum(Xmat[,"WHITEC"])
  num.self<-sum(Xmat[,"SELF"])
  num.bem<-sum(Xmat[,"BEAMT"])
  #For now we will only distingish between employed and unemployed
reg.2<-lm(y~X.red)  
summary(reg.full)
summary(reg.2)
  
  
  
#############################################  
#Other Count Model Examples and Statistics: #
#############################################


#Poisson Regressions:
  summary(reg.full)
  AIC(reg.full)
  AIC(reg.2)
  m1 <- glm(y ~ Xmat, family="poisson")
  summary(m1)
  AIC(m1)
  m2 <- glm(y ~ X.red, family="poisson")
  summary(m2)
  AIC(m2) 
  #In m2 (poisson): EDUC,ADDON & PRESCRIP are only non-significant. 

#Just gonna try dropping them for now

#Third Reduced Data set and Regressions
  
#Linear Reg 3:  
  
  drop.2<-c("EDUC", "ADDON", "PRESCRIP")  
  X.red2<-X.red[ ,!(colnames(X.red) %in% drop.2)]
  reg.3<-lm(y~X.red2)
  summary(reg.3)
  AIC(reg.3) 

#Poisson Reg 3:

  m3<-glm(y ~ X.red2, family="poisson")
  summary(m3) #Everything is significant in this Model
  AIC(m3)     #But AIC is high than in m2
#Linear AIC
AIC(reg.full)
AIC(reg.2)
AIC(reg.3)


#Poisson AIC
AIC(m1) #Not sure why this AIC is so much lower than all others
AIC(m2)
AIC(m3)
  
#When using poisson regression you should test for over dispersion;
  
  dispersiontest(m2) #from AER package
  dispersiontest(m2,trafo=2) #not sure what trafo does  
  #^Since we don't see over dispersion there is no need to use neg.binom model
  
  
#Now Consider Poisson vs. Zero.Inf or Hurdle Models:  
#Let's examen predicted zeros;
  mu <- predict(m1, type = "response")
  exp <- sum(dpois(x = 0, lambda = mu))
  round(exp) #expected number of "zero visits"
  sum(dfile$DOCVIS<1) #"actual number of "zero visits"
  #^Wow that is actually really good, probably no need for hurdle model



#So here we can claim since there is no over dispersion and... 
#we do not underestimate # of zeros Poisson is appropriate count data model.

p.fit<-m1$fitted.values
y.fit<-reg.2$fitted.values
  
  
  
#Other Types of Count data Regression Models:

#Neg.binomial Regressions:
  # ^ Specifically use this when overdispersion is present:
  
  neg.bin<-glm.nb(y~X.red)
  summary(neg.bin)
  AIC(neg.bin)

  
    
#Haven't gotten any other the models below this point to run, 
#maybe because we don't need to improv?
  
#Hurdle Poisson Model:
  #unsure of how to use X1, 
  #in her code it looks like its just = Xmat
  X1<-X.red
  
  hpoisson <- hurdle(y ~ X.red, link = "logit", dist = "poisson")
  summary(hpoisson)
  
  ?hurdle
  
# Hurdle or truncated negative binonomial model coefficients
  
  hnegbin <- hurdle(y ~ X.red | X1, link = "logit", dist = "negbin")
  summary(hnegbin)
  
  # Zero-inflated Poisson model coefficients
  
  zip <- zeroinfl(y ~ X.red | X1, link = "logit", dist = "poisson")
  summary(zip)
  
#Zero-inflated negative binomial model coefficients
  
  zinb <- zeroinfl(y ~ X.red | X1, link = "logit", dist = "negbin")
  summary(zinb)

#Subsetting and Sorting Data
  #Figure out FAMHIST
  qual=dfile[,c(1:2,4,6,8,10:20,23:25,27:35,38,40)]
  quant=dfile[,-c(1:2,4,6,8,10:20,23:25,27:35,38,40)]
  colnames(qual)<-colnames(dfile[c(1:2,4,6,8,10:20,23:25,27:35,38,40)])
  colnames(quant)<-colnames(dfile[-c(1:2,4,6,8,10:20,23:25,27:35,38,40)])
  head(quant,2)
  head(qual,2)
  dim(qual)
  dim(quant)
  length(unique(quant$EDUC))
  quant$EDUC
  temp<-dfile[,dfile$EDUC==11.5]
  table(dfile$EDUC)
  head(quant,2)

#Sorting quant data by countinous vs. discrete:
dim(quant)

#Dropped the TI variable at this stage
quant.d<-cbind(quant$AGE, quant$EDUC, quant$DOCVIS, quant$HOSPVIS, quant$NUMOBS, quant$PRESCRIP)
quant.c<-cbind(quant$ALC, quant$HANDPER, quant$LOGINC, quant$HHNINC)
qd.names<-c("Age", "Educ", "DocVis", "HospVis", "NumObs", "Prescrip")
qc.names<-c("Alc", "HandPer", "LogInc", "HHINC")
colnames(quant.d)<-qd.names
colnames(quant.c)<-qc.names


#Cullen and Frey for Discrete Quantitative Variables;
dim(quant.d)
names(quant.d)
  for (i in 1:6){
   descdist(quant.d[,i], discrete = TRUE, boot=1000)
  }

no.ed<-quant.d[,c(1, 3:6)]

#Compare AIC.
no.ed<-quant.d[,c(1, 3:6)]
#^education removed b/c we can't fit to Pois


############################
#                          #
#Check Out this Loop Matt  #
#                          #  
############################
for (i in 1:5 ){
  aic.pois<-list()
  aic.gem<-list()
  fp<-list()
  fg<-list()
fp[[i]]<- fitdist(no.ed[,i], "pois");
fg[[i]]<-fitdist(no.ed[,i], "geom")
aic.pois[[i]]<-summary(fp[[i]])
aic.geom[[i]]<-summary(fg[[i]])
}
aic.pois
aic.geom
#^What these to list AIC/Summaries for all columns



truehist(dfile$DOCVIS) 
#Cullen and Frey for Continuous Quantitative Variables;
dim(quant.c)
for (i in 1:4){
  descdist(quant.c[,i], discrete = TRUE, boot=1000)
}



#Histogram for Quantitive Variables:

for (i in 1:14){
  truehist(quant[,i],col="skyblue3",ylab="Counts",xlab="Values")
}
  

#main function not working for title:

#barplots for qualitative Variables  
t.1<-table(qual[,2])
barplot(t.1)
qual.nme<-names(qual)
for (i in 2:30){
  t.tab<-table(qual[,i])
  barplot(t.tab,col="skyblue3",ylab=,xlab="Values", main= paste("Barplot for",qual.nme[i]))
}

#^Figure out how to format more/add legends

                 
 
####################
#Regression Models #
####################
  head(dfile,2)
  unique(dfile.na$YEAR)
  names(dfile)
  y<-dfile$DOCVIS
  Xmat<-as.matrix(dfile[,c(2:20,22:34,36,38:41)]) #matrix of all other variables
  reg.full<-lm(y~Xmat)
  summary(reg.full)
  AIC(reg.full)
  
  
  #Drop Loginc,HAUPTS, FAMHIST, Reals, FACHHS, ABITUR
  #Drop all employment variables except "Working"
  colnames(Xmat)
  drops <- c("LOGINC","FAMHIST", "HAUPTS", "REALS", "ABITUR", "UNIV", "YEAR1984", "HOSPITAL","HANDDUM","FACHHS","WHITEC", "BLUEC","SELF", "BEAMT", "UNEMPLOY" )
  X.red<-Xmat[ ,!(colnames(Xmat) %in% drops)]
  #Dropping YEAR1984 so as to make it our "base" year w.r.t other year dummy variables

###############################################
# Some code for Looking at Employment Dummies #
###############################################  
  tot<-length(Xmat[,1])
  num.emp<-sum(Xmat[,"WORKING"])
  num.unemp<-sum(Xmat[,"UNEMPLOY"])
  num.wc<-sum(Xmat[,"BLUEC"])
  num.bc<-sum(Xmat[,"WHITEC"])
  num.self<-sum(Xmat[,"SELF"])
  num.bem<-sum(Xmat[,"BEAMT"])
  #For now we will only distingish between employed and unemployed
reg.2<-lm(y~X.red)  
summary(reg.full)
summary(reg.2)
  
  
  
  
#Other Model Stats
#Using
summary(reg.full)
AIC(reg.full)
m1 <- glm(y ~ Xmat, family="poisson")
summary(m1)
AIC(m1)
m2 <- glm(y ~ X.red, family="poisson")
AIC(m2)
