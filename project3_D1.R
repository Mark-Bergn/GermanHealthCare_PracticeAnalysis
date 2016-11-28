#R Project 3 Github Copy
library(car)
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

setwd("C:/Users/markb/Desktop/Stats")
dfile.na<-read.csv("Assignments/Project 3/german_healthcare_usage.csv", header=T)
#Dataing cleaning and Structure:  
  
  #Remove missing observations;
    dfile<-na.omit(dfile.na)
    nrow(dfile.na)-nrow(dfile) #number of NA's in original file
    dfile<-dfile[,c(1:2,4:44)]#Year Variable Dropped as Dummy Vars are used instead;
    
    #Basic Characteristics  
    dim(dfile)
    head(dfile,2)
    names(dfile)
    length(names(dfile))
    str(dfile) 
    
    
    
    
#Subsetting and Sorting Data
  qual<-cbind(dfile$FEMALE,dfile$HANDDUM, dfile$TRAVEL, dfile$HHKIDS, dfile[,11:21], dfile[,24:26], dfile$DOCTOR,dfile$HEALTHY)
  head(dfile[,11])
  head(dfile[,"MARRIED"])
  head(dfile$HEALTHY)
  qual=dfile[,c(1:2,4,6,8,10:20,23:25,28:36,40)]
  quant=dfile[,-c(1:2,4,6,8,10:20,23:25,28:36,40)]
  colnames(qual)<-colnames(dfile[c(1:2,4,6,8,10:20,23:25,28:36,40)])
  colnames(quant)<-colnames(dfile[-c(1:2,4,6,8,10:20,23:25,28:36,40)])
  summary(quant)
  quant<-quant[,c(1:7,9:14)]#Removing HSAT b/c NEWSAT is better
  summary(quant)
  head(qual)
  head(quant)
  ncol(qual)
  ncol(quant)
 
  
  

#Sorting quant data by countinous vs. discrete:
summary(quant)
unique(quant$EDUC) #Not sure how to classify
quant.d<-cbind(quant$AGE, quant$DOCVIS, quant$HOSPVIS, quant$NUMOBS, quant$TI, quant$NEWHSAT, quant$PRESCRIP)
quant.c<-cbind(quant$ALC, quant$HANDPER, quant$EDUC, quant$INCOME, quant$LOGINC, quant$HHNINC)



#Cullen and Frey for Discrete Quantitative Variables;
dim(quant.d)
  for (i in 1:7){
   descdist(quant.d[,i], discrete = TRUE, boot=1000)
  }
  
#Cullen and Frey for Continuous Quantitative Variables;
dim(quant.c)
for (i in 1:6){
  descdist(quant.c[,i], discrete = TRUE, boot=1000)
}

#Histogram for Quantitive Variables:

for (i in 1:14){
  truehist(quant[,i],col="skyblue3",ylab="Counts",xlab="Values")
}
  

#main function not working for title:

#barplots for qualitative Variables  
for (i in 1:20){
   barplot(qual[,i],col="skyblue3",ylab="Counts",xlab="Values",main=colnames(qual[,i]))
}

#^Figure out better plots display
  

