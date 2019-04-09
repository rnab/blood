#Missing data example code for PLRe

setwd("C:/RNOneDrive/OneDrive/Documents/BBK/projects/blood/dev")

library(dplyr)
library(ggplot2)
library(mice)

#Read in labmort_all
lm<-readRDS("../data/labmort_all.RDS")
lm$bmi<-round(lm$bmi,1)
lm$nimp<-as.factor(lm$nimp)
#lm$d1y<-as.factor(lm$d1y)
#lm$d5y<-as.factor(lm$d5y)
#lm$d10y<-as.factor(lm$d10y)
lm$ageband<-ifelse(lm$age>80,8,floor(lm$age/10))

#Only have partial follow up in last year so discard 2009
lm<-lm[lm$src!="2009",]
#Discard anyone below 20
lm<-lm[lm$age>=20,]

#Examine NAs
na_count <-sapply(lm, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count$var<-rownames(na_count)



#Generate missingness pattern for report
#This varlist excludes the variables that I drop in final analysis
xvars<-c('wcpsi','rcpsi','lmppcnt','pvpsi','rwp','crp')
prevars<-c('sex',	'age', 'd5y','bmi', 'nimp',	'smokerstatus')
tomiss<-lm[,c(prevars,xvars)]
names(tomiss)<-c('sex','age','d5y','bmi','nimp','smokerstatus','wbc','rbc','lmp','mpv','rdw','crp')

m=md.pattern(tomiss,rotate.names=T)

#Demonstrate bias
toPlot<-lm%>%group_by(d5y,age,d=is.na(crp))%>%summarise(n=n())
ggplot(data=toPlot,aes(x=age,y=n,col=d))+geom_line()

