library(dplyr)
library(ggplot2)
library(mice)

#Read in labmort_all
lm<-readRDS("data/labmort_all_2015.RDS")
lm$bmi<-round(lm$bmi,1)
lm$nimp<-as.factor(lm$nimp)
#lm$d1y<-as.factor(lm$d1y)
#lm$d5y<-as.factor(lm$d5y)
#lm$d10y<-as.factor(lm$d10y)
lm$ageband<-ifelse(lm$age>80,8,floor(lm$age/10))

#Only have partial follow up in last year so discard 2009
#lm<-lm[lm$src!="2009",]

#Discard anyone below 20
lm<-lm[lm$age>=20,]

#Examine NAs
na_count <-sapply(lm, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count$var<-rownames(na_count)



#Generate missingness pattern for report
#This varlist excludes the variables that I drop in final analysis
xvars<-c('wcpsi','rcpsi','lmppcnt','pvpsi','rwp','crp','chpsi','cepsi','trpsi','mvpsi','pbp','cop','uap','bup','tpp','amp','ldpsi','sgp')
prevars<-c('sex',	'age', 'd5y','bmi', 'nimp',	'smokerstatus')
tomiss<-lm[,c(prevars,xvars)]
names(tomiss)<-c('sex','age','d5y','bmi','nimp','smokerstatus','wbc','rbc','lmp','mpv','rdw','crp','chl','cre','tri','mcv','pbp','cot','sua','bun','pro','alb','ldh','glu')

m=md.pattern(tomiss)
#z<-data.frame(m)
write.csv(m,"output/misspattern.csv")



####HOTDECK IMPUTATION####
library(VIM)
#Set up ageband
lm$ageband<-ifelse(lm$age>80,8,floor(lm$age/10))
#Check reasonable number of records in each domain
table(lm$ageband,lm$smokerstatus,lm$d5y)


impcells<-lm%>%count(ageband,smokerstatus,d5y)
write.csv(impcells,'output/impcells.csv')


hdvars<-c(	'bmi', 'wcpsi', 'rcpsi', 'lmppcnt','pvpsi', 'rwp', 'crp', 'plpsi', 'chpsi', 'cepsi', 'trpsi', 'gbpsi', 'hgp', 'mvpsi', 'pbp', 'cop', 'scp', 'uap', 'bup', 'tpp', 'amp','ldpsi','sgp')

set.seed(412871543)
lmi<-hotdeck(data=lm,variable = hdvars,domain_var=c('ageband','smokerstatus','d5y'))


#Check imputations#

xraw=lmi$cop[lmi$cop_imp==F&lmi$smokerstatus=="SM"]
ximp=lmi$cop[lmi$cop_imp==T&lmi$smokerstatus=="SM"]
hist(xraw)
hist(ximp)

toplot<-aggregate(lmi$crp~lmi$age+lmi$crp_imp,FUN='mean')
names(toplot)<-c('x','var_imp','var')
ggplot(data=toplot,aes(x=x,y=var,col=var_imp))+geom_point()

toplot<-aggregate(lmi$cepsi~lmi$age+lmi$cepsi_imp,FUN='mean')
names(toplot)<-c('x','var_imp','var')
ggplot(data=toplot,aes(x=x,y=var,col=var_imp))+geom_point()

toplot<-aggregate(lmi$trpsi~lmi$age+lmi$trpsi_imp,FUN='mean')
names(toplot)<-c('x','var_imp','var')
ggplot(data=toplot,aes(x=x,y=var,col=var_imp))+geom_point()

toplot<-aggregate(lmi$wcpsi~lmi$age+lmi$wcpsi_imp,FUN='median')
names(toplot)<-c('x','var_imp','var')
ggplot(data=toplot,aes(x=x,y=var,col=var_imp))+geom_point()

toplot<-aggregate(lmi$cop~lmi$smokerstatus+lmi$cop_imp,FUN='mean')
names(toplot)<-c('x','var_imp','var')
ggplot(data=toplot,aes(x=x,y=var,col=var_imp))+geom_point()

toplot<-aggregate(lmi$rwp~lmi$age+lmi$rwp_imp,FUN='median')
names(toplot)<-c('x','var_imp','var')
ggplot(data=toplot,aes(x=x,y=var,col=var_imp))+geom_point()


morttable<-readRDS("data/morttable.RDS")

lmi_m<-merge(lmi,morttable,by=c("sex","age"))


PlotSMR<-function(var,imp,d5y,q5y,lq,uq,dp,vname,bw){
  
  x=round(var,dp)
  a=quantile(x,probs=lq,na.rm=T)
  b=quantile(x,probs=uq,na.rm=T)
  #bw=3/(10^(dp+1))
  z<-aggregate(cbind(d5y,q5y)~x+imp,FUN='sum')
  z$ave<-z[,3]/z[,4]
  z$se=sqrt(z[,3])/z[,4]
  toplot<-z[z[,1]<b & z[,1]>a,]
  p=ggplot(data=toplot,aes(x=toplot[,1],y=toplot[,5],col=toplot[,2]))+geom_point()+
    ggtitle(paste("5yr SMR by ",vname,sep=""))+labs(x=colnames(x),y="Standardised Mortality Ratio (5yr)")+geom_errorbar(aes(ymin=ave-1.96*se, ymax=ave+1.96*se), width=bw/2)
  p
  }

PlotSMR(lmi_m$wcpsi,lmi_m$wcpsi_imp,lmi_m$d5y,lmi_m$pqx,0.02,0.98,0,"White Blood Cell Count",0.5)
PlotSMR(lmi_m$rcpsi,lmi_m$rcpsi_imp,lmi_m$d5y,lmi_m$pqx,0.01,0.99,1,"Red Blood Cell Count",0.05)
PlotSMR(lmi_m$rwp,lmi_m$rwp_imp,lmi_m$d5y,lmi_m$pqx,0.01,0.99,1,"Red Blood Cell Distribution Width",0.05)
PlotSMR(lmi_m$pbp,lmi_m$pbp_imp,lmi_m$d5y,lmi_m$pqx,0.01,0.99,0,"Lead (ug dL)",0.5)


###Rblood cell count example plot ###
x=round(lmi_m$rcpsi/2,1)*2
a=quantile(x,probs=0.01,na.rm=T)
b=quantile(x,probs=0.99,na.rm=T)
#bw=3/(10^(dp+1))
z<-aggregate(cbind(lmi_m$d5y,lmi_m$pqx)~x+lmi_m$rcpsi_imp,FUN='sum')
names(z)<-c('x','imputed','V1','V2')
z$ave<-z[,3]/z[,4]
z$se=sqrt(z[,3])/z[,4]
toplot<-z[z[,1]<b & z[,1]>a,]
p=ggplot(data=toplot,aes(x=toplot[,1],y=toplot[,5],col=imputed))+geom_point()+
  ggtitle("5yr SMR by Red Blood Cell Count")+labs(x="Red Blood Cell Count",y="Standardised Mortality Ratio (5yr)")+geom_errorbar(aes(ymin=ave-1.96*se, ymax=ave+1.96*se), width=0.05/2)
p
ggsave(filename="RBCImpExample.png",device="png",path="output/",width=8,height=4,p)


####Save Imputed Datasets ####
saveRDS(lmi,"data/lmi_2015.RDS")
write.csv(lmi,"data/lmi_2015.csv")


####MODEL IMPUTATION####
#library(mice)
#lm$agesq=lm$age^2
#lm$cotlevel<-as.factor(ifelse(is.na(lm$cop)==T,'',ifelse(lm$cop<1,"Low",ifelse(lm$cop<100,"Med","High"))))
#newprevars<-c('sex','age','agesq','diabetestype',	'smokerstatus', 'd5y', 'nimp',	'bmi', 'wcpsi', 'rcpsi', 'lmppcnt','pvpsi', 'rwp', 'crp', 'plpsi', 'chpsi', 'cepsi', 'trpsi', 'gbpsi', 'hgp', 'mvpsi', 'pbp', 'cotlevel', 'scp', 'uap', 'bup', 'tpp', 'amp','ldpsi','sgp')
#lmi<-mice(lm[,newprevars],method='pmm')
#saveRDS(lmi,"data/lmi.RDS")
#write.csv(lmi,"data/lmi.csv")


#Checks on imputed values
#z<-ifelse(lmi$where[,('wcpsi')]==F,lmi$data$wcpsi,lmi$imp$wcpsi[,1])
#isreal<-ifelse(lmi$where[,('wcpsi')]==T,"Imputed","Original")
#age=lmi$data$age
#toplot<-data.frame(aggregate(z~age+isreal,FUN='mean'))
#ggplot(data=toplot,aes(x=age,y=z,col=isreal))+geom_point()