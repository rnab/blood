#####Data checks and summaries#####

#check properties of nhall is consistent with nhc and nh3
nh3<-readRDS('data/labmortNHA3.rds')
nhc<-readRDS('data/labmortNHAC.rds')
nhall<-readRDS('data/labmort_all.rds')
sum(nh3$seqn)
sum(nhall$seqn[nhall$src=="nha3"])
sum(nhc$seqn)
sum(nhall$seqn[nhall$src != "nha3"])
rm(nh3,nhc)

####Age/Sex/Smoker/Impairment distribution - not used####
library(dplyr)
nhall<-nhall[nhall$age>19,]
nhall$n<-1
nhall$src2<-ifelse(nhall$src=="nha3","nha3","nhac")
z=nhall%>%group_by(src2,sex,smokerstatus)%>%summarise(n=sum(n),mbmi=mean(bmi,na.rm=T),mage=mean(age),mfup=mean(fup,na.rm=T),nd=sum(mortstat,na.rm=T),nd1=sum(d1y,na.rm=T),
                                       nd5=sum(d5y,na.rm=T),mean(nimp,na.rm=T))


####Missingness by age####
xvar_nodupe<-c('wcp','lmppcnt','moppcnt','grppcnt','lmp','mop','grp','rcp','hgp','htp','mvpsi','mcpsi',
               'mhp','rwp','plp','dwp','grpdif','lmpdif','mopdif','eop','bop','blp','prp','mep','mlp',
               'bap','lap', 'anp','bsp', 'hzp','pkp','pop','mrp','mip','sip','shp','ttp','txp','vup',
               'pbp','epp','fep','tip', 'pxp','frp','fop','rbp','vbp','vcp','icpsi','capsi','sep','vap',
               'vep', 'acp','bcp','bxp','lup','lyp','rep','cop','tcp','tgp', 'lcp','hdp','aap', 'abp',
               'lpp','fhpsi','lhpsi','fbp', 'crp','napsi','skpsi','clpsi','c3psi',
               'scp', 'psp','uap', 'sgp', 'bup','tbp','cep', 'sfp','chp','trp', 'aspsi', 'atpsi','ggpsi',
               'ldpsi','appsi', 'tpp', 'amp','gbp','ospsi')

nh3<-nh3[nh3$age>=20,]
na_count <-sapply(nh3[,xvar_nodupe], function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count$var<-rownames(na_count)
xvar_nodupe2<-na_count$var[na_count$na_count/17030 <0.1]

#just look at crp

nh3$wcpna=ifelse(is.na(nh3$crp)==T,1,0)
nh3$n<-1

nh3$ageband=floor(nh3$age/5)*5
z=nh3%>%group_by(ageband)%>%summarise(missno=sum(wcpna),n=sum(n))

write.csv(z,"crpmissbyage.csv")


####Create Kaplan Meier Survival plots ####
library(survival)
library(survminer)
nhallx<-nhall[nhall$age>=60 & nhall$age <70,]

#by impairment count
fit<-survfit(Surv(fup, mortstat)~nimp, data = nhallx)
ggsurvplot(fit)+ggtitle("Kaplan-Meier survival curves by impairment count: Aged 60-69 at outset")


#by smoker status
fit<-survfit(Surv(fup, mortstat)~smokerstatus, data = nhallx)
ggsurvplot(fit)+ggtitle("Kaplan-Meier survival curves by smoker status: Aged 60-69 at outset")



###cotinine in Smokers####

x<-summary(nhall$cop[nhall$smokerstatus=="NS"])
summary(nhall$cop[nhall$smokerstatus=="EX"])
summary(nhall$cop[nhall$smokerstatus=="SM"])


library(ggplot2)
nhallz<-nhall[nhall$cop<600,]
ggplot(nhallz, aes(x=cop)) + 
  geom_histogram(data = subset(nhallz,smokerstatus=="NS"), fill = "red", alpha = 0.2) + 
  geom_histogram(data = subset(nhallz,smokerstatus=="EX"), fill = "blue", alpha = 0.2)


ggplot(nhallz, aes(x=cop)) + 
    geom_histogram(data = subset(nhallz,smokerstatus=="SM"), fill = "blue", alpha = 0.5) + ggtitle("Serum cotinine levels (ng/mL) in current smokers")



#RDW by sex
mrdw<-aggregate(rwp~age,data=subset(nhall,sex==1),FUN='mean')
frdw<-aggregate(rwp~age,data=subset(nhall,sex==2),FUN='mean')

toplot<-cbind(mrdw,frdw[,2])
names(toplot)<-c('age','Males','Females')
ggplot(data=toplot)+geom_line(aes(x=age,y=Males),col='blue')+geom_line(aes(x=age,y=Females),col='red')


rdw<-nhall%>%group_by(age,sex)%>%summarise(AvRDW=mean(rwp,na.rm=T))
rdw$sex2=ifelse(rdw$sex==1,"Male","Female")
ggplot(data=rdw,aes(x=age,y=AvRDW,col=sex2))+geom_point()+ggtitle("Average RDW reading by Age and Sex")
