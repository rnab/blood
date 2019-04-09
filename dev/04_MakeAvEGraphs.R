
###SETUP####
library(dplyr)
library(ggplot2)
library(gridExtra)

#nhall<-readRDS("data/labmort_all.RDS")
nha3<-readRDS("data/labmortNHA3.RDS")
nha3<-nha3[nha3$age>=20,]

#create a compound impairment variable
nimp=nha3$hascancer+nha3$hasstroke+nha3$hasemphysema+nha3$hasheartattack+nha3$hasheartfailure+nha3$hasdiabetes
nha3$nimp=ifelse(nimp>4,4,nimp)
table(nha3$nimp)


####CREATE MORTALITY TABLE ####

nha3$cnt<-1
m<-nha3%>%group_by(age,sex)%>%summarise(qx=sum(d5y)/sum(cnt))

#ggplot(data=m,aes(x=age,y=qx,colour=as.factor(sex)))+geom_smooth()+
#  ggtitle("Probability of death in the next 5 years, by age and sex")+labs(x="Age at Survey",y="")
#ggsave(filename="mort.png",path="./smr/")
#Join qx back onto labmort
#nha3<-merge(nha3,m,by=c("sex","age"))
#nha3.cc<-nha3[complete.cases(nha3),]

#I'm not using the smoothed mortality rates as my expected...
set.seed(14365)
mm<-m[m$sex==1,]
mm$pqx<-predict(loess(qx~age,data=mm))
mplot<-ggplot(data=mm)+geom_point(aes(x=age,y=qx),col='blue')+geom_line(aes(x=age,y=pqx),col='red')+ggtitle("Probability of death in 5 years from entry: Males")+labs(x="Age at survey",y="")

mf<-m[m$sex==2,]
mf$pqx<-predict(loess(qx~age,data=mf))
fplot<-ggplot(data=mf)+geom_point(aes(x=age,y=qx),col='blue')+geom_line(aes(x=age,y=pqx),col='red')+ggtitle("Probability of death in 5 years from entry: Females")+labs(x="Age at survey",y="")

g <- arrangeGrob(mplot,fplot,nrow=1)
ggsave(filename="mort.png",path="./smr/",g)


#Now need to merge this back onto nha3 as qx
morttable<-rbind(mm,mf)

saveRDS(morttable,"data/morttable.RDS")
write.csv(morttable,"data/morttable.csv")

nha3<-merge(nha3,morttable,by=c("sex","age"))


####PRODUCE SMR GRAPHS####


#CI explanation
#https://ibis.health.state.nm.us/resource/SMR_ISR.html

PlotSMR<-function(var,d5y,q5y,lq,uq,dp,vname,bw){

x=round(var,dp)
a=quantile(x,probs=lq,na.rm=T)
b=quantile(x,probs=uq,na.rm=T)
#bw=3/(10^(dp+1))
z<-aggregate(cbind(d5y,q5y)~x,FUN='sum')
z$ave<-z[,2]/z[,3]
z$se=sqrt(z[,2])/z[,3]
toplot<-z[z[,1]<b & z[,1]>a,]
p=ggplot(data=toplot,aes(x=toplot[,1],y=toplot[,4]))+geom_point()+
  ggtitle(paste("5yr SMR by ",vname,sep=""))+labs(x=colnames(x),y="Standardised Mortality Ratio (5yr)")+geom_errorbar(aes(ymin=ave-1.96*se, ymax=ave+1.96*se), width=bw/2)
#print(p)
q=qplot(x[x>a&x<b],geom="histogram", main=paste("Distribution of ",vname),xlab=vname,ylab="Frequency",binwidth=bw)
#print(q)
grid.arrange(p,q,nrow=2)
#hist(x[x>a&x<b])

g <- arrangeGrob(p,q, nrow=2) #generates g

ggsave(filename=paste(vname,".png",sep=""),device="png",path="./smr",width=8,height=6,g)
}



PlotSMR(nha3$wcp,nha3$d5y,nha3$pqx,0.02,0.98,0,"White Blood Cell Count",0.5)

PlotSMR(nha3$rcp,nha3$d5y,nha3$pqx,0.01,0.99,1,"Red Blood Cell Count",0.05)

PlotSMR(nha3$pbp,nha3$d5y,nha3$pqx,0.01,0.99,0,"Lead (ug dL)",0.5)

PlotSMR(nha3$sep,nha3$d5y,nha3$pqx,0.02,0.98,-1,"Serum Selenium (ng mL)",5)

PlotSMR(nha3$ldpsi,nha3$d5y,nha3$pqx,0.02,0.98,-1,"Serum lactate dehydrogenase (U L)",5)

PlotSMR(nha3$uap,nha3$d5y,nha3$pqx,0.02,0.98,0,"Serum Uric Acid (mg dL)",0.5)

PlotSMR(nha3$rwp,nha3$d5y,nha3$pqx,0.02,0.95,1,"Red Cell Distribution Width (percent)",0.05)

PlotSMR(nha3$acp,nha3$d5y,nha3$pqx,0.02,0.95,2,"Serum Alpha Carotene (ug dL)",0.5)

PlotSMR(nha3$crp,nha3$d5y,nha3$pqx,0.02,0.98,1,"Serum C-Reactive Protein (mg dL)",0.05)

PlotSMR(nha3$lmppcnt,nha3$d5y,nha3$pqx,0.02,0.98,0,"Lymphocyte percent (Coulter)",0.5)

PlotSMR(nha3$appsi,nha3$d5y,nha3$pqx,0.05,0.95,-1,"Serum Alkaline Phosphatase (U L)",5)

PlotSMR(nha3$bup,nha3$d5y,nha3$pqx,0.05,0.95,1,"Serum Blood-Urea Nitrogen (mg dL)",0.5)

PlotSMR(nha3$dwp,nha3$d5y,nha3$pqx,0.05,0.95,1,"Platelet distribution Width (percent)",0.05)

PlotSMR(nha3$bxp,nha3$d5y,nha3$pqx,0.01,0.95,1,"Serum beta cryptoxanthin (ug dL)",0.5)

PlotSMR(nha3$clpsi,nha3$d5y,nha3$pqx,0.02,0.98,0,"Serum chloride (mmol L)",0.5)

PlotSMR(nha3$amp,nha3$d5y,nha3$pqx,0.02,0.98,1,"Serum albumin (g dL)",0.05)

PlotSMR(nha3$tpp,nha3$d5y,nha3$pqx,0.02,0.98,1,"Serum total protein (g dL)",0.05)

PlotSMR(nha3$lyp,nha3$d5y,nha3$pqx,0.01,0.98,0,"Serum lycopene (ug dL)",0.5)

PlotSMR(nha3$sgp,nha3$d5y,nha3$pqx,0.02,0.95,-1,"Serum glucose (mg dL)",5)

PlotSMR(nha3$grppcnt,nha3$d5y,nha3$pqx,0.02,0.98,0,"Granulocyte percent",0.5)

#PlotSMR(nha3$cop,nha3$d5y,nha3$pqx,0.05,0.95,-2,"Serum cotinine (ng mL)",0.5)

PlotSMR(nha3$cep,nha3$d5y,nha3$pqx,0.02,0.98,1,"Serum creatinine (mg dL)",0.05)

PlotSMR(nha3$mvpsi,nha3$d5y,nha3$pqx,0.05,0.98,0,"Mean cell volume (fL)",0.5)

####COTININE ANALYSIS ####
#Need more detailed analysis for cotinine because of skewed distribution
copgrp<-ifelse(nha3$cop<1,"<1 ng/mL",ifelse(nha3$cop<100,"<100 ng/mL",">100ng/mL"))
z<-aggregate(cbind(nha3$d5y,nha3$qx)~copgrp+nha3$smokerstatus,FUN='sum')
z$ave<-z[,3]/z[,4]
z$se=sqrt(z[,3])/z[,4]

write.csv(z,"./smr/cotinine.csv")


####Quick lead analysis###
nhall<-readRDS("data/labmort_all.RDS")
z=aggregate(nhall$pbp~nhall$src,FUN='mean',na.rm=T)
write.csv(z,"./smr/bloodlead.csv")
