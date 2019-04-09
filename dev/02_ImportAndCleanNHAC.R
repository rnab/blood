library(Hmisc)
library(dplyr)

getcont<-function(yr,demo_fn,biopro_fn,cbc_fn,crp_fn,mcq_fn,smq_fn,bmx_fn,diq_fn,pbc_fn,cot_fn)
{
  demo<-sasxport.get(paste("contdata/demo/",demo_fn,sep=""))
  demo<-demo[,c('seqn','riagendr','ridageyr')]
  
  biopro<-sasxport.get(paste("contdata/biopro/",biopro_fn,sep=""))
  
  cbc<-sasxport.get(paste("contdata/cbc/",cbc_fn,sep=""))
  crp<-sasxport.get(paste("contdata/crp/",crp_fn,sep=""))
  
  mcq<-sasxport.get(paste("contdata/mcq/",mcq_fn,sep=""))
  mcq<-mcq[,c('seqn','mcq160b','mcq160e','mcq160f','mcq160g','mcq160k','mcq220')]
  
  smq<-sasxport.get(paste("contdata/smq/",smq_fn,sep=""))
  smq<-smq[,c('seqn','smq020','smq040','smd055')]
  
  bmx<-sasxport.get(paste("contdata/bmx/",bmx_fn,sep=""))
  bmx<-bmx[,c('seqn','bmxbmi')]
  
  diq<-sasxport.get(paste("contdata/diq/",diq_fn,sep=""))
  diq<-diq[,c('seqn','diq010','diq050')]
  
  pbc<-sasxport.get(paste("contdata/pbc/",pbc_fn,sep=""))
  
  if (yr>2001){
  cot<-sasxport.get(paste("contdata/cot/",cot_fn,sep=""))
  }
  #general health condition was causing problems - so delete
  #hsq<-sasxport.get(paste("contdata/hsq/",hsq_fn,sep=""))
  #hsq<-hsq[,c('seqn','hsd010')]
  
  mort<- read.fwf(paste("contdata/mort/NHANES_",yr,"_",yr+1,"_MORT_2011_PUBLIC.dat",sep=""),widths=c(14,1,1,1,3,1,1,21,3,3,1,1,1,1,1))
  colnames(mort)<-c("seqn","eligstat","mortstat","causeavl","ucod_leading","diabetes","hyperten","nothing","permth_int"
                    ,"permth_exm","mortsrce_ndi","mortsrce_cms","mortsrce_ssa","mortsrce_dc","mortsrce_dcl")
  mort<-mort[-c(8)]
  mort$d1y<-0
  mort$d1y[mort$mortstat==1 & mort$permth_int<=12]<-1
  mort$d5y<-0
  mort$d5y[mort$mortstat==1 & mort$permth_int<=60]<-1
  mort$d10y<-0
  mort$d10y[mort$mortstat==1 & mort$permth_int<=120]<-1
  
  out<-merge(demo,merge(cbc,merge(biopro,crp,by='seqn'),by='seqn'),by='seqn')
  out<-merge(out,mort,by='seqn')
  out<-merge(out,merge(mcq,merge(smq,merge(bmx,diq,by='seqn'),by='seqn'),by='seqn'),by='seqn')
  out<-merge(out,pbc,by='seqn')
  if (yr>2001){
  out<-merge(out,cot,by='seqn')}
  
  #out<-merge(out,hsq,by='seqn')
  out$src=as.character(yr)
  return(out)
}

a<-getcont(1999,"DEMO.XPT","LAB18.XPT","LAB25.XPT","LAB11.XPT","MCQ.XPT","SMQ.XPT","BMX.XPT","DIQ.XPT","LAB06.XPT","LAB06.XPT")
b<-getcont(2001,"DEMO_B.XPT","L40_B.XPT","L25_B.XPT","L11_B.XPT","MCQ_B.XPT","SMQ_B.XPT","BMX_B.XPT","DIQ_B.XPT","L06_B.XPT","L06_B.XPT")
b$lbxsldsi=b$lbdsldsi
c<-getcont(2003,"DEMO_C.XPT","L40_C.XPT","L25_C.XPT","L11_C.XPT","MCQ_C.XPT","SMQ_C.XPT","BMX_C.XPT","DIQ_C.XPT","L06BMT_C.XPT","L06COT_C.XPT")
d<-getcont(2005,"DEMO_D.XPT","BIOPRO_D.XPT","CBC_D.XPT","CRP_D.XPT","MCQ_D.XPT","SMQ_D.XPT","BMX_D.XPT","DIQ_D.XPT","PBCD_D.XPT","COT_D.XPT")
e<-getcont(2007,"DEMO_E.XPT","BIOPRO_E.XPT","CBC_E.XPT","CRP_E.XPT","MCQ_E.XPT","SMQ_E.XPT","BMX_E.XPT","DIQ_E.XPT","PBCD_E.XPT","COTNAL_E.XPT")
f<-getcont(2009,"DEMO_F.XPT","BIOPRO_F.XPT","CBC_F.XPT","CRP_F.XPT","MCQ_F.XPT","SMQ_F.XPT","BMX_F.XPT","DIQ_F.XPT","PBCD_F.XPT","COTNAL_F.XPT")

nh3vars<-c('seqn',
           'src',
           'sex',
           'age',
           'fup',
           'mortstat',
           'ucod_leading',
           'eligstat',
           'd1y',
           'd5y',
           'd10y',
           'bmi',
           'hasheartfailure',
           'hasstroke',
           'hasbronchitis',
           'hasemphysema',
           'hascancer',
           'hasdiabetes',
           'diabetestype',
           'hasheartattack',
           'smoked100',
           'smokerstatus',
           'smokedlastage',
           'wcpsi',
           'rcpsi',
           'lmppcnt',
           'mopdif',
           'pvpsi',
           'rwp',
           'crp',
           'plpsi',
           'chpsi',
           'cepsi',
           'trpsi',
           'gbpsi',
           'hgp',
           'mvpsi',
           'pbp',
           'cop',
           'scp',
           'uap',
           'bup',
           'tpp',
           'amp',
           'ldpsi',
           'sgp'
           
)

nhcvars<-c('seqn',
           'src',
           'riagendr',
           'ridageyr',
           'permth_int',
           'mortstat',
           'ucod_leading',
           'eligstat',
           'd1y',
           'd5y',
           'd10y',
           'bmxbmi',
           'mcq160b',
           'mcq160f',
           'mcq160k',
           'mcq160g',
           'mcq220',
           'diq010',
           'diq050',
           'mcq160e',
           'smq020',
           'smq040',
           'smd055',
           'lbxwbcsi',
           'lbxrbcsi',
           'lbxlypct',
           'lbxmopct',
           'lbxmpsi',
           'lbxrdw',
           'lbxcrp',
           'lbxpltsi',
           'lbdschsi',
           'lbdscrsi',
           'lbdstrsi',
           'lbdsgbsi',
           'lbxhgb',
           'lbxmcvsi',
           'lbxbpb',
           'lbxcot',
           'lbxsca',
           'lbxsua',
           'lbxsbu',
           'lbxstp',
           'lbxsal',
           'lbxsldsi',
           'lbxsgl'
           )

a<-a[,nhcvars]
b<-b[,nhcvars]
c<-c[,nhcvars]
d<-d[,nhcvars]
e<-e[,nhcvars]
f<-f[,nhcvars]

labmort<-bind_rows(a,b,c,d,e,f)



#Smoker status cleaning
#note that nhac smokerstatus is 1=everyday, 2=not everyday, 3= not at all
table(labmort$smq040,labmort$src)
ss<-ifelse(labmort$src=="nha3",labmort$smq040,ifelse(labmort$smq040==3,2,ifelse(labmort$smq040==2,1,labmort$smq040)))
#missing smoker status = smoked fewer than 100 - so count as NS
ss[is.na(ss)]<-2
#Update smokerstatus with EX, SM and NS
labmort$smq040<-as.factor(ifelse(ss==1,"SM",ifelse(ss==2 & labmort$smq020==1,"EX","NS")))
rm(ss)

#Indicator variable cleaning

labmort$mcq160k=(ifelse(labmort$mcq160k==1,1,0))
labmort$mcq160f=(ifelse(labmort$mcq160f==1,1,0))
labmort$mcq160b=(ifelse(labmort$mcq160b==1,1,0))
labmort$mcq160g=(ifelse(labmort$mcq160g==1,1,0))
labmort$mcq220=(ifelse(labmort$mcq220==1,1,0))
labmort$mcq160e=(ifelse(labmort$mcq160e==1,1,0))

labmort$diq010=(ifelse(labmort$diq010==1,1,0))
labmort$diq050=(ifelse(labmort$diq010==1,ifelse(labmort$diq050==1,"T1","T2"),"NO"))

labmort$riagendr<-as.factor(labmort$riagendr)
#labmort$d1y<-as.factor(labmort$d1y)
#labmort$d5y<-as.factor(labmort$d5y)
#labmort$d10y<-as.factor(labmort$d10y)


#Save the continuous file
saveRDS(labmort,"data/labmortNHAC.RDS")



labmort_nha3<-readRDS('data/labmortNHA3.rds')

f1=labmort_nha3[,nh3vars]
f2=labmort[,nhcvars]
names(f2)<-nh3vars
labmort_all<-bind_rows(f1,f2)


#set up the impairment count variable
nimp=labmort_all$hascancer+labmort_all$hasemphysema+labmort_all$hasheartattack+labmort_all$hasheartfailure+labmort_all$hasstroke+labmort_all$hasdiabetes
nimp=ifelse(nimp>4,4,nimp)
table(nimp)
table(nimp,labmort_all$d5y,labmort_all$src)

table(nimp,labmort_all$age)

labmort_all$nimp<-nimp

#Save the combined file
saveRDS(labmort_all,"data/labmort_all.RDS")

write.csv(labmort_all,"data/labmort_all.csv")

####Checks on combined files####

#Lead looks a bit funny
aggregate(labmort_all$pbp,by=list(labmort_all$src,labmort_all$sex,labmort_all$sex),FUN=mean,na.rm=T)
hist(f1$pbp[f1$pbp<30])
hist(f2$pbp[f2$pbp<30])
aggregate(labmort_all$ldpsi,by=list(labmort_all$src,labmort_all$sex,labmort_all$sex),FUN=mean,na.rm=T)

#I checked documentation and both are ug/Dl
#This is probably real - lead exposures have gone down over time

na.mean<-function(y){return(mean(y,na.rm=T))}
plot_age<-function(v,ttl)
{
  toplot=aggregate(v,by=list(age=labmort_all$age,d5y=labmort_all$d5y),na.mean)
  ggplot(toplot,aes(x=age,y=x,colour=as.factor(d5y)))+geom_point()+ggtitle(ttl)
  #ggsave(paste(ttl,".pdf",sep=""),plot=last_plot(),path="plots")
}


plot_age(labmort_all$crp,"crp")
plot_age(labmort_all$wcpsi,"wcpsi")
plot_age(labmort_all$lmppcnt,"lmppcnt")
plot_age(labmort_all$chpsi,"chpsi")
plot_age(labmort_all$bmi,"bmi")
plot_age(labmort_all$smokerstatus,"smoker")


labmort_all.cc<-labmort_all[complete.cases(labmort_all),]

m1<-glm(d5y~hsageir+crp+wcpsi+chpsi+lmppcnt, family=binomial(link='logit'),data=labmort_all.cc)
summary(m1)

p<-ifelse(predict(m1,type="response")>0.5,1,0)

#Create a confusion dataframe
confus<-data.frame(actual=labmort_all.cc$d5y,pred=p)
tp=nrow(confus[confus$actual==1 & confus$pred==1,])
tn=nrow(confus[confus$actual==0 & confus$pred==0,])
fp=nrow(confus[confus$actual==0 & confus$pred==1,])
fn=nrow(confus[confus$actual==1 & confus$pred==0,])

m1.acc<-((tp+tn)/(tp+tn+fp+fn))
m1.sens<-(tp/(tp+fn))