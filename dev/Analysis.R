setwd("C:/RNOneDrive/OneDrive/Documents/BBK/projects/blood/dev")

library(dplyr)
library(ggplot2)
library(randomForest)
library(nnet)
library(pROC)
library(caret)

#Read in imputed labmort_all
lm<-readRDS("../data/lmi.RDS")
#some tidying up
lm$bmi<-round(lm$bmi,1)
lm$f5y=as.factor(lm$d5y)

table(lm$src,lm$d1y)
table(lm$src,lm$d5y)
table(lm$src,lm$d10y)



#xvars<-c('wcpsi','rcpsi','lmppcnt','pvpsi','rwp','crp','plpsi','chpsi','cepsi','trpsi','gbpsi','hgp','mvpsi','pbp','cop','scp','uap','bup','tpp','amp','ldpsi','sgp')

xvars<-c('wcpsi','rcpsi','lmppcnt','pvpsi','rwp','crp','chpsi','cepsi','trpsi','mvpsi','pbp','cop','uap','bup','tpp','amp','ldpsi','sgp')

#impvars<-c('wcpsi_imp','rcpsi_imp','lmppcnt_imp','pvpsi_imp','rwp_imp','crp_imp','plpsi_imp','chpsi_imp','cepsi_imp','trpsi_imp','gbpsi_imp','hgp_imp','mvpsi_imp','pbp_imp','cop_imp','scp_imp','uap_imp','bup_imp','tpp_imp','amp_imp','ldpsi_imp','sgp_imp')

impvars<-c('wcpsi_imp','rcpsi_imp','lmppcnt_imp','pvpsi_imp','rwp_imp','crp_imp','chpsi_imp','cepsi_imp','trpsi_imp','mvpsi_imp','pbp_imp','cop_imp','uap_imp','bup_imp','tpp_imp','amp_imp','ldpsi_imp','sgp_imp')


prevars<-c('sex',	'age', 'd5y',	'f5y','bmi', 'nimp',	'smokerstatus')

#Full rename of the blood variables
lm<-lm[,c(prevars,impvars,xvars)]
names(lm)<-c('sex',	'age',	'd5y',	'f5y',	'bmi',	'nimp',	'smokerstatus','wbc_imp',	'rbc_imp',	'lmp_imp',	'mpv_imp',	'rdw_imp',	'crp_imp',	'chl_imp',	'cre_imp',	'tri_imp',	'mcv_imp',	'pbp_imp',	'cot_imp',	'sua_imp',	'bun_imp',	'pro_imp',	'alb_imp',	'ldh_imp',	'glu_imp','wbc',	'rbc',	'lmp',	'mpv',	'rdw',	'crp',	'chl',	'cre',	'tri',	'mcv',	'pbp',	'cot',	'sua',	'bun',	'pro',	'alb',	'ldh',	'glu')

for (i in c(26:43)){
  lq<-quantile(lm[,i],probs=c(0.025),na.rm=T)
  uq<-quantile(lm[,i],probs=c(0.975),na.rm=T)
  lm[,i+18]=(ifelse(lm[,i]>uq,1,ifelse(lm[,i]<lq,1,0)))
  colnames(lm)[i+18]=paste(colnames(lm)[i],"_hilo",sep="")
}

lm$tfail<-apply(lm[,54:61],1,sum)
lm$timpute<-apply(lm[,8:25],1,sum)
table(lm$timpute)
table(lm$tfail,lm$nimp==0)

#Complete cases = 34111
#Actually this is not strictly complete cases as there is some missingness in bmi, smoker status etc.
lmc<-lm[lm$timpute==0,]

#Count deaths
sum(lm$d5y)
sum(lmc$d5y)




#Set up cross validation folds##
set.seed(1397846)

folds<-vector()
rvec<-runif(nrow(lm))
for (i in 1:nrow(lm))
{if (rvec[i]<0.2) folds[i]=1
else if (rvec[i]<0.4) folds[i]=2
else if (rvec[i]<0.6) folds[i]=3
else if(rvec[i]<0.8) folds[i]=4
else folds[i]=5
}

table(folds)



#### M1: Blood age and sex ####



mf1<-"f5y~age+I(age^2)+sex+wcpsi+rcpsi+lmppcnt+pvpsi+rwp+crp+plpsi+chpsi+cepsi+trpsi+gbpsi+hgp+mvpsi+pbp+cop+scp+uap+bup+tpp+amp+ldpsi+sgp"

mf1="f5y~age+I(age^2)+sex+wcpsi+rcpsi+lmppcnt+pvpsi+rwp+crp+chpsi+cepsi+trpsi+mvpsi+pbp+cop+uap+bup+tpp+amp+ldpsi+sgp"
mf1="f5y~age+I(age^2)+sex+wbc	+rbc+	lmp+	mpv+	rdw+	crp+chl	+cre+	tri+	mcv+	pbp+	cot	+sua+	bun+	pro	+alb+	ldh+glu"



mf1glm="f5y ~ age + sex+ I(age^2) + rdw + I(rdw^2)  + wbc  + crp + I(crp^2) + cre + I(cre^2) + alb + I(alb^2) +rbc+lmp+mpv"



fit.M1<-function(ds,testpart) {
  #set up the cross validation folds
  set.seed(12487)
  
  train<-ds[folds!=testpart,]
  test<-ds[folds==testpart,]
  
  lm1<-glm(as.formula(mf1glm), family=binomial(link='logit'),data=train)
  train$glm1.p<-predict(lm1,train,type="response")
  test$glm1.p<-predict(lm1,test,type="response")
  
  nnet1<-nnet(as.formula(mf1), data=train,size=30,decay=0.01,maxit=1000)
  train$nn1.p<-predict(nnet1,train)
  test$nn1.p<-predict(nnet1,test)
  
  rf1<-randomForest(as.formula(mf1),data=train,ntree=100,na.action="na.omit")
  train$rf1.p<-predict(rf1,train,type='prob')[,2]
  test$rf1.p<-predict(rf1,test,type='prob')[,2]
  
  return(list(train,test,lm1,nnet1,rf1))
}

m11<-fit.M1(lm,1)
m12<-fit.M1(lm,2)
m13<-fit.M1(lm,3)
m14<-fit.M1(lm,4)
m15<-fit.M1(lm,5)

summary(m11[[3]])

varImp(m11[[5]])
varImpPlot(m11[[5]], main="M3: Variable importance")

rfAUC<-c(roc(m11[[2]]$d5y,m11[[2]]$rf1.p)$auc,roc(m12[[2]]$d5y,m12[[2]]$rf1.p)$auc,roc(m13[[2]]$d5y,m13[[2]]$rf1.p)$auc,roc(m14[[2]]$d5y,m14[[2]]$rf1.p)$auc,roc(m15[[2]]$d5y,m15[[2]]$rf1.p)$auc)
lmAUC<-c(roc(m11[[2]]$d5y,m11[[2]]$glm1.p)$auc,roc(m12[[2]]$d5y,m12[[2]]$glm1.p)$auc,roc(m13[[2]]$d5y,m13[[2]]$glm1.p)$auc,roc(m14[[2]]$d5y,m14[[2]]$glm1.p)$auc,roc(m15[[2]]$d5y,m15[[2]]$glm1.p)$auc)
nnAUC<-c(roc(m11[[2]]$d5y,m11[[2]]$nn1.p)$auc,roc(m12[[2]]$d5y,m12[[2]]$nn1.p)$auc,roc(m13[[2]]$d5y,m13[[2]]$nn1.p)$auc,roc(m14[[2]]$d5y,m14[[2]]$nn1.p)$auc,roc(m15[[2]]$d5y,m15[[2]]$nn1.p)$auc)
M1.Test.AUCSummary<-data.frame(rfAUC,lmAUC,nnAUC)

rfAUC<-c(roc(m11[[1]]$d5y,m11[[1]]$rf1.p)$auc,roc(m12[[1]]$d5y,m12[[1]]$rf1.p)$auc,roc(m13[[1]]$d5y,m13[[1]]$rf1.p)$auc,roc(m14[[1]]$d5y,m14[[1]]$rf1.p)$auc,roc(m15[[1]]$d5y,m15[[1]]$rf1.p)$auc)
lmAUC<-c(roc(m11[[1]]$d5y,m11[[1]]$glm1.p)$auc,roc(m12[[1]]$d5y,m12[[1]]$glm1.p)$auc,roc(m13[[1]]$d5y,m13[[1]]$glm1.p)$auc,roc(m14[[1]]$d5y,m14[[1]]$glm1.p)$auc,roc(m15[[1]]$d5y,m15[[1]]$glm1.p)$auc)
nnAUC<-c(roc(m11[[1]]$d5y,m11[[1]]$nn1.p)$auc,roc(m12[[1]]$d5y,m12[[1]]$nn1.p)$auc,roc(m13[[1]]$d5y,m13[[1]]$nn1.p)$auc,roc(m14[[1]]$d5y,m14[[1]]$nn1.p)$auc,roc(m15[[1]]$d5y,m15[[1]]$nn1.p)$auc)
M1.Train.AUCSummary<-data.frame(rfAUC,lmAUC,nnAUC)

write.csv(M1.Test.AUCSummary,'./outputs/M1TestAUCSummary.csv')
write.csv(M1.Train.AUCSummary,'./outputs/M1TrainAUCSummary.csv')


#And plot ROCs for 1st partition
r<-roc(m11[[2]]$d5y,m11[[2]]$rf1.p)
rdf<-data.frame(r$sensitivities, 1-r$specificities)
names(rdf)<-c('tpr','fpr')

r<-roc(m11[[2]]$d5y,m11[[2]]$glm1.p)
glmdf<-data.frame(r$sensitivities, 1-r$specificities)
names(glmdf)<-c('tpr','fpr')

r<-roc(m11[[2]]$d5y,m11[[2]]$nn1.p)
nndf<-data.frame(r$sensitivities, 1-r$specificities)
names(nndf)<-c('tpr','fpr')

ggplot()+
  geom_line(data=rdf,aes(x=fpr,y=tpr,col="m3: Random Forest"))+
  geom_line(data=glmdf,aes(x=fpr,y=tpr,col="m1: Log. Regression"))+
  geom_line(data=nndf,aes(x=fpr,y=tpr,col="m2: Neural Net"))+
  ggtitle("m1-3: ROC curves on first test partition")+
  xlab("False Positive Rate")+ylab("True Positive Rate")+
  scale_color_manual(values = c("m3: Random Forest"="red","m1: Log. Regression"="black","m2: Neural Net"="blue"))+labs(colour="Model")

ggsave("./outputs/m1-3ROC.png",plot=last_plot(),device='png',width=5,height=3,units="in")



#### M2: Blood only ####


mfbloodonly<-"f5y ~ wcpsi+rcpsi+lmppcnt+pvpsi+rwp+crp+plpsi+chpsi+cepsi+trpsi+gbpsi+hgp+mvpsi+pbp+cop+scp+uap+bup+tpp+amp+ldpsi+sgp"
mfbloodonly="f5y~wbc	+rbc+	lmp+	mpv+	rdw+	crp+chl	+cre+	tri+	mcv+	pbp+	cot	+sua+	bun+	pro	+alb+	ldh+glu"

mfagesexonly<-"f5y ~ age +I(age^2)+ sex"

mffull<-"f5y ~ age + sex+ I(age^2)+bmi+nimp +smokerstatus+nimp+wcpsi+rcpsi+lmppcnt+pvpsi+rwp+crp+plpsi+chpsi+cepsi+trpsi+gbpsi+hgp+mvpsi+pbp+cop+scp+uap+bup+tpp+amp+ldpsi+sgp"
mffull<-"f5y ~ age+I(age^2)++bmi+nimp +smokerstatus +sex+wbc	+rbc+	lmp+	mpv+	rdw+	crp+chl	+cre+	tri+	mcv+	pbp+	cot	+sua+	bun+	pro	+alb+	ldh+glu"


fit.M2<-function(ds,testpart) {
  #set up the cross validation folds
  set.seed(12487)
  
  train<-ds[folds!=testpart,]
  test<-ds[folds==testpart,]
  
  rf1<-randomForest(as.formula(mfbloodonly),data=train,ntree=100,na.action="na.omit")
  train$rf1.p<-predict(rf1,train,type='prob')[,2]
  test$rf1.p<-predict(rf1,test,type='prob')[,2]
  
  rf2<-randomForest(as.formula(mfagesexonly),data=train,ntree=100,na.action="na.omit")
  train$rf2.p<-predict(rf2,train,type='prob')[,2]
  test$rf2.p<-predict(rf2,test,type='prob')[,2]
  
  rf3<-randomForest(as.formula(mffull),data=train,ntree=100,na.action="na.omit")
  train$rf3.p<-predict(rf3,train,type='prob')[,2]
  test$rf3.p<-predict(rf3,test,type='prob')[,2]
  
  return(list(train,test,rf1,rf2,rf3))
}

m21<-fit.M2(lm,1)
m22<-fit.M2(lm,2)
m23<-fit.M2(lm,3)
m24<-fit.M2(lm,4)
m25<-fit.M2(lm,5)

varImp(m11[[3]])
varImpPlot(m21[[3]], main="M3.1: Variable importance")
varImpPlot(m21[[5]], main="M3.2: Variable importance")

rf1AUC<-c(roc(m21[[2]]$d5y,m21[[2]]$rf1.p)$auc,roc(m22[[2]]$d5y,m22[[2]]$rf1.p)$auc,roc(m23[[2]]$d5y,m23[[2]]$rf1.p)$auc,roc(m24[[2]]$d5y,m24[[2]]$rf1.p)$auc,roc(m25[[2]]$d5y,m25[[2]]$rf1.p)$auc)
rf2AUC<-c(roc(m21[[2]]$d5y,m21[[2]]$rf2.p)$auc,roc(m22[[2]]$d5y,m22[[2]]$rf2.p)$auc,roc(m23[[2]]$d5y,m23[[2]]$rf2.p)$auc,roc(m24[[2]]$d5y,m24[[2]]$rf2.p)$auc,roc(m25[[2]]$d5y,m25[[2]]$rf2.p)$auc)
rf3AUC<-c(roc(m21[[2]]$d5y,m21[[2]]$rf3.p)$auc,roc(m22[[2]]$d5y,m22[[2]]$rf3.p)$auc,roc(m23[[2]]$d5y,m23[[2]]$rf3.p)$auc,roc(m24[[2]]$d5y,m24[[2]]$rf3.p)$auc,roc(m25[[2]]$d5y,m25[[2]]$rf3.p)$auc)

M2.Test.AUCSummary<-data.frame(rf1AUC,rf2AUC,rf3AUC)

rf1AUC<-c(roc(m21[[1]]$d5y,m21[[1]]$rf1.p)$auc,roc(m22[[1]]$d5y,m22[[1]]$rf1.p)$auc,roc(m23[[1]]$d5y,m23[[1]]$rf1.p)$auc,roc(m24[[1]]$d5y,m24[[1]]$rf1.p)$auc,roc(m25[[1]]$d5y,m25[[1]]$rf1.p)$auc)
rf2AUC<-c(roc(m21[[1]]$d5y,m21[[1]]$rf2.p)$auc,roc(m22[[1]]$d5y,m22[[1]]$rf2.p)$auc,roc(m23[[1]]$d5y,m23[[1]]$rf2.p)$auc,roc(m24[[1]]$d5y,m24[[1]]$rf2.p)$auc,roc(m25[[1]]$d5y,m25[[1]]$rf2.p)$auc)
rf3AUC<-c(roc(m21[[1]]$d5y,m21[[1]]$rf3.p)$auc,roc(m22[[1]]$d5y,m22[[1]]$rf3.p)$auc,roc(m23[[1]]$d5y,m23[[1]]$rf3.p)$auc,roc(m24[[1]]$d5y,m24[[1]]$rf3.p)$auc,roc(m25[[1]]$d5y,m25[[1]]$rf3.p)$auc)

M2.Train.AUCSummary<-data.frame(rf1AUC,rf2AUC,rf3AUC)

write.csv(M2.Test.AUCSummary,'./outputs/M2TestAUCSummary.csv')
write.csv(M2.Train.AUCSummary,'./outputs/M2TrainAUCSummary.csv')



#Model 1 (from previous model - age+sex+blood)
r<-roc(m11[[2]]$d5y,m11[[2]]$rf1.p)
rdf0<-data.frame(r$sensitivities, 1-r$specificities)
names(rdf0)<-c('tpr','fpr')

#Model 2 
r<-roc(m21[[2]]$d5y,m21[[2]]$rf1.p)
rdf1<-data.frame(r$sensitivities, 1-r$specificities)
names(rdf1)<-c('tpr','fpr')

#Age and sex only - this is rubbish don't plot
r<-roc(m21[[2]]$d5y,m21[[2]]$rf2.p)
rdf2<-data.frame(r$sensitivities, 1-r$specificities)
names(rdf2)<-c('tpr','fpr')

r<-roc(m21[[2]]$d5y,m21[[2]]$rf3.p)
rdf3<-data.frame(r$sensitivities, 1-r$specificities)
names(rdf3)<-c('tpr','fpr')


ggplot()+
  geom_line(data=rdf0,aes(x=fpr,y=tpr,col="m3: Blood, age, sex"))+
  geom_line(data=rdf1,aes(x=fpr,y=tpr,col="m3.1: Blood only"))+
  geom_line(data=rdf3,aes(x=fpr,y=tpr,col="m3.2: Full model"))+
  ggtitle("M2: ROC curves on first test partition")+
  xlab("False Positive Rate")+ylab("True Positive Rate")+
  scale_color_manual(values = c("m3: Blood, age, sex"="red","m3.1: Blood only"="black","m3.2: Full model"="grey"))+labs(colour="Model")

ggsave("./outputs/M2ROC.png",plot=last_plot(),device='png',width=5,height=3,units="in")


#### Comparison against Reference range approach####
#Create a general test performance set


d5y=c(m21[[2]]$d5y,m22[[2]]$d5y,m23[[2]]$d5y,m24[[2]]$d5y,m25[[2]]$d5y)
p=c(m21[[2]]$rf1.p,m22[[2]]$rf1.p,m23[[2]]$rf1.p,m24[[2]]$rf1.p,m25[[2]]$rf1.p)
tf<-c(m21[[2]]$tfail,m22[[2]]$tfail,m23[[2]]$tfail,m24[[2]]$tfail,m25[[2]]$tfail)

##Performance of 1, 2, 3 tfail
tfail1=ifelse(tf>0,1,0)
tfail2=ifelse(tf>1,1,0)
tfail3=ifelse(tf>2,1,0)

pr.1<-ifelse(p>0.2,1,0)
pr.3<-ifelse(p>0.3,1,0)
pr.5<-ifelse(p>0.4,1,0)

cnt<-rep(1,NROW(pr.1))

s<-aggregate(cnt~pr.1+pr.3+pr.5+tfail1+tfail2+tfail3+d5y,FUN='sum')

write.csv(s,'./outputs/rr_comparison.csv')




###Some ROC curves just to hammer the point home

r<-roc(m21[[2]]$d5y,m21[[2]]$tfail)
rdfrr<-data.frame(r$sensitivities, 1-r$specificities)
names(rdfrr)<-c('tpr','fpr')


ggplot()+
  geom_line(data=rdf0,aes(x=fpr,y=tpr,col="m3: Blood, age, sex"))+
  geom_line(data=rdf1,aes(x=fpr,y=tpr,col="m3.1: Blood only"))+
  geom_line(data=rdfrr,aes(x=fpr,y=tpr,col="m4: Reference Ranges"))+
  ggtitle("ROC curves on first test partition")+
  xlab("False Positive Rate")+ylab("True Positive Rate")+
  scale_color_manual(values = c("m3: Blood, age, sex"="red","m3.1: Blood only"="black","m4: Reference Ranges"="grey"))+labs(colour="Model")

ggsave("./outputs/RefRangeROC.png",plot=last_plot(),device='png',width=5,height=3,units="in")

#What's the AUC of this?
r$auc



#### M3: Complete cases####
set.seed(12487)

folds<-vector()
rvec<-runif(nrow(lmc))
for (i in 1:nrow(lmc))
{if (rvec[i]<0.2) folds[i]=1
else if (rvec[i]<0.4) folds[i]=2
else if (rvec[i]<0.6) folds[i]=3
else if(rvec[i]<0.8) folds[i]=4
else folds[i]=5
}

table(folds)


fit.M3<-function(ds,testpart) {
  #set up the cross validation folds
  set.seed(12487)
  
  train<-ds[folds!=testpart,]
  test<-ds[folds==testpart,]
  
  rf1<-randomForest(as.formula(mfbloodonly),data=train,ntree=100,na.action="na.omit")
  train$rf1.p<-predict(rf1,train,type='prob')[,2]
  test$rf1.p<-predict(rf1,test,type='prob')[,2]
  
  return(list(train,test,rf1))
}

m31<-fit.M3(lmc,1)
m32<-fit.M3(lmc,2)
m33<-fit.M3(lmc,3)
m34<-fit.M3(lmc,4)
m35<-fit.M3(lmc,5)

varImpPlot(m31[[3]], main="M3.3: Variable importance")



rf1AUC<-c(roc(m31[[2]]$d5y,m31[[2]]$rf1.p)$auc,roc(m32[[2]]$d5y,m32[[2]]$rf1.p)$auc,roc(m33[[2]]$d5y,m33[[2]]$rf1.p)$auc,roc(m34[[2]]$d5y,m34[[2]]$rf1.p)$auc,roc(m35[[2]]$d5y,m35[[2]]$rf1.p)$auc)

M3.Test.AUCSummary<-data.frame(rf1AUC)

rf1AUC<-c(roc(m31[[1]]$d5y,m31[[1]]$rf1.p)$auc,roc(m32[[1]]$d5y,m32[[1]]$rf1.p)$auc,roc(m33[[1]]$d5y,m33[[1]]$rf1.p)$auc,roc(m34[[1]]$d5y,m34[[1]]$rf1.p)$auc,roc(m35[[1]]$d5y,m35[[1]]$rf1.p)$auc)

M3.Train.AUCSummary<-data.frame(rf1AUC)

write.csv(M3.Test.AUCSummary,'./outputs/M3TestAUCSummary.csv')
write.csv(M3.Train.AUCSummary,'./outputs/M3TrainAUCSummary.csv')





fit.models<-function(ds,testpart){
  
  ####Modelling####
  folds<-vector()
  set.seed(123)
  
  rvec<-runif(nrow(ds))
  
  for (i in 1:nrow(ds))
  {if (rvec[i]<0.2) folds[i]=1
  else if (rvec[i]<0.4) folds[i]=2
  else if (rvec[i]<0.6) folds[i]=3
  else if(rvec[i]<0.8) folds[i]=4
  else folds[i]=5
  }
  
  
  train<-ds[folds!=testpart,]
  test<-ds[folds==testpart,]
  
  lm1<-glm(as.formula(mf), family=binomial(link='logit'),data=train)
  train$glm1.p<-predict(lm1,train,type="response")
  test$glm1.p<-predict(lm1,test,type="response")
  
  lm2<-glm(as.formula(mfblood), family=binomial(link='logit'),data=train)
  train$glm2.p<-predict(lm2,train,type="response")
  test$glm2.p<-predict(lm2,test,type="response")
  
  nnet1<-nnet(as.formula(mf), data=train,size=30,decay=0.01,maxit=100)
  train$nn1.p<-predict(nnet1,train)
  test$nn1.p<-predict(nnet1,test)
  
  nnet2<-nnet(d5y~age+sex+smokerstatus, data=train,size=30,decay=0.01,maxit=100)
  train$nn2.p<-predict(nnet2,train)
  test$nn2.p<-predict(nnet2,test)
  
  nnet3<-nnet(as.formula(mfblood), data=train,size=30,decay=0.01,maxit=100)
  train$nn3.p<-predict(nnet3,train)
  test$nn3.p<-predict(nnet3,test)
  
  rf1<-randomForest(as.formula(mfblood),data=train,ntree=100,na.action="na.omit")
  train$rf1.p<-predict(rf1,train,type='prob')[,2]
  test$rf1.p<-predict(rf1,test,type='prob')[,2]
  
  return(list(train,test))
}

x1=fit.models(lmc,1)

write.csv(x1[[2]],"../data/fitted_complete.csv")

#What's the overlap between my model and tfail?

rpred=ifelse(x1[[2]]$rf1.p>0.3,1,0)
rrfail=ifelse(x1[[2]]$tfail>0,1,0)
died=x1[[2]]$d5y
table(rpred,rrfail,died)



hist(x1[[1]]$rf1.p)


mf="f5y ~ age + smokerstatus+nimp+ sex+rwp+wcpsi+crp";

rf1<-randomForest(as.formula(mf),data=lm,ntree=100,na.action="na.omit")

plot(smooth(roc(lm$d5y,predict(rf1,lm,type='prob')[,2])))


importance(rf1)
varImpPlot(rf1,type=2)


healthy<-lm[lm$nimp==0,]

rf2<-randomForest(as.formula(mfblood),data=healthy,ntree=100,na.action="na.omit")
importance(rf2)
varImpPlot(rf2,type=2)


rf1.p<-predict(rf1,lm,type='prob')[,2]

rf1r<-PlotROC(lm$d5y,rf1.p,100)
ggplot()+
  geom_line(data=rf1r,aes(x=fpr,y=tpr),col="grey")+
  ggtitle("ROC curves: TEST DATA")

out<-data.frame(rf1.p,lm$d5y)
write.csv(out,"../data/rfout.csv")


#Compute ROC curves
nnet1r<-PlotROC(x1[[2]]$d5y ,x1[[2]]$nn1.p, 100)
nnet2r<-PlotROC(x1[[2]]$d5y,x1[[2]]$nn2.p,100)
nnet3r<-PlotROC(x1[[2]]$d5y,x1[[2]]$nn3.p,100)
glm1r<-PlotROC(x1[[2]]$d5y,x1[[2]]$glm1.p,100)
glm2r<-PlotROC(x1[[2]]$d5y,x1[[2]]$glm2.p,100)
rf1r<-PlotROC(x1[[2]]$d5y,x1[[2]]$rf1.p,100)



#compare my ROC against built-in
ggplot()+
  geom_line(data=rdf,aes(x=fpr,y=tpr),col="pink")+
  geom_line(data=rf1r,aes(x=fpr,y=tpr),col="grey")+
  ggtitle("ROC curves: TEST DATA")


ggplot()+
  #geom_line(data=glm1r,aes(x=fpr,y=tpr),col="red")+
  geom_line(data=glm2r,aes(x=fpr,y=tpr),col="pink")+
  #geom_line(data=nnet1r,aes(x=fpr,y=tpr),col="blue")+
  #geom_line(data=nnet2r,aes(x=fpr,y=tpr),col="black")+
  geom_line(data=nnet3r,aes(x=fpr,y=tpr),col="green")+
  geom_line(data=rf1r,aes(x=fpr,y=tpr),col="grey")+
  ggtitle("ROC curves: TEST DATA")

nnet1r<-PlotROC(x1[[1]]$d5y ,x1[[1]]$nn1.p, 100)
nnet2r<-PlotROC(x1[[1]]$d5y,x1[[1]]$nn2.p,100)
nnet3r<-PlotROC(x1[[1]]$d5y,x1[[1]]$nn3.p,100)
glmr<-PlotROC(x1[[1]]$d5y,x1[[1]]$glm.p,100)
glm2r<-PlotROC(x1[[1]]$d5y,x1[[1]]$glm2.p,100)
rf1r<-PlotROC(x1[[1]]$d5y,x1[[1]]$rf1.p,100)

ggplot()+
  geom_line(data=glm1r,aes(x=fpr,y=tpr),col="red")+
  #geom_line(data=glm2r,aes(x=fpr,y=tpr),col="pink")+
  geom_line(data=nnet1r,aes(x=fpr,y=tpr),col="blue")+
  #geom_line(data=nnet2r,aes(x=fpr,y=tpr),col="black")+
  #geom_line(data=nnet3r,aes(x=fpr,y=tpr),col="green")+
  geom_line(data=rf1r,aes(x=fpr,y=tpr),col="grey")+
  ggtitle("ROC curves: TRAINING DATA")



####Using Caret ####

library(caret)


xvars<-c('wcpsi',
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
'sgp')

prevars<-c('sex',	'age', 'd5y',	'bmi', 'nimp',	'smokerstatus')

lm<-lm[,c(prevars,xvars)]

lm<-lm[complete.cases(lm),]

lm$f5y<-as.factor(lm$d5y)

inTraining <- createDataPartition(lm$d5y, p = .75, list = FALSE)
training <- lm[ inTraining,]
testing  <- lm[-inTraining,]


fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 5
  ## repeated ten times
  #repeats = 10
)


gbmFit1 <- train(as.formula(mfblood), data = training, 
                 method = "rpart", 
                 trControl = fitControl)

gbmFit1

tree<-rpart(as.formula(mfblood),data=training,cp=0.005989912)
plot(tree)
text(tree)