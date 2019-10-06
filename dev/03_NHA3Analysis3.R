#This code performs an elastic net Cox regression on the variables in NHANES3 dataset
#in order to identify the best predictors of mortality.

nha3<-readRDS("data/labmortNHA3_2015.RDS")
nha3<-nha3[nha3$age>=20,]
nimp=nha3$hascancer+nha3$hasstroke+nha3$hasemphysema+nha3$hasheartattack+nha3$hasheartfailure+nha3$hasdiabetes
nha3$nimp=ifelse(nimp>4,4,nimp)
table(nha3$nimp)
prevars<-c('sex',	'age',	'fup', 'mortstat',	'bmi', 'nimp',	'smokerstatus')

#This is the non-SI version of this list - 95 candidate variables in total.
xvar_nodupe<-c('wcp','lmppcnt','moppcnt','grppcnt','rcp','hgp','htp','mvpsi','mcpsi',
               'mhp','rwp','plp','dwp','grpdif','lmpdif','mopdif','eop','bop','blp','prp','mep','mlp',
               'bap','lap', 'anp','bsp', 'hzp','pkp','pop','mrp','mip','sip','shp','ttp','txp','vup',
               'pbp','epp','fep','tip', 'pxp','frp','fop','rbp','vbp','vcp','icpsi','capsi','sep','vap',
               'vep', 'acp','bcp','bxp','lup','lyp','rep','cop','tcp','tgp', 'lcp','hdp','aap', 'abp',
               'lpp','fhpsi','lhpsi','fbp', 'crp','napsi','skpsi','clpsi','c3psi',
               'scp', 'psp','uap', 'sgp', 'bup','tbp','cep', 'sfp','chp','trp', 'aspsi', 'atpsi','ggpsi',
               'ldpsi','appsi', 'tpp', 'amp','gbp','ospsi')

# non-percentage wbc counts: 'lmp','mop','grp'
#extra vars excluded ,'hopsi','vdp','t4p', 'thp','tmp','tap','hppcag')
#urine variables removed 'udp','urp','ubp','uip',
#diab varaibles removed 'ghp','g1p','g1ptim1','g1ptim2', 'g2p','c1p','c2p','i1p','i2p',
#antibody removed 'tep','ahp','hbp','ssp','sap','hcp', 'dhp','h1p','h2p', 'rup','vrp','top','rfp','l1p','hpp',


na_count <-sapply(nha3[,xvar_nodupe], function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count$var<-rownames(na_count)
#write.csv(na_count,"outputs/na.count.csv")
#barplot(na_count$na_count)

#missingness threshold 10%
xvar_nodupe2<-na_count$var[na_count$na_count/17030 <0.1]
xvar_nodupe2

tomodel<-nha3[,c(prevars,xvar_nodupe2)]
tomodel.cc<-tomodel[complete.cases(tomodel),]
#Set up the stratification bands
tomodel.cc$strat=as.factor(floor(tomodel.cc$age/10))
tomodel.cc$strat[tomodel.cc$strat==9]=8
tomodel.cc$nimp<-as.factor(tomodel.cc$nimp)


library(survival)
library(survminer)
library(glmnet)

#stratified by 10yr ageband
#strat.cox<-coxph(Surv(fup,mortstat)~strata(strat)+ age+I(age^2)+sex+nimp+wcp, data=tomodel.cc)
#summary(strat.cox)

#weighted Cox
#wt.cox<-coxphw(Surv(fup,mortstat)~ age+I(age^2)+sex+nimp+wcp, data=tomodel.cc)
#summary(wt.cox)

#unstratified full model
res.cox<-coxph(Surv(fup,mortstat)~ age+I(age^2)+sex+bmi+I(bmi^2)+sex:bmi+nimp:sex+nimp:smokerstatus+wcp	+	lmppcnt	+	moppcnt	+	grppcnt	+	rcp	+	hgp	+	htp	+	mvpsi	+	mcpsi	+	mhp	+	rwp	+
                 plp	+	dwp	+	pbp	+	epp	+	fep	+	tip	+	pxp	+	acp	+	bcp	+	bxp	+	lup	+	lyp	+	rep	+	cop		+
                 frp	+	fop	+	rbp	+	capsi	+	sep	+	vap	+	vep	+	tcp	+	tgp	+	hdp	+	crp	+	napsi	+	skpsi	+	clpsi+		
                 c3psi	+	scp	+	psp	+	uap	+	sgp	+	bup	+	tbp	+	cep	+	chp	+	aspsi	+	atpsi	+	ldpsi	+	appsi	+	tpp	+	amp, data=tomodel.cc)


#unstratified full model with quadratic terms

res.cox<-coxph(Surv(fup,mortstat)~ age+I(age^2)+sex+bmi+I(bmi^2)+sex:bmi+nimp:sex+nimp:smokerstatus+wcp	+	lmppcnt	+	moppcnt	+	grppcnt	+	rcp	+	hgp	+	htp	+	mvpsi	+	mcpsi	+	mhp	+	rwp	+	
  plp	+	dwp	+	pbp	+	epp	+	fep	+	tip	+	pxp	+	acp	+	bcp	+	bxp	+	lup	+	lyp	+	rep	+	cop	+	
  frp	+	fop	+	rbp	+	capsi	+	sep	+	vap	+	vep	+	tcp	+	tgp	+	hdp	+	crp	+	napsi	+	skpsi	+	clpsi	+	
  c3psi	+	scp	+	psp	+	uap	+	sgp	+	bup	+	tbp	+	cep	+	chp	+	aspsi	+	atpsi	+	ldpsi	+	appsi	+	tpp	+	amp +
I(wcp^2)	+	I(lmppcnt^2)	+	I(moppcnt^2)	+	I(grppcnt^2)		+	I(rcp^2)	+	I(hgp^2)	+	I(htp^2)	+	I(mvpsi^2)	+	I(mcpsi^2)	+	I(mhp^2)	+	I(rwp^2)	+	
  I(plp^2)	+	I(dwp^2)	+	I(pbp^2)	+	I(epp^2)	+	I(fep^2)	+	I(tip^2)	+	I(pxp^2)	+	I(acp^2)	+	I(bcp^2)	+	I(bxp^2)	+	I(lup^2)	+	I(lyp^2)	+	I(rep^2)	+	I(cop^2)	+	
  I(frp^2)	+	I(fop^2)	+	I(rbp^2)	+	I(capsi^2)	+	I(sep^2)	+	I(vap^2)	+	I(vep^2)	+	I(tcp^2)	+	I(tgp^2)	+	I(hdp^2)	+	I(crp^2)	+	I(napsi^2)	+	I(skpsi^2)	+	I(clpsi^2)	+	
  I(c3psi^2)	+	I(scp^2)	+	I(psp^2)	+	I(uap^2)	+	I(sgp^2)	+	I(bup^2)	+	I(tbp^2)	+	I(cep^2)	+	I(chp^2)	+	I(aspsi^2)	+	I(atpsi^2)	+	I(ldpsi^2)	+	I(appsi^2)	+	I(tpp^2)	+	I(amp^2), data=tomodel.cc)


summary(res.cox)

yvar<-c("fup","mortstat")
y<-tomodel.cc[,yvar]
names(y)<-c("time","status")
y=data.matrix(y)

fit <- glmnet(model.matrix(res.cox),y , family="cox",  maxit = 10000,alpha=1)
plot(fit)
#summary(fit)

cv.fit <- cv.glmnet(model.matrix(res.cox),y , family="cox",  maxit = 10000,alpha=1)
plot(cv.fit)

#artifically increase lambda to get a comfortable number of predictors
c=coef(cv.fit,s=cv.fit$lambda.min+0.02)
c=coef(cv.fit,s=exp(-4))
e=rownames(c)
selvars<-e[c[,1]!=0]
selvars


####Final NHA3 model form ####

xvar_final<-c('wcp','pbp','sep','ldpsi','uap','rwp','acp','crp','appsi','bup','dwp','bxp','clpsi','amp','tpp','lyp','sgp','grppcnt','cop','cep','mvpsi')

tomodel.final<-nha3[,c(prevars,xvar_final)]
tomodel.final$strat=as.factor(floor(tomodel.final$age/10))
tomodel.final$strat[tomodel.final$strat==9]=8
tomodel.final$nimp<-as.factor(tomodel.final$nimp)

tomodel.final.cc<-tomodel.final[complete.cases(tomodel.final),]




sel.cox<-coxph(Surv(fup,mortstat)~ age+I(age^2) + sex + bmi + smokerstatus+ wcp +  rwp  +  dwp +
                 pbp  +    acp  +    bxp + lyp + cop+sep  +    crp   +        clpsi    +       sgp  +     cep+        
                 ldpsi  +    appsi   +   amp + I(grppcnt^2)+I(mvpsi^2)+I(uap^2)+ I(bup^2)+I(tpp^2)
               +grppcnt+mvpsi+uap+bup+tpp, data=tomodel.final.cc)

summary(sel.cox)

#Stratified model
sel.cox.strat<-coxph(Surv(fup,mortstat)~ strata(strat) + age+I(age^2) + sex + bmi + smokerstatus+ wcp +  rwp  +  dwp +
                 pbp  +    acp  +    bxp + lyp + cop+sep  +    crp   +        clpsi    +       sgp  +     cep+        
                 ldpsi  +    appsi   +   amp + I(grppcnt^2)+I(mvpsi^2)+I(uap^2)+ I(bup^2)+I(tpp^2)
               +grppcnt+mvpsi+uap+bup+tpp, data=tomodel.final.cc)

summary(sel.cox.strat)


#Also fit to healthy only to check that coefficient estimates are similar
healthy<-tomodel.final.cc[tomodel.final.cc$nimp==0,]
hsel.cox<-coxph(Surv(fup,mortstat)~ age+I(age^2) + sex + bmi + nimp + smokerstatus+ wcp +  rwp  +  dwp +
                  pbp  +    acp  +    bxp + lyp + cop+sep  +    crp   +        clpsi    +       sgp  +     cep+        
                  ldpsi  +    appsi   +   amp + I(grppcnt^2)+I(mvpsi^2)+I(uap^2)+ I(bup^2)+I(tpp^2)
                +grppcnt+mvpsi+uap+bup+tpp, data=healthy)
summary(hsel.cox)

#Also try fitting a baseline model without blood
base.cox<-coxph(Surv(fup,mortstat)~ age+I(age^2)+sex+nimp+nimp:smokerstatus, data=tomodel.final.cc)
summary(base.cox)

basicbase.cox<-coxph(Surv(fup,mortstat)~ age+I(age^2)+sex, data=tomodel.final.cc)
summary(basicbase.cox)

