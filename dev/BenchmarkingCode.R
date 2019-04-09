#R Benchmarking code
library(microbenchmark)

setwd("C:/RNOneDrive/OneDrive/Documents/BBK/projects/blood/dev")
lm<-readRDS("../data/lmi.RDS")

library(nnet)
set.seed(627836)
microbenchmark(
nnet<-nnet(d5y~age+sex+smokerstatus, data=lm,size=30,decay=0.01,maxit=100)
,times=10)


#Lenovo330s
#min       lq     mean   median       uq    max neval
#10.93664 12.05094 13.66772 12.92959 14.80713 18.444    10

#Dell T20
#min       lq     mean   median       uq      max neval
#10.95316 14.35526 15.86438 14.58599 17.44246 22.53212    10