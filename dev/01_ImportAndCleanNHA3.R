library(tidyverse)


#Read and clean the NHANES III data
setwd("~/Projects/Blood")

# read in the fixed-width format ASCII file
mort <- read_fwf(file="data/NHANES_III_MORT_2015_PUBLIC.dat",
                col_types = "ciiiiiiiddii",
                fwf_cols(publicid = c(1,14),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         dodqtr = c(22,22),
                         dodyear = c(23,26),
                         wgt_new = c(27,34),
                         sa_wgt_new = c(35,42),
                         fup = c(43,45),
                         permth_exm = c(46,48)
                ),
                na = "."
)

# create the ID (SEQN) for the NHANES surveys
mort$seqn <- as.numeric(substr(mort$publicid,1,5))
# NOTE:   SEQN is the unique ID for NHANES.

#Drop NHIS variables
mort <- select(mort, -publicid)
mort <- select(mort, -dodqtr)
mort <- select(mort, -dodyear)
mort <- select(mort, -wgt_new)
mort <- select(mort, -sa_wgt_new)







# mort<- read.fwf("data/NHANES_III_MORT_2015_PUBLIC.dat",widths=c(14,1,1,1,3,1,1,21,3,3,1,1,1,1,1))
# colnames(mort)<-c("seqn","eligstat","mortstat","causeavl","ucod_leading","diabetes","hyperten","nothing","fup"
#                   ,"permth_exm","mortsrce_ndi","mortsrce_cms","mortsrce_ssa","mortsrce_dc","mortsrce_dcl")
# 
# 
# mort<-mort[-c(8)]


exam<-read.fwf("data/exam.dat",widths=c(5,1518,4,4706))

#only interested in bmi - drop others
exam<-exam[c(1,3)]
colnames(exam)<-c('seqn','bmi')

table(exam$bmi)
exam$bmi[exam$bmi==8888]<-NA


adult<-read.fwf("data/adult.dat",widths=c(5,1445,1,1,1,2,3,4,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,1,1,1,1,3,1,2,3,4,1,3,3,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                            1,	2,	3,	3,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	2,	2,	2,	1,	1,	1,	1,	1,	1,	1,	3,	3,	3,	3,	3,	3,	1,	1,	1,	1,	1,	1,	1,	1,	1,	2,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	6,	3,	1,	1,	1,	2,	1,	2,	1,	1,	3,	1,	1,	1,	1,	3,	1,	2,	1,	3,	1,	3,	1,	3,	1,	2,	1,	1,	3,	1,	1,	1,	1,	1,	1,	1,	3,	1,	3,	1,	1,	3,	3,	3,	1,	1,	1,	3,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	3,	3,	3,	1,	1,	1,	1,	3,	3,	1,	3,	3,	3,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	3,	4,	1,	2,	3,	3,	3,	3,	3,	3,	3,	4,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	4,	3,	4,	3,	4,	4,	4,	3,	3,	3,	3,	3,	3,	1,	2,	4,	2,	4,	2,	3,	2,	3,	2,	3,	2,	3,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	2,	2,	2,	2,	2,	1,	1,	1,	2,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	5,	2,	1,	1,	1,	1,	2,	1,	3,	1,	3,	3,	1,	4,	3,	1,	3,	3,	3,	1,	1,	3,	1,	1,	3,	3,	1,	1,	3,	1,	1,	1,	1,	2,	1,	1,	2,	1,	1,	1,	1,	1,	2,	2,	2,	4,	1,	2,	1,	1,	1,	1,	1,	1,	2,	3,	1,	2,	1,	2,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	2,	4,	3,	1,	1,	4,	1,	1,	4,	1,	1,	4,	1,	1,	4,	1,	3,	4,	1,	3,	4,	1,	1,	4,	1,	1,	4,	1,	5,	4,	4,	5,
                                            3,	4,	5,	3,	4,	5,	3,	4,	1,	1,	1,	1,	4,	5,	5,	4,	1,	4,	4,	2,	1,	1,	2,	1,	3,	1,	1,	1,	2,	2,	2,	2,	2,	4,	4,	2,	2,	4,	4,	2,	2,	4,	4,	2,	2,	4,	4,	2,	2,	4,	4,	2,	2,	4,	4,	2,	2,	4,	4,	2,	2,	4,	4,	2,	2,	4,	4,	2,	2,	4,	4,	2,	2,	4,	4,	2,	2,	4,	4,	2,	2,	4,	4,	2,	2,	4,	4,	2,	2,	4,	4,	2,	2,	4,	4,	2,	2,	4,	4,	4,	4,	4,	6,	5,	4,	4,	4,	6,	5,	4,	4,	4,	6,	5,	4,	4,	4,	6,	5,	4,	4,	4,	6,	5,	4,	4,	4,	6,	5,	4,	4,	4,	6,	5,	4,	4,	4,	6,	5,	4,	4,	4,	6,	5,	4,	4,	4,	6,	5,	4,	4,	4,	6,	5,	4,	4,	4,	6,	5,	4,	4,	4,	6,	5,	4,	4,	4,	6,	5,	4,	4,	4,	6,	5,	4,	4,	4,	6,	5,	3,	4,	2,	2,	4,	3,	4,	2,	2,	4,	3,	4,	2,	2,	4,	3,	4,	2,	2,	4,	1,	3,	1,	3,	1,	3,	1,	2,	3,	1,	2,	3,	1,	2,	3,	1,	2,	3,	2,	2,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	30,	1,	1,	1,	1,	1,	1,	1,	1,	3,	3,	1,	3,	1,	3,	1,	3,	3,	1,	1,	3,	3,	1,	1,	3,	3,	1,	1,	3,	3,	1,	1,	2,	2,	3,	3,	3,	3,	1,	3,	2,	3,	2))

adult<-adult[-2]
adult<-adult[c(1,	2,	11,	12,	14,	15,	22,	23,	74,	79,	141,	542,	544,	552)]
colnames(adult)<-c('seqn',	'generalhealth',	'hasheartfailure',	'hasstroke',	'hasbronchitis',	'hasemphysema',	'hasskincancer',	'hasothercancer',	'hasdiabetes',	'hasinsulin',	'hasheartattack',	'smoked100',	'smokerstatus',	'smokedlastage')

adult$smokedlastage[adult$smokedlastage==999]<-NA



lab<-read.fwf("data/lab.dat",widths=c(5,	5,	1,	1,	1,	1,	1,	2,	1,	4,	2,	2,	3,	2,	1,	1,	6,	1,	1,	2,	1,	2,	1,	2,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	4,	1,	1,	1,	4,	1,	4,	1,	1,	1,	1,	1,	5,	1,	1,	5,	1,	5,	5,	5,	5,	5,	5,	5,	5,	4,	5,	4,	4,	5,	5,	5,	5,	5,	5,	5,	5,	5,	6,	5,	5,	5,	5,	3,	3,	2,	2,	2,	1,	1,	1,	1,	2,	2,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	4,	5,	4,	5,	3,	5,	4,	6,	4,	4,	4,	5,	5,	4,	6,	6,	8,	4,	6,	4,	4,	4,	4,	3,	4,	5,	6,	3,	4,	4,	5,	3,	4,	3,	4,	3,	4,	3,	4,	5,	3,	5,	4,	5,	3,	4,	3,	4,	3,	4,	3,	4,	3,	4,	5,	4,	4,	4,	5,	6,	1,	1,	2,	1,	1,	1,	1,	1,	5,	4,	5,	3,	5,	5,	1,	5,	4,	5,	2,	4,	5,	4,	5,	4,	5,	3,	5,	3,	5,	4,	6,	4,	6,	3,	4,	4,	6,	4,	6,	3,	3,	4,	4,	4,	4,	3,	3,	3,	3,	3,	3,	4,	1,	5,	6,	2,	3,	3,	5,	6,	5,	5,	6,	6,	6,	7,	1,	6,	7,	5,	6,	5,	4,	6,	7))
colnames(lab)<-c('seqn',	'dmpfseq',	'dmpstat',	'dmarethn',	'dmaracer',	'dmaethnr',	'sex',	'age',	'hsageu',	'hsaitmor',	'hsfsizer',	'hshsizer',	'dmpcntyr',	'dmpfipsr',	'dmpmetro',	'dmpcregn',	'dmppir',	'sdpphase',	'sdppsu6',	'sdpstra6',	'sdppsu1',	'sdpstra1',	'sdppsu2',	'sdpstra2',	'wtpfqx6',	'wtpfex6',	'wtpfhx6',	'wtpfalg6',	'wtpfcns6',	'wtpfsd6',	'wtpfmd6',	'wtpfhsd6',	'wtpfhmd6',	'wtpfqx1',	'wtpfex1',	'wtpfhx1',	'wtpfalg1',	'wtpfcns1',	'wtpfsd1',	'wtpfmd1',	'wtpfhsd1',	'wtpfhmd1',	'wtpfqx2',	'wtpfex2',	'wtpfhx2',	'wtpfalg2',	'wtpfcns2',	'wtpfsd2',	'wtpfmd2',	'wtpfhsd2',	'wtpfhmd2',	'wtpqrp1',	'wtpqrp2',	'wtpqrp3',	'wtpqrp4',	'wtpqrp5',	'wtpqrp6',	'wtpqrp7',	'wtpqrp8',	'wtpqrp9',	'wtpqrp10',	'wtpqrp11',	'wtpqrp12',	'wtpqrp13',	'wtpqrp14',	'wtpqrp15',	'wtpqrp16',	'wtpqrp17',	'wtpqrp18',	'wtpqrp19',	'wtpqrp20',	'wtpqrp21',	'wtpqrp22',	'wtpqrp23',	'wtpqrp24',	'wtpqrp25',	'wtpqrp26',	'wtpqrp27',	'wtpqrp28',	'wtpqrp29',	'wtpqrp30',	'wtpqrp31',	'wtpqrp32',	'wtpqrp33',	'wtpqrp34',	'wtpqrp35',	'wtpqrp36',	'wtpqrp37',	'wtpqrp38',	'wtpqrp39',	'wtpqrp40',	'wtpqrp41',	'wtpqrp42',	'wtpqrp43',	'wtpqrp44',	'wtpqrp45',	'wtpqrp46',	'wtpqrp47',	'wtpqrp48',	'wtpqrp49',	'wtpqrp50',	'wtpqrp51',	'wtpqrp52',	'wtpxrp1',	'wtpxrp2',	'wtpxrp3',	'wtpxrp4',	'wtpxrp5',	'wtpxrp6',	'wtpxrp7',	'wtpxrp8',	'wtpxrp9',	'wtpxrp10',	'wtpxrp11',	'wtpxrp12',	'wtpxrp13',	'wtpxrp14',	'wtpxrp15',	'wtpxrp16',	'wtpxrp17',	'wtpxrp18',	'wtpxrp19',	'wtpxrp20',	'wtpxrp21',	'wtpxrp22',	'wtpxrp23',	'wtpxrp24',	'wtpxrp25',	'wtpxrp26',	'wtpxrp27',	'wtpxrp28',	'wtpxrp29',	'wtpxrp30',	'wtpxrp31',	'wtpxrp32',	'wtpxrp33',	'wtpxrp34',	'wtpxrp35',	'wtpxrp36',	'wtpxrp37',	'wtpxrp38',	'wtpxrp39',	'wtpxrp40',	'wtpxrp41',	'wtpxrp42',	'wtpxrp43',	'wtpxrp44',	'wtpxrp45',	'wtpxrp46',	'wtpxrp47',	'wtpxrp48',	'wtpxrp49',	'wtpxrp50',	'wtpxrp51',	'wtpxrp52',	'hyaitmo',	'mxplang',	'mxpsessr',	'mxptidw',	'mxpaxtmr',	'hxptidw',	'hxpaxtmr',	'hxpsessr',	'phplang',	'phphemo',	'phpchm2',	'phpinsu',	'phpsnti',	'phpsnda',	'phpdrin',	'phpdrti',	'phpdrda',	'phpfast',	'phpbest',	'wcp',	'wcpsi',	'lmppcnt',	'moppcnt',	'grppcnt',	'lmp',	'mop',	'grp',	'rcp',	'rcpsi',	'hgp',	'hgpsi',	'htp',	'htpsi',	'mvpsi',	'mcpsi',	'mhp',	'mhpsi',	'rwp',	'rwpsi',	'plp',	'plpsi',	'dwp',	'pvpsi',	'grpdif',	'lmpdif',	'mopdif',	'eop',	'bop',	'blp',	'prp',	'mep',	'mlp',	'bap',	'lap',	'anp',	'bsp',	'hzp',	'pkp',	'pop',	'mrp',	'mip',	'sip',	'shp',	'ttp',	'txp',	'vup',	'pbp',	'pbpsi',	'epp',	'eppsi',	'fep',	'fepsi',	'tip',	'tipsi',	'pxp',	'frp',	'frpsi',	'fop',	'fopsi',	'rbp',	'rbpsi',	'vbp',	'vbpsi',	'vcp',	'vcpsi',	'icpsi',	'capsi',	'sep',	'sepsi',	'vap',	'vapsi',	'vep',	'vepsi',	'acp',	'acpsi',	'bcp',	'bcpsi',	'bxp',	'bxpsi',	'lup',	'lupsi',	'lyp',	'lypsi',	'rep',	'repsi',	'cop',	'tcp',	'tcpsi',	'tgp',	'tgpsi',	'lcp',	'lcpsi',	'hdp',	'hdpsi',	'aap',	'aapsi',	'abp',	'abpsi',	'lpp',	'lppsi',	'fhpsi',	'lhpsi',	'fbp',	'fbpsi',	'crp',	'tep',	'ahp',	'hbp',	'ssp',	'sap',	'hcp',	'dhp',	'h1p',	'h2p',	'rup',	'rupunit',	'vrp',	'top',	'rfp',	'l1p',	'hpp',	'napsi',	'skpsi',	'clpsi',	'c3psi',	'scp',	'scpsi',	'psp',	'pspsi',	'uap',	'uapsi',	'sgp',	'sgpsi',	'bup',	'bupsi',	'tbp',	'tbpsi',	'cep',	'cepsi',	'sfp',	'sfpsi',	'chp',	'chpsi',	'trp',	'trpsi',	'aspsi',	'atpsi',	'ggpsi',	'ldpsi',	'appsi',	'tpp',	'tppsi',	'amp',	'ampsi',	'gbp',	'gbpsi',	'ospsi',	'ghp',	'ghpmeth',	'g1p',	'g1psi',	'g1pcode',	'g1ptim1',	'g1ptim2',	'g2p',	'g2psi',	'c1p',	'c1psi',	'c2p',	'c2psi',	'i1p',	'i1psi',	'i1p2pflg',	'i2p',	'i2psi',	'udp',	'udpsi',	'urp',	'urpsi',	'ubp',	'uip')

#drop the weight variables and the variables cop and hpp - as these are replaced by lab2
lab<-lab[-c(25:155,261,296)]


lab2<-read.fwf("data/lab2.dat",widths=c(5,	5,	1,	1,	1,	1,	1,	2,	1,	4,	2,	2,	3,	2,	1,	1,	6,	1,	1,	2,	1,	2,	1,	2,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	9,	4,	1,	1,	1,	4,	1,	4,	1,	5,	4,	5,	5,	4,	5,	6,	6,	4,	4,	1,	1))
colnames(lab2)<-c('seqn',	'dmpfseq',	'dmpstat',	'dmarethn',	'dmaracer',	'dmaethnr',	'sex',	'age',	'hsageu',	'hsaitmor',	'hsfsizer',	'hshsizer',	'dmpcntyr',	'dmpfipsr',	'dmpmetro',	'dmpcregn',	'dmppir',	'sdpphase',	'sdppsu6',	'sdpstra6',	'sdppsu1',	'sdpstra1',	'sdppsu2',	'sdpstra2',	'wtpfqx6',	'wtpfex6',	'wtpfhx6',	'wtpfalg6',	'wtpfcns6',	'wtpfsd6',	'wtpfmd6',	'wtpfhsd6',	'wtpfhmd6',	'wtpfqx1',	'wtpfex1',	'wtpfhx1',	'wtpfalg1',	'wtpfcns1',	'wtpfsd1',	'wtpfmd1',	'wtpfhsd1',	'wtpfhmd1',	'wtpfqx2',	'wtpfex2',	'wtpfhx2',	'wtpfalg2',	'wtpfcns2',	'wtpfsd2',	'wtpfmd2',	'wtpfhsd2',	'wtpfhmd2',	'wtpqrp1',	'wtpqrp2',	'wtpqrp3',	'wtpqrp4',	'wtpqrp5',	'wtpqrp6',	'wtpqrp7',	'wtpqrp8',	'wtpqrp9',	'wtpqrp10',	'wtpqrp11',	'wtpqrp12',	'wtpqrp13',	'wtpqrp14',	'wtpqrp15',	'wtpqrp16',	'wtpqrp17',	'wtpqrp18',	'wtpqrp19',	'wtpqrp20',	'wtpqrp21',	'wtpqrp22',	'wtpqrp23',	'wtpqrp24',	'wtpqrp25',	'wtpqrp26',	'wtpqrp27',	'wtpqrp28',	'wtpqrp29',	'wtpqrp30',	'wtpqrp31',	'wtpqrp32',	'wtpqrp33',	'wtpqrp34',	'wtpqrp35',	'wtpqrp36',	'wtpqrp37',	'wtpqrp38',	'wtpqrp39',	'wtpqrp40',	'wtpqrp41',	'wtpqrp42',	'wtpqrp43',	'wtpqrp44',	'wtpqrp45',	'wtpqrp46',	'wtpqrp47',	'wtpqrp48',	'wtpqrp49',	'wtpqrp50',	'wtpqrp51',	'wtpqrp52',	'wtpxrp1',	'wtpxrp2',	'wtpxrp3',	'wtpxrp4',	'wtpxrp5',	'wtpxrp6',	'wtpxrp7',	'wtpxrp8',	'wtpxrp9',	'wtpxrp10',	'wtpxrp11',	'wtpxrp12',	'wtpxrp13',	'wtpxrp14',	'wtpxrp15',	'wtpxrp16',	'wtpxrp17',	'wtpxrp18',	'wtpxrp19',	'wtpxrp20',	'wtpxrp21',	'wtpxrp22',	'wtpxrp23',	'wtpxrp24',	'wtpxrp25',	'wtpxrp26',	'wtpxrp27',	'wtpxrp28',	'wtpxrp29',	'wtpxrp30',	'wtpxrp31',	'wtpxrp32',	'wtpxrp33',	'wtpxrp34',	'wtpxrp35',	'wtpxrp36',	'wtpxrp37',	'wtpxrp38',	'wtpxrp39',	'wtpxrp40',	'wtpxrp41',	'wtpxrp42',	'wtpxrp43',	'wtpxrp44',	'wtpxrp45',	'wtpxrp46',	'wtpxrp47',	'wtpxrp48',	'wtpxrp49',	'wtpxrp50',	'wtpxrp51',	'wtpxrp52',	'hyaitmo',	'mxplang',	'mxpsessr',	'mxptidw',	'mxpaxtmr',	'hxptidw',	'hxpaxtmr',	'hxpsessr',	'cop',	'hopsi',	'vdp',	'vdpsi',	't4p',	't4psi',	'thp',	'thpsi',	'tmp',	'tap',	'hpp',	'hppcag')

#drop the weight variables
lab2<-lab2[-c(2:163)]

#merge both lab files
labcombined<-merge(x=lab,y=lab2,by="seqn")


#merge with mortfile
labmort<-merge(x=labcombined,y=mort, by="seqn")

#merge with adult file - all the kids drop out
labmort<-merge(labmort,adult,by='seqn')
#merge with exam file
labmort<-merge(labmort,exam,by='seqn')

#labmort$fup<-as.numeric(labmort$fup)

labmort$d1y<-0
labmort$d1y[labmort$mortstat==1 & labmort$fup<=12]<-1
labmort$d5y<-0
labmort$d5y[labmort$mortstat==1 & labmort$fup<=60]<-1
labmort$d10y<-0
labmort$d10y[labmort$mortstat==1 & labmort$fup<=120]<-1


labmort$wcp[labmort$wcp==88888]<-NA
labmort$wcpsi[labmort$wcpsi==88888]<-NA
labmort$lmppcnt[labmort$lmppcnt==88888]<-NA
labmort$moppcnt[labmort$moppcnt==88888]<-NA
labmort$grppcnt[labmort$grppcnt==88888]<-NA
labmort$lmp[labmort$lmp==88888]<-NA
labmort$mop[labmort$mop==8888]<-NA
labmort$grp[labmort$grp==88888]<-NA
labmort$rcp[labmort$rcp==8888]<-NA
labmort$rcpsi[labmort$rcpsi==8888]<-NA
labmort$hgp[labmort$hgp==88888]<-NA
labmort$hgpsi[labmort$hgpsi==88888]<-NA
labmort$htp[labmort$htp==88888]<-NA
labmort$htpsi[labmort$htpsi==88888]<-NA
labmort$mvpsi[labmort$mvpsi==88888]<-NA
labmort$mcpsi[labmort$mcpsi==88888]<-NA
labmort$mhp[labmort$mhp==88888]<-NA
labmort$mhpsi[labmort$mhpsi==88888]<-NA
labmort$rwp[labmort$rwp==88888]<-NA
labmort$rwpsi[labmort$rwpsi==888888]<-NA
labmort$plp[labmort$plp==88888]<-NA
labmort$plpsi[labmort$plpsi==88888]<-NA
labmort$dwp[labmort$dwp==88888]<-NA
labmort$pvpsi[labmort$pvpsi==88888]<-NA
labmort$grpdif[labmort$grpdif==888]<-NA
labmort$lmpdif[labmort$lmpdif==888]<-NA
labmort$mopdif[labmort$mopdif==88]<-NA
labmort$eop[labmort$eop==88]<-NA
labmort$bop[labmort$bop==88]<-NA
labmort$blp[labmort$blp==8]<-NA
labmort$prp[labmort$prp==8]<-NA
labmort$mep[labmort$mep==8]<-NA
labmort$mlp[labmort$mlp==8]<-NA
labmort$bap[labmort$bap==88]<-NA
labmort$lap[labmort$lap==88]<-NA
labmort$anp[labmort$anp==8]<-NA
labmort$bsp[labmort$bsp==8]<-NA
labmort$hzp[labmort$hzp==8]<-NA
labmort$pkp[labmort$pkp==8]<-NA
labmort$pop[labmort$pop==8]<-NA
labmort$mrp[labmort$mrp==8]<-NA
labmort$mip[labmort$mip==8]<-NA
labmort$sip[labmort$sip==8]<-NA
labmort$shp[labmort$shp==8]<-NA
labmort$ttp[labmort$ttp==8]<-NA
labmort$txp[labmort$txp==8]<-NA
labmort$vup[labmort$vup==8]<-NA
labmort$pbp[labmort$pbp==8888]<-NA
labmort$pbpsi[labmort$pbpsi==88888]<-NA
labmort$epp[labmort$epp==8888]<-NA
labmort$eppsi[labmort$eppsi==88888]<-NA
labmort$fep[labmort$fep==888]<-NA
labmort$fepsi[labmort$fepsi==88888]<-NA
labmort$tip[labmort$tip==8888]<-NA
labmort$tipsi[labmort$tipsi==888888]<-NA
labmort$pxp[labmort$pxp==8888]<-NA
labmort$frp[labmort$frp==8888]<-NA
labmort$frpsi[labmort$frpsi==8888]<-NA
labmort$fop[labmort$fop==88888]<-NA
labmort$fopsi[labmort$fopsi==88888]<-NA
labmort$rbp[labmort$rbp==8888]<-NA
labmort$rbpsi[labmort$rbpsi==888888]<-NA
labmort$vbp[labmort$vbp==888888]<-NA
labmort$vbpsi[labmort$vbpsi==88888888]<-NA
labmort$vcp[labmort$vcp==8888]<-NA
labmort$vcpsi[labmort$vcpsi==888888]<-NA
labmort$icpsi[labmort$icpsi==8888]<-NA
labmort$capsi[labmort$capsi==8888]<-NA
labmort$sep[labmort$sep==8888]<-NA
labmort$sepsi[labmort$sepsi==8888]<-NA
labmort$vap[labmort$vap==888]<-NA
labmort$vapsi[labmort$vapsi==8888]<-NA
labmort$vep[labmort$vep==88888]<-NA
labmort$vepsi[labmort$vepsi==888888]<-NA
labmort$acp[labmort$acp==888]<-NA
labmort$acpsi[labmort$acpsi==8888]<-NA
labmort$bcp[labmort$bcp==8888]<-NA
labmort$bcpsi[labmort$bcpsi==88888]<-NA
labmort$bxp[labmort$bxp==888]<-NA
labmort$bxpsi[labmort$bxpsi==8888]<-NA
labmort$lup[labmort$lup==888]<-NA
labmort$lupsi[labmort$lupsi==8888]<-NA
labmort$lyp[labmort$lyp==888]<-NA
labmort$lypsi[labmort$lypsi==8888]<-NA
labmort$rep[labmort$rep==888]<-NA
labmort$repsi[labmort$repsi==8888]<-NA
labmort$cop[labmort$cop==88888]<-NA
labmort$tcp[labmort$tcp==888]<-NA
labmort$tcpsi[labmort$tcpsi==88888]<-NA
labmort$tgp[labmort$tgp==8888]<-NA
labmort$tgpsi[labmort$tgpsi==88888]<-NA
labmort$lcp[labmort$lcp==888]<-NA
labmort$lcpsi[labmort$lcpsi==8888]<-NA
labmort$hdp[labmort$hdp==888]<-NA
labmort$hdpsi[labmort$hdpsi==8888]<-NA
labmort$aap[labmort$aap==888]<-NA
labmort$aapsi[labmort$aapsi==8888]<-NA
labmort$abp[labmort$abp==888]<-NA
labmort$abpsi[labmort$abpsi==8888]<-NA
labmort$lpp[labmort$lpp==888]<-NA
labmort$lppsi[labmort$lppsi==8888]<-NA
labmort$fhpsi[labmort$fhpsi==88888]<-NA
labmort$lhpsi[labmort$lhpsi==8888]<-NA
labmort$fbp[labmort$fbp==8888]<-NA
labmort$fbpsi[labmort$fbpsi==8888]<-NA
labmort$crp[labmort$crp==88888]<-NA
labmort$tep[labmort$tep==888888]<-NA
labmort$ahp[labmort$ahp==8]<-NA
labmort$hbp[labmort$hbp==8]<-NA
labmort$ssp[labmort$ssp==88]<-NA
labmort$sap[labmort$sap==8]<-NA
labmort$hcp[labmort$hcp==8]<-NA
labmort$dhp[labmort$dhp==8]<-NA
labmort$h1p[labmort$h1p==8]<-NA
labmort$h2p[labmort$h2p==8]<-NA
labmort$rup[labmort$rup==88888]<-NA
labmort$rupunit[labmort$rupunit==8888]<-NA
labmort$vrp[labmort$vrp==88888]<-NA
labmort$top[labmort$top==888]<-NA
labmort$rfp[labmort$rfp==88888]<-NA
labmort$l1p[labmort$l1p==88888]<-NA
labmort$hpp[labmort$hpp==8]<-NA
labmort$napsi[labmort$napsi==88888]<-NA
labmort$skpsi[labmort$skpsi==8888]<-NA
labmort$clpsi[labmort$clpsi==88888]<-NA
labmort$c3psi[labmort$c3psi==88]<-NA
labmort$scp[labmort$scp==8888]<-NA
labmort$scpsi[labmort$scpsi==88888]<-NA
labmort$psp[labmort$psp==8888]<-NA
labmort$pspsi[labmort$pspsi==88888]<-NA
labmort$uap[labmort$uap==8888]<-NA
labmort$uapsi[labmort$uapsi==88888]<-NA
labmort$sgp[labmort$sgp==888]<-NA
labmort$sgpsi[labmort$sgpsi==88888]<-NA
labmort$bup[labmort$bup==888]<-NA
labmort$bupsi[labmort$bupsi==88888]<-NA
labmort$tbp[labmort$tbp==8888]<-NA
labmort$tbpsi[labmort$tbpsi==888888]<-NA
labmort$cep[labmort$cep==8888]<-NA
labmort$cepsi[labmort$cepsi==888888]<-NA
labmort$sfp[labmort$sfp==888]<-NA
labmort$sfpsi[labmort$sfpsi==8888]<-NA
labmort$chp[labmort$chp==8888]<-NA
labmort$chpsi[labmort$chpsi==888888]<-NA
labmort$trp[labmort$trp==8888]<-NA
labmort$trpsi[labmort$trpsi==888888]<-NA
labmort$aspsi[labmort$aspsi==888]<-NA
labmort$atpsi[labmort$atpsi==888]<-NA
labmort$ggpsi[labmort$ggpsi==8888]<-NA
labmort$ldpsi[labmort$ldpsi==8888]<-NA
labmort$appsi[labmort$appsi==8888]<-NA
labmort$tpp[labmort$tpp==8888]<-NA
labmort$tppsi[labmort$tppsi==888]<-NA
labmort$amp[labmort$amp==888]<-NA
labmort$ampsi[labmort$ampsi==888]<-NA
labmort$gbp[labmort$gbp==888]<-NA
labmort$gbpsi[labmort$gbpsi==888]<-NA
labmort$ospsi[labmort$ospsi==888]<-NA
labmort$ghp[labmort$ghp==8888]<-NA
labmort$ghpmeth[labmort$ghpmeth==8]<-NA
labmort$g1p[labmort$g1p==88888]<-NA
labmort$g1psi[labmort$g1psi==888888]<-NA
labmort$g1pcode[labmort$g1pcode==88]<-NA
labmort$g1ptim1[labmort$g1ptim1==888]<-NA
labmort$g1ptim2[labmort$g1ptim2==888]<-NA
labmort$g2p[labmort$g2p==88888]<-NA
labmort$g2psi[labmort$g2psi==888888]<-NA
labmort$c1p[labmort$c1p==88888]<-NA
labmort$c1psi[labmort$c1psi==88888]<-NA
labmort$c2p[labmort$c2p==888888]<-NA
labmort$c2psi[labmort$c2psi==888888]<-NA
labmort$i1p[labmort$i1p==888888]<-NA
labmort$i1psi[labmort$i1psi==8888888]<-NA
labmort$i1p2pflg[labmort$i1p2pflg==8]<-NA
labmort$i2p[labmort$i2p==888888]<-NA
labmort$i2psi[labmort$i2psi==8888888]<-NA
labmort$udp[labmort$udp==88888]<-NA
labmort$udpsi[labmort$udpsi==888888]<-NA
labmort$urp[labmort$urp==88888]<-NA
labmort$urpsi[labmort$urpsi==8888]<-NA
labmort$ubp[labmort$ubp==888888]<-NA
labmort$uip[labmort$uip==8888888]<-NA
labmort$hopsi[labmort$hopsi==8888]<-NA
labmort$vdp[labmort$vdp==88888]<-NA
labmort$vdpsi[labmort$vdpsi==88888]<-NA
labmort$t4p[labmort$t4p==8888]<-NA
labmort$t4psi[labmort$t4psi==88888]<-NA
labmort$thp[labmort$thp==888888]<-NA
labmort$thpsi[labmort$thpsi==888888]<-NA
labmort$tmp[labmort$tmp==8888]<-NA
labmort$tap[labmort$tap==8888]<-NA
labmort$hppcag[labmort$hppcag==8]<-NA

#labmort_nha3 splits skin and other cancer - I just want one cancer flag
labmort$hascancer<-ifelse(labmort$hasskincancer ==2 & labmort$hasothercancer ==2,2,1)

#create a src variable to record that this came from NHA3
labmort$src<-"nha3"

labmort$sex<-as.factor(labmort$sex)

#Smoker status cleaning
#note that nhac smokerstatus is 1=everyday, 2=not everyday, 3= not at all
table(labmort$smokerstatus,labmort$src)
ss<-ifelse(labmort$src=="nha3",labmort$smokerstatus,ifelse(labmort$smokerstatus==3,2,ifelse(labmort$smokerstatus==2,1,labmort$smokerstatus)))
#missing smoker status = smoked fewer than 100 - so count as NS
ss[is.na(ss)]<-2
#Update smokerstatus with EX, SM and NS
labmort$smokerstatus<-as.factor(ifelse(ss==1,"SM",ifelse(ss==2 & labmort$smoked100==1,"EX","NS")))
rm(ss)

#NHA3: 1=Y, 2=N, 3=blank 9 =Unknown
#NHAC: 1=Y, 2=N, 3=borderline, 7=refused,9=unknown
labmort$hasbronchitis=(ifelse(labmort$hasbronchitis==1,1,0))
labmort$hasstroke=(ifelse(labmort$hasstroke==1,1,0))
labmort$hasheartfailure=(ifelse(labmort$hasheartfailure==1,1,0))
labmort$hasemphysema=(ifelse(labmort$hasemphysema==1,1,0))
labmort$hascancer=(ifelse(labmort$hascancer==1,1,0))
labmort$hasheartattack=(ifelse(labmort$hasheartattack==1,1,0))
#Diabetes coding: , 0=no diabetes 1=diabetic,no insulin, 2=diabetic insulin
labmort$hasdiabetes=(ifelse(labmort$hasdiabetes==1,1,0))
labmort$diabetestype=(ifelse(labmort$hasdiabetes==1,ifelse(labmort$hasinsulin==1,"T1","T2"),"NO"))

nimp=labmort$hascancer+labmort$hasemphysema+labmort$hasheartattack+labmort$hasheartfailure+labmort$hasstroke+labmort$hasdiabetes
nimp=ifelse(nimp>5,5,nimp)
table(nimp)
table(nimp,labmort$d5y)

table(nimp,labmort$age)



saveRDS(labmort,"data/labmortNHA3_2015.RDS")



