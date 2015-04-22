###### CIEE group temperature trophic expt database
###### June 19 2014 #################################
###### code for manuscript #######################
###### Mary O'Connor ################################

#### first, open data and set up using the file 'database prep May 12 2014'

#### Objectives for this file:
#### 1. Characterize the database: number of studies, ecosystems, etc. 







###### database summary for JASM:

dat<-dat1

ref.Expt<-ddply(dat, .(RefShort, Expt, System, Method), summarize, max(MaxTrophicLev), .drop=TRUE)
ref.Expt
table(ref.Expt$System)
table(ref.Expt$Method)
table(ref.Expt$Method, ref.Expt$System)
length(unique(data.fat$RefShort))
length(unique(data.fat$Expt))

hist(log(dat$BodySizeGen), breaks=40, col='gray40', xlab='log(Body Size)', cex.lab=1.5, cex.axis=1.5, main='log(Body Size)', cex.main=2)

ref.genTime<-ddply(dat, .(RefShort, ExptA, System, TrophicLev), summarize, min(SGenTime), .drop=TRUE)
ref.genTime
hist(log(ref.genTime$..1), breaks=40, col='gray40', cex.lab=1.5, cex.axis=1.5, xlab='log(Generation Time)', main='Generation Time', cex.main=2)

#how many studies manipulated more than 2 temps?
ref.temps<-ddply(dat, .(RefShort, System, ExptA), summarize, length(unique(TempIncr)), .drop=TRUE)
#ref.temps
table(ref.temps$..1)

#how many experiments added nutrients
ref.addT<-ddply(dat, .(ExptA ,RefShort, NutrientsAdded, StressorAdded), summarize, length(unique(TempIncr)), .drop=TRUE)
table(ref.addT$NutrientsAdded)
table(ref.addT$StressorAdded)
head(ref.addT)
