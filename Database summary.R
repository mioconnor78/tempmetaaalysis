###### CIEE group temperature trophic expt database
###### June 19 2014 #################################
###### code for manuscript #######################
###### Mary O'Connor ################################

#### first, open data and set up using the file 'database prep May 12 2014'

#### Objectives for this file:
#### 1. Characterize the database: number of studies, ecosystems, etc. 







###### database summary for JASM:

dat<-data.fmm

systems <- ddply(dat, .(System), summarize, length(unique(RefShort)))
systems <- ddply(dat, .(System), summarize, length(unique(Expt)))
method.r <- ddply(dat, .(Method), summarize, length(unique(RefShort)))
method.s <- ddply(dat, .(Method), summarize, length(unique(Expt)))
method.r <- ddply(dat, .(Method), summarize, length(unique(RefShort)))
method.s <- ddply(dat, .(Method), summarize, length(unique(Expt)))

#how many studies manipulated more than 2 temps?
ref.temps<-ddply(dat, .(RefShort, System, ExptA), summarize, length(unique(TempIncr)), .drop=TRUE)
names(ref.temps) <- c('RefShort', 'System', 'ExptA', 'Templevs')
ref.temps.r <- ddply(ref.temps, .(Templevs), summarize, length(unique(ExptA)))
ref.temps.s <- ddply(ref.temps, .(Templevs), summarize, length(unique(RefShort)))
cbind(ref.temps.r, ref.temps.s[,2]) -> temps.sum
names(temps.sum) <- c('Templevs', 'ExptA', 'Refs')
temps.sum 


#how many experiments added nutrients
ref.addT<-ddply(dat, .(ExptA ,RefShort, NutrientsAdded, StressorAdded), summarize, length(unique(TempIncr)), .drop=TRUE)
nT.r <- ddply(ref.addT, .(NutrientsAdded), summarize, length(unique(ExptA)))
nT.s <- ddply(ref.addT, .(NutrientsAdded), summarize, length(unique(RefShort)))
cbind(nT.r, nT.s[,2]) -> nT.sum
names(nT.sum) <- c('nT', 'ExptA', 'Refs')
nT.sum 
sT.r <- ddply(ref.addT, .(StressorAdded), summarize, length(unique(ExptA)))
sT.s <- ddply(ref.addT, .(StressorAdded), summarize, length(unique(RefShort)))
cbind(sT.r, sT.s[,2]) -> sT.sum
names(sT.sum) <- c('sT', 'ExptA', 'Refs')
sT.sum 

#range of max trophic levels
ref.mTL<-ddply(dat, .(RefShort, Expt, System, Method), summarize, max(MaxTrophicLev), .drop=TRUE)



hist(log(dat$BodySizeGen), breaks=40, col='gray40', xlab='log(Body Size)', cex.lab=1.5, cex.axis=1.5, main='log(Body Size)', cex.main=2)

ref.genTime<-ddply(dat, .(RefShort, ExptA, System, TrophicLev), summarize, min(SGenTime), .drop=TRUE)
ref.genTime
hist(log(ref.genTime$..1), breaks=40, col='gray40', cex.lab=1.5, cex.axis=1.5, xlab='log(Generation Time)', main='Generation Time', cex.main=2)




