###### Multi-trophic temperature experiment Meta-analysis
###### April 16 2015 #################################
###### code for manuscript #######################
###### rebuilding analysis for detecting basic effect sizes
###### Mary O'Connor ################################



###### ANALYSES ###########

## QUESTION 1. Can we predict the effect of temperature on autotrophic abundance?

## visualize data

#### 1a. Does the presence of consumers modify the effect of temprature on autotrophic abundance?
dat<-data.fmm
dat<-dat[((dat$Day=='Final')|(dat$Day=='Peak')|(dat$Day=='Averaged')),] # use final, peak or averaged dates

### visualizing total autotroph biomass with and without consumers, grouped by grpA, which is Expt x max TL
trellis.device()
scatterplot(logAbundTot.Auto~TempIncr | grpA, data=dat[(dat$MaxTrophicLev==1),], xlim=c(-7, 21), smoother=FALSE, by.groups=TRUE, legend.plot=TRUE, legend.coords="topright", cex.lab=.5, xlab='Temp', ylab='logAbundance', main='Total Autotrophs, No Consumers')

trellis.device()
scatterplot(logAbundTot.Auto~TempIncr | grpA, data=dat[(dat$MaxTrophicLev==2),], xlim=c(-7, 21),  smoother=FALSE, by.groups=TRUE, legend.plot=TRUE, legend.coords="topright", cex.lab=.5, xlab='Temp', ylab='logAbundance', main='Total Autotrophs, w/ herbivores')

# all total autotrophs  ###### RESULTS FIGURES 1 AND 2 FOR TALK
trellis.device()
scatterplot(logAbundTot.Auto~TempIncr | grpA, data=dat, xlim=c(-7, 21),  smoother=FALSE, by.groups=TRUE, legend.plot=FALSE, legend.coords="topright", xlab='Temperature Increase', ylab='logAbundance', main='Total Autotrophs',  cex.lab=1.5, cex.main=2, cex.axis=1.5, pch=rep(1:10, length(unique(dat$grpA))))

trellis.device()
scatterplot(logAbundTot.Auto~TempMean | grpA, data=dat, xlim=c(0, 30),  smoother=FALSE, by.groups=TRUE, legend.plot=FALSE, legend.coords="topright", xlab='Mean Temperature', ylab='logAbundance', main='Total Autotrophs',  cex.lab=1.5, cex.main=2, cex.axis=1.5, pch=rep(1:10, length(unique(dat$grpA))))

trellis.device()
scatterplot(logAbundTot.Auto~TempMean | grpA, data=dat, xlim=c(-7, 21),  smoother=FALSE, by.groups=TRUE, legend.plot=FALSE, legend.coords="topright", xlab='Mean Temperature', ylab='logAbundance', main='Total Autotrophs',  cex.lab=1.5, cex.main=2, cex.axis=1.5)

scatterplot(logAbundTot.Auto~TempIncr | grpA, data=dat[(dat$MaxTrophicLev=='2'),], xlim=c(-7, 21), smoother=FALSE, by.groups=TRUE, legend.plot=FALSE, legend.coords="topright", xlab='Temp', ylab='logBiomassTot', main='Total Autotrophs, with Herbs', cex.lab=2)

scatterplot(logAbundTot.Auto~TempIncr | grpA, data=dat[(dat$ConsumerPresent=='yes')&(dat$MaxTrophicLev>='3'),], smoother=FALSE, by.groups=TRUE, legend.plot=FALSE, xlab='Temp', ylab='logBiomassTot', main='Total Autotrophs, w/ 3 TL')

