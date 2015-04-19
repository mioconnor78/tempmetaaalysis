###### Multi-trophic temperature experiment Meta-analysis
###### April 16 2015 #################################
###### code for manuscript #######################
###### Mary O'Connor ################################


#### Objectives for this file: to test the following hypotheses

# Energetic constraint predictions: 
#E1)	temperature increases NPP
#E2)	temperature increases ER even more  #not enough data

# Temperature affects dynamics [for control treatments]:
# D1) Bcr
# D2) C:R Biomass
# D3) K
# D4) R*

# Temperature affects abundance [for control treatments]
# A1) Density
# A2) Biomass
# A3) Total Biomass
# A4) H/A ratio?

# Experimental conditions: stressors, nutrients, other





#### STEP 1: open data and set up using the file 'database prep May 12 2014'
# load data file
dat1 <- data.fat

# load file of studies that report productivity data
datp <- read.csv("productivityrows.csv")


#### ANALYSIS

# E1) temperaure increases NPP

# Visualizing data: 
trellis.device()
scatterplot(Productivity~TempMean | grpA, data=dat1, xlim=c(0, 35),  smoother=FALSE, by.groups=TRUE, legend.plot=FALSE, legend.coords="topleft", xlab='Mean Temperature', ylab='NPP', main='NPP',  cex.lab=1.5, cex.main=2, cex.axis=1.5, pch=rep(1:10, length(unique(datp$grpA))))

# notes:  add muren study

scatterplot(ER~TempMean | grpA, data=datp, xlim=c(0, 35),  smoother=FALSE, by.groups=TRUE, legend.plot=TRUE, legend.coords="topleft", xlab='Mean Temperature', ylab='ER', main='ER',  cex.lab=1.5, cex.main=2, cex.axis=1.5, pch=rep(1:10, length(unique(datp$grpA))))

# analyze it: write a model to test for how temperature effects NPP. predictors would be: MeanTemp, CP, StressorAdded, NutrientsAdded

m0 <- lme(Productivity ~ TempMean*ConsumerPresent + TempMean*StressorAdded + TempMean*NutrientsAdded, random=~TempIncr|ExptA, data=datp, method='ML', na.action=na.omit, control = list(maxIter = 200)) 

m1 <- lme(Productivity ~  TempMean*StressorAdded + TempMean*NutrientsAdded, random=~TempIncr|ExptA, data=datp, method='ML', na.action=na.omit, control = list(maxIter = 200)) 

m2 <- lme(Productivity ~  TempMean*StressorAdded, random=~TempIncr|ExptA, data=datp, method='ML', na.action=na.omit, control = list(maxIter = 200)) 

m3 <- lme(Productivity ~  TempMean*NutrientsAdded, random=~TempIncr|ExptA, data=datp, method='ML', na.action=na.omit, control = list(maxIter = 200)) 

m1a <- lme(Productivity ~ TempMean*ConsumerPresent + TempMean*StressorAdded + TempMean*NutrientsAdded, random=~1|ExptA, data=datp, method='ML', na.action=na.omit) 

m1b<-lm(Productivity ~ TempMean*ConsumerPresent + TempMean*StressorAdded + TempMean*NutrientsAdded, data=datp, na.action=na.omit) 

###### Comparing models ##############
######################################

# AICc using parameter estimation recommended by bolker et al on glmm.wikidot.com/faq
# run for each candidate model
q <- 2
K <- function(x) (length(fixef(x))) + (q*(q+1)/2) 
AICc.mem <- function(x) -2*as.numeric(logLik(x)) + 2*K(x)*(length(datp$Productivity)/(length(datp$Productivity)-K(x)-1))

AIC.sum <- as.data.frame(cbind(AICc.mem(m0), AICc.mem(m1), AICc.mem(m2), AICc.mem(m3)))
#names(AIC.sum) <- c('m0','m1',   'm2', 'm3', 'm4', 'm5', 'm6', 'm7', 'm1b')

#for fewer random effects
q <- 1
AIC.sum$m1a <- AICc.mem(m1a)
q <- 0
AIC.sum$mfb <- AIC(m1b)

AIC.sum

## so the full model is the best one, with random effects

# calculating weights and delta AIC
library(qpcR)
akaike.weights(AIC.sum)


### results for now: stressor added reverses effect of temp on PPY

# akaike.weights(AIC.sum)
$deltaAIC
[1]  6.303030  3.991099  0.000000  3.318565 33.738557 72.710160

$rel.LL
[1] 4.278725e-02 1.359390e-01 1.000000e+00 1.902754e-01 4.718080e-08 1.626258e-16

$weights
[1] 3.125435e-02 9.929787e-02 7.304593e-01 1.389885e-01 3.446366e-08 1.187915e-16

#> summary(m2)
Linear mixed-effects model fit by maximum likelihood
 Data: datp 
       AIC      BIC    logLik
  238.7946 252.5032 -111.3973

Random effects:
 Formula: ~TempIncr | ExptA
 Structure: General positive-definite, Log-Cholesky parametrization
            StdDev    Corr  
(Intercept) 7.2320999 (Intr)
TempIncr    0.5362497 0.707 
Residual    1.8945151       

Fixed effects: Productivity ~ TempMean * StressorAdded 
                             Value Std.Error DF    t-value p-value
(Intercept)                2.81038  3.107249 28  0.9044602  0.3735
TempMean                   0.32039  0.182060 28  1.7597985  0.0894
StressorAddedyes          35.16446 15.592451  9  2.2552235  0.0506
TempMean:StressorAddedyes -1.67976  0.860347 28 -1.9524260  0.0610
 Correlation: 
                          (Intr) TempMn StrssA
TempMean                  -0.724              
StressorAddedyes          -0.199  0.144       
TempMean:StressorAddedyes  0.153 -0.212 -0.876

Standardized Within-Group Residuals:
        Min          Q1         Med          Q3         Max 
-2.07985389 -0.17808345 -0.05793914  0.09451724  2.68653714 

Number of Observations: 41
Number of Groups: 11 
> 

##################################################################################
# Temperature affects dynamics [for control treatments]:
##################################################################################

#I've changed the control/experimental codes for the database, so I'm reruning the BCR fig. This now includes terrestrial systems, but only ones where the CR pairs are plant-herbivore. 

## selecting data for autotroph responses for studies with CFCs
dat<-dat1[(dat1$trophy=='A'),]

#Add a column coding each experiment based on whether it has a consumer free control. 
CFC <- ddply(dat[(dat$MaxTrophicLev=='1'),], .(RefShort, ExptA), summarize, mean(dat$logAbundTot.Auto))
CFC$cfc <- rep('Y', length(CFC[,1]))
dat.cfca <- merge(dat, CFC, all = TRUE)
dat.cfc <- dat.cfca[which(dat.cfca$cfc=='Y'),]

# Bcr = K / R*, so tot Auto from maxtrophlev = 1 / tot auto from maxtrophlev = 2
# so for each exptA, we want to est Bcr. then we could use match to paste it back in. 
Bcr1 <- ddply(dat.cfc, .(RefShort, ExptA, TempMean, MaxTrophicLev), summarize, length(unique(abundTot.Aut)))
Kb <- ddply(dat.cfc[(dat.cfc$MaxTrophicLev=='1'),], .(RefShort, ExptA, TempMean), summarize, mean(abundTot.Aut))
stressor <- ddply(dat.cfc[(dat.cfc$MaxTrophicLev=='1'),], .(RefShort, ExptA, TempMean), summarize, unique(StressorAdded))
R.star <- ddply(dat.cfc[(dat.cfc$MaxTrophicLev=='2'),], .(RefShort, ExptA, TempMean), summarize, mean(abundTot.Aut))
cfc.size <- ddply(dat.cfc[(dat.cfc$MaxTrophicLev=='1'),], .(RefShort, ExptA, TempMean), summarize, mean(BodySizeGen))
names(Kb) <- c('RefShort', 'ExptA', 'TempMean', 'K')
names(R.star) <- c('RefShort', 'ExptA', 'TempMean', 'R.star')
Kb$R.star <- R.star$R.star
Kb$Bcr <- Kb$K / Kb$R.star
Kb$size <- cfc.size[,4]
Kb$Stressor <- stressor[,4]
Kb$tempBcrID <- paste(Kb$ExptA, Kb$TempMean)

dat.cfc$tempBcrID <- paste(dat.cfc$ExptA, dat.cfc$TempMean)
dat.cfc$Bcr <- Kb$Bcr[match(dat.cfc$tempBcrID, Kb$tempBcrID)]


## plot by grpA (expt + max trophic lev)
trellis.device()
# squares = stressor applied
scatterplot((Bcr)~TempMean | ExptA, data=Kb, smoother=FALSE, by.groups=TRUE, legend.plot=FALSE, legend.coords="topright", cex.lab=2, xlab='Temperature Mean (C)', ylab='Bcr', main='Bcr, CFC expts', pch = c(19,19,19,15,19,15,19,19,19,19,19,15,19), cex = 2, col = (c(1, 'orange', 2,2,  3,3, 4,'lightblue', 'lightblue', 6,6, 8,8)), lwd = 3)

trellis.device()
# squares = stressor applied
scatterplot((Bcr)~TempMean | ExptA, data=dat.cfc, smoother=FALSE, by.groups=TRUE, legend.plot=TRUE, legend.coords="topright", cex.lab=2, xlab='Temperature Mean (C)', ylab='Bcr', main='', pch = c(19,19,19,22,19,22,19,19, 19,19,22,19), cex = 2, col = (c(1,1,2,2, 'orange', 3,3, 4,4, 'purple','purple', 6,6, 8,8, 'brown', 'brown', 'seagreen', 'seagreen', 'orange', 'orange', 'lightblue', 'lightblue')), lwd = 3, cex.axis = 1.5)

# July 15th evening: Ok, generally warming strengthens Bcr, but a) now models aren't all running and I'm too tired to figure it out, and b) did i have the symbols wrong on the last analysis?? cause now two of the 'stressor' studies are increasing with warming...??
# next steps: move on for now, but come back to this analysis, and consider including non-Autotrophic resources?




## create a subset of the database with control treatments
datc <- dat1[which(dat1$TrtLevelType=='control'),]


####################################################################################
### D1) Bcr: How does Bcr change with temperature? 
### Approach A: estimate K/R* for CFC studies
#####################################################################################

## selecting data for autotroph responses for studies with CFCs
dat<-datc[(datc$trophy=='A'),]

#Add a column coding each experiment based on whether it has a consumer free control. 
CFC <- ddply(dat[(dat$MaxTrophicLev=='1'),], .(RefShort, ExptA), summarize, mean(dat$logAbundTot.Auto))
CFC <- CFC[-12,]
CFC$cfc <- rep('Y', length(CFC[,1]))
dat.cfca <- merge(dat, CFC, all = TRUE)
dat.cfc <- dat.cfca[which(dat.cfca$cfc=='Y'),]

# Bcr = K / R*, so tot Auto from maxtrophlev = 1 / tot auto from maxtrophlev = 2
# so for each exptA, we want to est Bcr. then we could use match to paste it back in. 
Bcr1 <- ddply(dat.cfc, .(RefShort, ExptA, TempMean, MaxTrophicLev), summarize, length(unique(abundTot.Aut)))
Kb <- ddply(dat.cfc[(dat.cfc$MaxTrophicLev=='1'),], .(RefShort, ExptA, TempMean), summarize, mean(abundTot.Aut))
stressor <- ddply(dat.cfc[(dat.cfc$MaxTrophicLev=='1'),], .(RefShort, ExptA, TempMean), summarize, unique(StressorAdded))
R.star <- ddply(dat.cfc[(dat.cfc$MaxTrophicLev=='2'),], .(RefShort, ExptA, TempMean), summarize, mean(abundTot.Aut))
cfc.size <- ddply(dat.cfc[(dat.cfc$MaxTrophicLev=='1'),], .(RefShort, ExptA, TempMean), summarize, mean(BodySizeGen))
names(Kb) <- c('RefShort', 'ExptA', 'TempMean', 'K')
names(R.star) <- c('RefShort', 'ExptA', 'TempMean', 'R.star')
Kb$R.star <- R.star$R.star
Kb$Bcr <- Kb$K / Kb$R.star
Kb$size <- cfc.size[,4]
Kb$Stressor <- stressor[,4]
Kb$tempBcrID <- paste(Kb$ExptA, Kb$TempMean)


#remove Harley b/c it's wild
#Kb <- Kb[-which(Kb$RefShort == "Harley 2003"),]

dat.cfc$tempBcrID <- paste(dat.cfc$ExptA, dat.cfc$TempMean)
dat.cfc$Bcr <- Kb$Bcr[match(dat.cfc$tempBcrID, Kb$tempBcrID)]


## plot by grpA (expt + max trophic lev)
trellis.device()
# squares = + herbivores, circles = no herbs
scatterplot((Bcr)~TempMean | ExptA, data=Kb, smoother=FALSE, by.groups=TRUE, legend.plot=FALSE, legend.coords="topright", cex.lab=2, xlab='Temperature Mean (C)', ylab='Bcr', main='Bcr, CFC expts', pch = c(15,19,15,19,19,19,19,19,19,15,19), cex = 2, col = (c(1,1,2,2, 'orange', 3,3, 4,4, 'purple','purple', 6,6, 8,8, 'brown', 'brown', 'seagreen', 'seagreen', 'orange', 'orange', 'lightblue', 'lightblue')), lwd = 3)

trellis.device()
# squares = stressor applied
scatterplot((Bcr)~TempMean | ExptA, data=dat.cfc, smoother=FALSE, by.groups=TRUE, legend.plot=FALSE, legend.coords="topright", cex.lab=2, xlab='Temperature Mean (C)', ylab='Bcr', main='', pch = c(22,19,22,19,19,19,19,19,19,22,19), cex = 2, col = (c(1,1,2,2, 'orange', 3,3, 4,4, 'purple','purple', 6,6, 8,8, 'brown', 'brown', 'seagreen', 'seagreen', 'orange', 'orange', 'lightblue', 'lightblue')), lwd = 3, cex.axis = 1.5)



###### The set of models ##########
###################################

dat<-dat.cfc

m0 <- lme(Bcr ~ TempMean, random=~TempMean|ExptA, data=dat, method='ML', na.action=na.omit, control = list(maxIter = 200)) 

m1 <- lme(Bcr ~ TempMean*StressorAdded, random=~TempMean|ExptA, data=dat, method='ML', na.action=na.omit) 

#m2 <- lme(Bcr ~ TempMean*Method, random=~TempMean|ExptA, data=dat, method='ML', na.action=na.omit) 

m3 <- lme(Bcr ~ TempMean, random=~1|ExptA, data=dat, method='ML', na.action=na.omit) 

#m3 <- lme(Bcr ~ TempMean*Subsystem, random=~1|ExptA, data=dat, method='ML', na.action=na.omit) 


#dat<-Kb

m0 <- lme(Bcr ~ TempMean, random=~TempMean|ExptA, data=dat, method='ML', na.action=na.omit, control = list(maxIter = 200)) 

m1 <- lme(Bcr ~ TempMean*stressor, random=~TempMean|ExptA, data=dat, method='ML', na.action=na.omit) 

m2 <- lme(Bcr ~ TempMean*size, random=~TempMean|ExptA, data=dat, method='ML', na.action=na.omit) 

m3 <- lme(Bcr ~ TempMean, random=~1|ExptA, data=dat, method='ML', na.action=na.omit) 

m4 <- lm(Bcr ~ TempMean, data = dat, na.action = na.omit)

m5 <- lm(Bcr ~ TempMean*Stressor, data = dat, na.action = na.omit)

m6 <- lm(Bcr ~ TempMean*size, data = dat, na.action = na.omit)

### could present the stressor added model; but clarify which of these symbols is the stressor added treatment?
### also do this w/o zostera; to do that, you have to start over with database preparation and recalculate tot auto, etc. 



###### Comparing models ##############
######################################

# AICc using parameter estimation recommended by bolker et al on glmm.wikidot.com/faq
# run for each candidate model
q <- 2
K <- function(x) (length(fixef(x))) + (q*(q+1)/2) 
AICc.mem <- function(x) -2*as.numeric(logLik(x)) + 2*K(x)*(length(dat$logAbundTot.Auto)/(length(dat$logAbundTot.Auto)-K(x)-1))

AIC.sum <- as.data.frame(cbind(AICc.mem(m0),AICc.mem(m1)))  

AICc(m4), AICc(m5), AICc(m6)))   
#AICc.mem(m3), AICc.mem(m4), AICc.mem(m6), 
names(AIC.sum) <- c('m0','m1', 'm2')

#for fewer random effects
q <- 1
AIC.sum$m3 <- AICc.mem(m3)

AIC.sum

# calculating weights and delta AIC
library(qpcR)
akaike.weights(AIC.sum)







# D2) C:R Biomass
# D3) K
# D4) R*

# Temperature affects abundance [for control treatments]
# A1) Density
# A2) Biomass
# A3) Total Biomass
# A4) H/A ratio?

# Experimental conditions: stressors, nutrients, other
# testing for git
