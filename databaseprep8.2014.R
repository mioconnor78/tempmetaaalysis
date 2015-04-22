###### CIEE group temperature trophic expt database
###### April 2015, modified from Aug 2014 code ###############
###### analyses for manuscript #######################
###### Mary O'Connor ################################


#### in this file: 
#### Section 1. Load and process the data to be ready for analysis
#### Section 2. Visualize data

#### this file prepares the datafiles for analysis. I have saved it separately, because we now have different analyses going on.


### SECTION 1: LOAD AND PROCESS DATA

### load the data
library(reshape2)
library(plyr)
library(lattice)
library(ggplot2)
library(car)
library(nlme)

data1 <- read.csv("tempexptsjul162014.csv")   #tempexptsmay14 matches the excel sheet may192014; the jul file is modified, to include changes to 'expt' and control for alterberg adn eklof.
datp <- read.csv("productivityrows.csv")
sizes.master <- read.csv("sizemaster.csv")


### merge by a new species ID column, and add general body sizes
data1$spID<-paste(data1$TaxonGrp, data1$Taxon, data1$SpeciesName)
data1$BodySizeGen <- sizes.master$BodySizeG[match(data1$spID, sizes.master$spID)]

data <- data1

##### REMOVE STUDIES THAT DON'T MEET OUR CRITERIA
## remove Gillespie b/c just mean; variances were manipulated
data<-data[-which(data$RefShort=='Gillespie et al. 2012'),] #if you use this study, check aphid densities, they look weird.
## remove Liu et al; bloom is so severe that there are no clear final abundances; also no dynamics.
#data<-data[-which(data$RefShort=='Liu et al. 2011'),]
## remove Wu et al; only prey could respond, and not through reproduction, so it's not really consistent with our criteria.
data<-data[-which(data$RefShort=='Wu et al. 2011'),]
## studies that showed only mortality, so not really population level responses.
##data<-data[-which(data$RefShort=='Barton and Schmitz 2009'),]
##data<-data[-which(data$RefShort=='Ritchie 1996'),]
## studies that allowed some dispersal so conceivably species could be replaced. leaving this in for now because i can't get the command to work.
#data<-data[-which((data$RefShort=='Thompson and Shurin 2011')&((data$AddTrtType=='biotic')||(data$AddTrtType=='both'))),]
## remove experiments that only present prey responses; these aren't consistent with our criteria, and there are likely many other papers out there that fit this description
#data<-data[-which(data$RefShort=='Harmon et al. 2009'),]
data<-data[-which(data$RefShort=='Brauer et al 2009'),]
## remove Morellisen adn Harley b/c there is a procedural control trt in the data here, and we need to remove this treatment, which will involve subtracting values from th experimental.
#data<-data[-which(data$RefShort=='Morelissen and Harley 2007'),]
## remove Sjorgerston low herbivory treatment; it doesn't logically fit as a treatment.
#data<-data[-which((data$RefShort=='Sjogersten et al. 2008')&(data$TrtLevel=='low herbivory')),]

### studies that need to be checked
data<-data[-which(data$RefShort=='Chase 1996'),]  # check spider treatments, and temps. is there the right number of rows for each trophic level x temp combination? also check whether plant data is abs or relative.

## DATA PROCESSING
data$TempMean<-as.numeric(as.character(data$TempMean))
data$SGenTime<-as.numeric(as.character(data$SamplingDay_Gen))
data$Biomass <- as.numeric(as.character(data$Biomass))
data$Expt<-as.factor(data$Expt)

### get rid of long columns: 
data<-data[,-(75:74)]  ## get rid of blank and notes cols
#data<-data[,-(58)] # get rid of an NA col
data<-data[,-(5)] # get rid of long ref column 


### add columns ####
#add an algae ID and invT columns: 
data$microalgae<-ifelse((data$TaxonGrp=='microalgae'), 'yes', 'no')
data$microalgae<-ifelse((data$TaxonGrp=='phytoplankton'), 'yes', data$microalgae)
k <- 8.62*10^-5
data$invT<- 1/((k*(data$TempMean + 273)))
data$invT<- as.numeric(data$invT)
data$grpID<-paste(data$Expt, data$Taxon, data$ConsumerPresent, data$MaxTrophicLev, data$SpeciesName)

### create a column for abundance (density or biomass); this prioritizes biomass when both are measured
data$abundance<-ifelse(is.na(data$Biomass), data$Density, data$Biomass)
data$abundance.var<-ifelse(is.na(data$Biomass), data$DensitySD, data$BiomassSD)
data$abundance.type<-ifelse(is.na(data$Biomass), 'Density', 'Biomass')

## 

data[is.na(data$abundance),]
data[(data$abundance < 0),]

## create a column for minimum trophic level; oh, maybe not necessary.
minT <- ddply(data, .(ExptA, RefShort, MaxTrophicLev), summarize, min(TrophicLev))


### log transform response variables
data$logDensity<-log(data$Density)
data$Biomass.normalized1<-as.numeric(as.character(ifelse(data$Biomass.normalized=='0', 'NA', data$Biomass.normalized)))
data$logBiomass<-log(data$Biomass.normalized1)
data$logAbund<-log(data$abundance + 0.001)

## DATA SUBSETTING FOR ANALYSIS
### make dataframe of final and averaged for multi-temp studies
data.fa<-data[-which(data$Day=='Initial'),]
data.fa<-data.fa[-which(data.fa$Day=='Middle'),]
data.fa<-data.fa[-which(data.fa$Day=='Peak'),]

## CONSOLIDATE ROWS: TOTAL BIOMASS OF AUTOTROPHS, HETEROTROPHS AND TOTAL FOOD WEB
#### consolidate to get total biomass rows
pp.sum<-ddply(data.fa[(data.fa$trophy=='A'),], .(Expt, RefShort, TempIncr, ConsumerPresent, MaxTrophicLev, TrophicLev), summarize, sum(Biomass.normalized1)) #changed from Biomass on 11.15.13
pp.sum$Total<-(rep('TotalP', length(pp.sum[,1])))
names(pp.sum)<-c("Expt1", "RefShort1", "TempIncr1","ConsumerPresent1", "MaxTrophicLev1","autotrophs", "BiomassTot", 'Total')
pp.sum<-pp.sum[(which(pp.sum$BiomassTot>0)),]
pp.sum$logBiomassTot<-log(as.numeric(pp.sum$BiomassTot))
dim(pp.sum)

#having some trouble deciding whether to include the ConsumerPresent column here. i think no.
ht.sum<-ddply(data.fa[(data.fa$trophy=='H'),], .(Expt, RefShort, TempIncr, MaxTrophicLev), summarize, sum(Biomass))
ht.sum$Total<-(rep('TotalH', length(ht.sum[,1])))
names(ht.sum)<-c("Expt1", "RefShort1","TempIncr1",  "MaxTrophicLev1","BiomassTot", 'Total')
ht.sum<-ht.sum[(which(ht.sum$BiomassTot!='NA')),]
ht.sum<-ht.sum[(which(ht.sum$BiomassTot>0)),]
ht.sum$logBiomassTot<-log(as.numeric(ht.sum$BiomassTot))
dim(ht.sum)



#### repeat for abundance; though this is going to compare, in some cases, density and biomass
pp.suma<-ddply(data.fa[(data.fa$trophy=='A'),], .(Expt, RefShort, TempIncr,  MaxTrophicLev,  abundance.type), summarize, sum(abundance)) #ConsumerPresent,
pp.suma$TotalA<-(rep('TotalP', length(pp.suma[,1])))
names(pp.suma)<-c("Expt1", "RefShort1", "TempIncr1","MaxTrophicLev1","abundtype","abundTot", 'TotalA') #"ConsumerPresent1", 
pp.suma<-pp.suma[(which(pp.suma$abundTot>0)),]
pp.suma$logabundTot<-log(as.numeric(pp.suma$abundTot))
dim(pp.suma)

ht.suma<-ddply(data.fa[(data.fa$trophy=='H'),], .(Expt, RefShort, TempIncr, MaxTrophicLev, abundance.type), summarize, sum(abundance))
ht.suma$TotalA<-(rep('TotalH', length(ht.suma[,1])))
names(ht.suma)<-c("Expt1", "RefShort1","TempIncr1", "MaxTrophicLev1","abundtype","abundTot", 'TotalA')
ht.suma<-ht.suma[(which(ht.suma$abundTot!='NA')),]
ht.suma<-ht.suma[(which(ht.suma$abundTot>0)),]
ht.suma$logabundTot.Het<-log(as.numeric(ht.suma$abundTot))
dim(ht.suma)  # this has 39 refs, which is right; -Brauer adn Barton et al w/ only plants

### merge to get C:R ratio for biomass; for this ratio, we only want pp.sum rows that have heterotrophs.
sums.rb<-merge(pp.sum, ht.sum, by.x=c("RefShort1",  "Expt1", "TempIncr1", "MaxTrophicLev1"), by.y=c("RefShort1",  "Expt1", "TempIncr1", "MaxTrophicLev1"), all=T)
sums.rb$HAratioB<-sums.rb$BiomassTot.y/sums.rb$BiomassTot.x

### merge to get C:R ratio for abundance; # pp.suma[(which(pp.suma$MaxTrophicLev1>=2)),]
sums.ra<-merge(pp.suma, ht.suma, by.x=c("RefShort1",  "Expt1", "TempIncr1", "MaxTrophicLev1"), by.y=c("RefShort1",  "Expt1", "TempIncr1", "MaxTrophicLev1"), all=T)
sums.ra$HAratio<-sums.ra$abundTot.y/sums.ra$abundTot.x
names(sums.ra)<-c('RefShort', 'Expt', 'TempIncr', 'MaxTrophicLev', 'abundtype.Auto', 'abundTot.Aut', 'Total.type', 'logAbundTot.Auto',  'abundtype.Het', 'abundTot.Het', 'Total.type2', 'logabundTot.Het', 'HAratio') # 'ConsumerPresent.Auto',
sums.ra<-sums.ra[,-7]
sums.ra<-sums.ra[,-10]
head(sums.ra)

#testing: 
sommer<-sums.ra[which(sums.ra$RefShort=='Sommer et al. 2007'),]
### for H/A biomass ratio, switch this to biomsas
sums.ra<-sums.ra[-which((sums.ra$Expt=='106') & (sums.ra$abundtype.Het=='Biomass')),]
#sums.ra<-sums.ra[-which((sums.ra$Expt=='118') & (sums.ra$abundtype.Het=='Biomass')),]
#sums.ra<-sums.ra[-which((sums.ra$Expt=='119') & (sums.ra$abundtype.Het=='Biomass')),]

### merge the sums and ratios into one file with dataset; start with pp.suma, because it has all the autotroph sums, including for the no-heterotroph treatments. 
sums.raf<-merge(sums.ra, pp.suma, by.x=c("RefShort",  "Expt", "TempIncr", "MaxTrophicLev",  'abundtype.Auto'), by.y=c("RefShort1",  "Expt1", "TempIncr1",  'MaxTrophicLev1',  'abundtype'), all=T) # 'ConsumerPresent.Auto',  'ConsumerPresent.Auto',
sums.raf<-sums.raf[,-13]
head(sums.raf)
## this seems to have worked. so far so good.
sommer<-sums.raf[which(sums.raf$RefShort=='Sommer et al. 2007'),]


### now merge with full dataset
data.fat<-merge(data.fa, sums.raf, by.x=c("RefShort",  "Expt", "TempIncr", 'MaxTrophicLev'), by.y=c("RefShort",  "Expt", "TempIncr", 'MaxTrophicLev'), all=T)
#data.fat[which(data.fat$Expt==13),]
#sums.raf[which(sums.raf$Expt==13),]

data.fat$grpA<-paste(data.fat$ExptA, data.fat$MaxTrophicLev)
data.fat$grpH<-paste(data.fat$ExptA, data.fat$MaxTrophicLev)


## Add a column to the database that indicates Tmin or Tmax
maxtemp<-ddply(data.fat, .(Expt), summarize, max(TempIncr))
names(maxtemp)<-c('Expt', 'TempIncrMax')
mintemp<-ddply(data.fat, .(Expt), summarize, min(TempIncr))
names(mintemp)<-c('Expt', 'TempIncrMin')
data.fm<-merge(data.fat, maxtemp, by="Expt")
data.fmm<-merge(data.fm, mintemp, by="Expt")
data.fmm<-data.fmm[,-(4:5)]
data.fmm$Tind<-ifelse(data.fmm$TempIncr==data.fmm$TempIncrMin, 'Tmin', 'Tmod')
data.fmm$Tind<-ifelse(data.fmm$TempIncr==data.fmm$TempIncrMax, 'Tmax', data.fmm$Tind)

### some checking the database: 
table(dat1$AddTrt, dat1$StressorAdded)
