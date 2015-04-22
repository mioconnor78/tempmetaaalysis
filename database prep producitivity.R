#### data processing for productivity data


### load the data
library(reshape2)
library(plyr)
library(lattice)
library(ggplot2)
library(car)
library(nlme)
setwd("/Users/maryo/Dropbox/Mary/R files")
datp <- read.csv("productivityrows.csv")


## DATA PROCESSING
datp$TempMean<-as.numeric(as.character(datp$TempMean))
datp$SGenTime<-as.numeric(as.character(datp$SamplingDay_Gen))
datp$Biomass <- as.numeric(as.character(datp$Biomass))
datp$Expt<-as.factor(datp$Expt)

### get rid of long columns: 
datp<-datp[,-(71:72)]  ## get rid of blank and notes cols
datp<-datp[,-(58)] # get rid of long ref column
datp<-datp[,-(5)] # get rid of an NA col


### add columns ####
#add an algae ID and invT columns: 
datp$microalgae<-ifelse((datp$TaxonGrp=='microalgae'), 'yes', 'no')
datp$microalgae<-ifelse((datp$TaxonGrp=='phytoplankton'), 'yes', datp$microalgae)
k <- 8.62*10^-5
datp$invT<- 1/((k*(datp$TempMean + 273)))
datp$invT<- as.numeric(datp$invT)
datp$grpID<-paste(datp$Expt, datp$Taxon, datp$ConsumerPresent, datp$MaxTrophicLev, datp$SpeciesName)

### create a column for abundance (density or biomass); this prioritizes biomass when both are measured
datp$abundance<-ifelse(is.na(datp$Biomass), datp$Density, datp$Biomass)
datp$abundance.var<-ifelse(is.na(datp$Biomass), datp$DensitySD, datp$BiomassSD)
datp$abundance.type<-ifelse(is.na(datp$Biomass), 'Density', 'Biomass')

## USE ONLY FINAL TIMEPOINTS
datp<-datp[which(datp$Day=='Final'),]

#distinguish the w/ and w/o consumer
datp$grpA<-paste(datp$ExptA, datp$ConsumerPresent, datp$TrtValue)

