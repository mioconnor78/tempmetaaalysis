###### CIEE group temperature trophic expt database
###### May 17 2014 #################################
###### analyses for manuscript #######################
###### Mary O'Connor ################################


#### CREATING AND MERGING SIZE MASTER FILE ###


### load the data
library(reshape2)
library(plyr)
library(lattice)
library(ggplot2)
library(car)
library(nlme)
setwd("/Users/maryoconnor/Dropbox/Mary/R files")
data1 <- read.csv("tempexptsmay14.csv")
sizes.master <- read.csv("sizemaster.csv")


### merge by a new species ID column
data1$spID<-paste(data1$TaxonGrp, data1$Taxon, data1$SpeciesName)
data1$BodySizeGen <- sizes.master$BodySizeG[match(data1$spID, sizes.master$spID)]

OK, so if new species are added to the database and we want general sizes, we need to add their spID to the size master and their size, manually.



### I used this sizesum to start a bodysizemaster sheet that we can update. Then, we can merge it with our database: 
data<-merge(data1, sizes.sum, by="spID", all=T, suffixes = c("", ".M"))

length(unique(data1$spID))
length(unique(sizes$spID))
length(unique(sizes.sum$spID))
length(unique(data$spID))
match(data$spID, sizes$spID)





##### creation of the size master file 
# the size master now has size information that other files don't have. So use this one for any general size updates! don't remake it. 
sizes <- read.csv("bodysizemay2014.csv")
sizes$spID<-paste(sizes$TaxonGrp, sizes$Taxon, sizes$SpeciesName)
names(sizes)
sizes.sum <- ddply(sizes, .(spID, BodySizeG, BodySizeSD, BodySizeUnits), summarize, length(unique(ExptA)))
write.csv(sizes.sum, "sizemaster.csv")