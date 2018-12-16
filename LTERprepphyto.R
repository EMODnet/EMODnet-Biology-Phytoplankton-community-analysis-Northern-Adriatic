library(reshape)
library(taxize)

assembleset<-function(filenambase){
  ds<-read.csv(paste(filenambase,"_basic.csv",sep=''),header=T,stringsAsFactors=F)
  droplist<-c(1,2,3,10,13,14,17,18)
  ds<-ds[,-droplist]
  dsmf<-read.csv(paste(filenambase,"_mf.csv",sep=""),header=T,stringsAsFactors=F)
  droplist<-c(1,2,4,7:12)
  dsmf<-dsmf[,-droplist]
  # correct for single r in occurencid of phytomf (not of phyto)
  names(dsmf)<-c("occurrenceid","measurementtype","measurementvalue")
  dsmfc<-cast(data=dsmf,formula=occurrenceid ~ measurementtype,value="measurementvalue")
  dsf<-merge(ds,dsmfc,by="occurrenceid")
  dsf$date<-as.Date(paste(dsf$yearcollected,"-",dsf$monthcollected,"-",dsf$daycollected,sep=""),format="%Y-%m-%d")
  dsf<-dsf[order(dsf$date,dsf$scientificname_accepted),]
  return(dsf)
}

phyto<-assembleset("Phytoplankton_downloaded")

# drop records without Abundance, and drop biomass values
phyto<-phyto[!is.na(phyto$Abundance),]
bc<-which(names(phyto)=="Biomass")
phyto<-phyto[,-bc]

# look up classification of all species
aidlist<-unique(phyto$aphiaidaccepted)
#apcl<-classification(aidlist,db="worms")
#save(apcl,file="phyto_apcl.Rdata")
load("phyto_apcl.Rdata")
#make data frame of results
txtr<-data.frame(aphiaidaccepted=aidlist,Kingdom=NA,Subkingdom=NA,Infrakingdom=NA,
                Phylum=NA,Subphylum=NA,Infraphylum=NA,Superclass=NA,Class=NA,Subclass=NA,
                Superorder=NA,Order=NA,Suborder=NA,Family=NA,Subfamily=NA,Genus=NA,
                Species=NA)
for (i in 1:length(aidlist)){
  for (j in 1:length(apcl[[i]]$rank)){
    txtr[i,apcl[[i]]$rank[j]]<-apcl[[i]]$name[j]
  }
}
# remove Plantae (too uncertain to keep) and Protista (doubts on validity of data)
# these are the only entries where Class is not available
txtr<-txtr[!is.na(txtr$Class),]
# make a 'displayName' field that contains species name or lowest higher taxon + ' indet.'
txtr$displayName<-txtr$Species
for (i in 1:length(txtr[,1])){
  if(is.na(txtr$Species[i])){
      j<-max(which(!is.na(txtr[i,1:17])))
      txtr$displayName[i]<-paste(txtr[i,j],"indet.")
    }
}
# make a 'displayGenus' field that contains genus name or lowest higher taxon
txtr$displayGenus<-txtr$Genus
for (i in 1:length(txtr[,1])){
  if(is.na(txtr$Genus[i])){
    j<-max(which(!is.na(txtr[i,1:17])))
    txtr$displayGenus[i]<-paste(txtr[i,j],"indet.")
  }
}
# make a 'displayClass' field that contains class to be displayed and used in analyses
txtr$displayClass<-txtr$Class

# only keep aphiaidaccepted, Class, displayName, displayGenus and displayClass in the file
txtr<-txtr[,-c(2:8,10:17)]

# merge taxonomic information with data. This will automatically delete records for which
# taxonomic information has been deleted (Plantae, Protista)
phyto<-merge(phyto,txtr,by="aphiaidaccepted")

# average over depth samples
# determine number of depth slices per sample
ndps<-cast(phyto,date~.,value='minimumdepth',fun=function(x) length(unique(x)))
names(ndps)[2]<-'ndps'
phyto<-merge(phyto,ndps,by='date')
# divide 'Abundance' field by number of depth slices (to be summed afterwards, 
# yielding the mean)
phyto$Abundance<-phyto$Abundance/phyto$ndps
# sum over depth slices to yield plankt again
phyto<-cast(phyto,date+yearcollected+monthcollected+daycollected+aphiaidaccepted+
              scientificname_accepted+displayName+displayGenus+displayClass+ndps~.,
            value="Abundance",fun=sum)
names(phyto)[length(names(phyto))]<-"Abundance"
## end of tuning basic data table. phyto now contains depth-averaged abundances


########## group by genus and save in PhytoGenus
# prepare Phytoplankton file
#Summing per class
tt<-cast(phyto,formula=date+yearcollected+monthcollected+daycollected+ndps~displayClass
         ,value='Abundance',fun=sum)
#Summing per genus
tt2<-cast(phyto,formula=date+yearcollected+monthcollected+daycollected+ndps~displayGenus
          ,value='Abundance',fun=sum,margins='grand_col')
#combining both
ttt<-merge(tt,tt2,by=c('date','yearcollected','monthcollected','daycollected','ndps'))

ldc<-5
ngens<-length(ttt[1,])-ldc
#

#frequency of observation per species
no<-apply(ttt[,(ldc+1):(ldc+ngens)],2,function(x) length(x[x>0]))
# remove all species from tt that have less than 20 observations
ll<-which(no<20)+ldc
ttt<-ttt[,-ll]
ngens<-length(ttt[1,])-ldc

PhytoGenus<-ttt
names(PhytoGenus)[1:ldc]<-c("datecollected","YearCollected","MonthCollected",
                               "DayCollected","ndps")
#
# store in PhytoGenus
#
save(PhytoGenus,txtr,file="PhytoGenus.Rdata")




# plot yearly averages of classes

pdf('phyto_genera_and_groups_per_year.pdf')
ttt<-PhytoGenus
txf<-unique(ttt$YearCollected)
mitxf<-min(as.numeric(txf))
matxf<-max(as.numeric(txf))
for (spec in 1:ngens){
  xf<-as.factor(ttt$YearCollected)
  xv<-as.numeric(xf)
  yv<-ttt[,spec+ldc]
  yv<-as.numeric(yv)
  yv<-sqrt(sqrt(yv))
  #      colv<-as.factor(tt$institutionCode[tt$locality==locs[stat]])
  #      colv<-as.numeric(colv)
  mt<-names(ttt)[spec+ldc]
  #      plot(xf,yv,main=mt,type="b",xlim=c(mitxf,matxf),col=colv,ylim=c(0,70),ylab='sqrt(sqrt(Abundance/m2))')
  plot(xf,yv,main=mt,type="b",ylab='sqrt(sqrt(Abundance))',col=c(rep('lightgrey',16),rep('darkgrey',11)))
}
dev.off()

