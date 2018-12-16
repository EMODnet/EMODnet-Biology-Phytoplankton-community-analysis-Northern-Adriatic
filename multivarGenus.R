library(ade4)
library(reshape)

load("PhytoGenus.Rdata")

row.names(PhytoGenus)<-paste(PhytoGenus$YearCollected,"_",
                                PhytoGenus$MonthCollected,"_",
                                PhytoGenus$DayCollected,sep="")
phsp<-PhytoGenus[,-c((1:20),length(PhytoGenus[1,]))]
phsp<-sqrt(sqrt(phsp))
pca_phsp<-dudi.pca(phsp,scannf=F,nf=2)
scatter(pca_phsp)
xy<-data.frame(yr=as.factor(PhytoGenus$YearCollected),ax1=pca_phsp$li[,1],ax2=pca_phsp$li[,2])
boxplot(ax1~yr,xy)
boxplot(ax2~yr,xy)

ss<-as.Date(PhytoGenus$datecollected)
plot(ss,pca_phsp$li[,1],type="l")
lines(ss,pca_phsp$li[,2],type="l",col='red')

s.class(pca_phsp$li, as.factor(PhytoGenus$MonthCollected), cpoint = 0,clabel=0.5,
        cstar=0,cellipse=0.3,col=rep('blue',12),xlim=c(-3.5,1.5),ylim=c(-3.5,3.5))
s.class(pca_phsp$li, as.factor(PhytoGenus$YearCollected), cpoint = 0,clabel=0.5,
        cstar=0,cellipse=0.3,add.plot=T,col=rep('red',27))

txtr<-txtr[,-c(1,3)] # remove aphiaidaccepted and displayName
txtr<-unique(txtr)
spsel<-names(phsp)
spseldf<-data.frame(displayGenus=as.character(spsel))
spseldf<-merge(spseldf,txtr,by='displayGenus',all.x=T,all.y=F)
spseldf$displayGenus<-as.character(spseldf$displayGenus)


mnsax1<-as.vector(by(pca_phsp$li[,1],PhytoGenus$YearCollected,mean))
mnsax2<-as.vector(by(pca_phsp$li[,2],PhytoGenus$YearCollected,mean))
mmnsax1<-as.vector(by(pca_phsp$li[,1],PhytoGenus$MonthCollected,mean))
mmnsax2<-as.vector(by(pca_phsp$li[,2],PhytoGenus$MonthCollected,mean))

yrmns<-data.frame(ax1=mnsax1,ax2=mnsax2,labs=as.character(1986:2012))
momns<-data.frame(ax1=mmnsax1,ax2=mmnsax2,labs=as.character(1:12))
spseldf$ax1<-pca_phsp$co[,1]
spseldf$ax2<-pca_phsp$co[,2]

save(yrmns,momns,spseldf,file="multivGenus.Rdata")

plotmultv<-function (specname){
  par(pty="s")
  plot(yrmns$ax1,yrmns$ax2,col='red',xlim=c(-8,4),ylim=c(-2,2),type="l",asp=1,xlab="",ylab="",
       axes=FALSE,frame.plot=TRUE,main="PCA biplot, chosen species in green")
  Axis(side=1, labels=FALSE)
  Axis(side=2, labels=FALSE)
  abline(h=0)
  abline(v=0)
  colv<-rep('lightgray',80)
  lwdv<-rep(0.1,80)
  if(!specname%in%spseldf$displayGenus){
    colv[which(spseldf$displayClass==specname)]<-'green'
    lwdv[which(spseldf$displayClass==specname)]<-2
  }
  arrows(0,0,spseldf$ax1*5,spseldf$ax2*5,col=colv,lwd=lwdv,code=0)
  if(specname%in%spseldf$displayGenus){
    spec=which(spseldf$displayGenus==specname)
    arrows(0,0,spseldf$ax1[spec]*5,spseldf$ax2[spec]*5,col="green",lwd=3,code=0)
    points(spseldf$ax1[spec]*5,spseldf$ax2[spec]*5,col="green",cex=2,pch=16)
  }
  text(yrmns$ax1,yrmns$ax2,yrmns$labs,col='red',cex=.7)
  lines(momns$ax1,momns$ax2,col='blue')
  text(momns$ax1,momns$ax2,momns$labs,col='blue',cex=.7)
  arrows(3,4,-7,4,col='red',lwd=2,code=2)
  text(-5,4.5,'Trend',col='red',cex=1)
  arrows(3.5,-3,3.5,3,col='blue',lwd=2,code=3)
  text(3.5,3.5,'Season',col='blue',cex=1)
}

plotmultv('Bacillariophyceae')
