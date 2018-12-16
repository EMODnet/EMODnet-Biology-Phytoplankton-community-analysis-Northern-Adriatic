# load("Phytoplankton.Rdata")
# load("Microzooplankton.Rdata")
# load("Mesozooplankton.Rdata")
# load("multiv.Rdata")
load("PhytoGenus.Rdata")
Phytoplankton=PhytoGenus
load("multivGenus.Rdata")
doc<-HTML(readLines('about.html'))

ldc<-5

plotspecs<-function(datset,species,transf){
  tt<-get(datset)
  if(species=="")species<-names(tt)[6]
  if(!species%in%names(tt))species<-names(tt)[6]
  ttt<-data.frame(y=tt[,'YearCollected'],sp=tt[,species])
  if(transf) ttt$sp<-sqrt(sqrt(ttt$sp))
  miy<-min(ttt$y)
  may<-max(ttt$y)
  boxplot(sp~y,data=ttt,main=species,ylab='abundance',outline=T,
          xlim=c(1,27),at=(miy-1985):(may-1985),
          col=c(rep('lightgray',17),rep('darkgray',10)))
}
plotseas<-function(datset,species,transf){
  tt<-get(datset)
  if(species=="")species<-names(tt)[6]
  if(!species%in%names(tt))species<-names(tt)[6]
  ttt<-data.frame(m=tt[,'MonthCollected'],sp=tt[,species])
  if(transf) ttt$sp<-sqrt(sqrt(ttt$sp))
  miy<-min(ttt$m)
  may<-max(ttt$m)
  boxplot(sp~m,data=ttt,main=species,ylab='abundance',outline=T,xlim=c(1,12),at=miy:may)
}


plotmultv<-function (datset,specname){
  if (datset=="Phytoplankton"){
    par(pty="s")
    plot(yrmns$ax1,yrmns$ax2,col='red',xlim=c(-8,4),ylim=c(-2,2),type="l",asp=1,xlab="",ylab="",
         axes=FALSE,frame.plot=TRUE,main=paste("PCA biplot,",specname,"in green"))
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
    text(yrmns$ax1,yrmns$ax2,yrmns$labs,col='red')
    lines(momns$ax1,momns$ax2,col='blue')
    text(momns$ax1,momns$ax2,momns$labs,col='blue')
    arrows(3,4,-7,4,col='red',lwd=2,code=2)
    text(-5,4.5,'Trend',col='red',cex=1)
    arrows(3.5,-3,3.5,3,col='blue',lwd=2,code=3)
    text(3.5,3.5,'Season',col='blue',cex=1)
  }else{
    frame()
  }
}

