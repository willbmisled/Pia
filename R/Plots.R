
#get the data
  setwd("~/PortableApps/R/scripts/Pia")
  load('CountSummaries.rda') 
  
  
#add birdNames to areaBirds
  Birds<-merge(areaBirds,birdNames,by.x='Sp',by.y='ColumnName',all.x=TRUE)[,-1]
  
#plotOrder for each species
  a<-aggregate(Birds$Mean,by=list(Birds$Abbr),mean)
  a<-a[order(a$x,decreasing=TRUE),]
  a$plotOrder<-1:nrow(a)
  a<-a[,-2]
  names(a)<-c('Abbr','plotOrder')
  
#add plotOrder to Birds and sort
  Birds<-merge(Birds,a,by='Abbr',all.x=TRUE)
  Birds<-Birds[order(c(Birds$plotOrder)),]
  

#plot birds by area
  #function to make plots
    plotArea<-function(Area='Aleman',Xlab=FALSE){
      Sp<-Birds[Birds$Area==Area,]
      if(Xlab==TRUE) {
        #xlabels<-substr(unique(Sp$Abbr),1,4)
        xlabels<-unique(Sp$Abbr)
      }else{
        xlabels<-NULL}
      Legend<-paste(unique(Sp$Area),' (N=',unique(Sp$N),")",sep='')
      x<-barplot(Sp$Mean,ylim=c(0,max(areaBirds$Mean+areaBirds$StdEr+.1)),names.arg=xlabels,axes=FALSE,cex.names=1.75,las=2)
      arrows(x,Sp$Mean-Sp$StdErr,x,Sp$Mean+Sp$StdErr,code=3,angle=90,length=.1)
      box()
      text(16.5,2,Legend,cex=2,adj=0)
      axis(2,at=1:2,labels=TRUE,las=1.5,cex.axis=2)
    }

###########setup for a multipanel graph
  Mai<-c(0,1,0.0000,0.1)  #default=c(1.36,1.093,1.093,0.56)
  Omi<-c(1.2,0.0,0.25,0.5)     #default=c(0,0,0,0)
  win.graph(11, 8.5) 
  par(mfrow=c(5,1),mai=Mai,omi=Omi)
  plotArea("Aleman",FALSE)
  plotArea("Español",FALSE)
  plotArea("Holanda",FALSE)
    mtext("Mean Abundance / Station (+/- StdErr)",2,4,cex=1.75)
  plotArea("Isla Hoste",FALSE)
  plotArea("Pía",TRUE)
    #mtext("Species",1,4,cex=2)
  
      

  
  
  barplot(Sp$Mean,names.arg=unique(Sp$Abbr),axes=FALSE,cex.names=.7,las=1.5)
  axis(2,at=1:2,labels=TRUE,las=1.5,cex.axis=2)
  
  ###########old
  
  
#plot birds by area
  a<-unique(Birds$Area)
  i<-1
  Sp<-Birds[Birds$Area==a[i],]
  xlabels=Sp$Abbr
  x<-barplot(Sp$Mean,ylim=c(0,max(areaBirds$Mean+areaBirds$StdEr+.1)),names.arg=xlabels,axes=FALSE)
  arrows(x,Sp$Mean-Sp$StdErr,x,Sp$Mean+Sp$StdErr,code=3,angle=90,length=.1)
  box()
  text(.25,2.4,unique(Sp$Area),cex=2,adj=0)
  axis(2,at=1:2,labels=TRUE,las=1.5)
  
  
  
  
  

#Plot Sp by Area
  
plotAggr<-function(Aggr=Birds$Abbr,i=11){
  a<-unique(Aggr)
  Sp<-Birds[Birds$Abbr==a[i],]
  xlabels=Sp$Area
  x<-barplot(Sp$Mean,ylim=c(0,max(areaBirds$Mean+areaBirds$StdEr+.1)),names.arg=xlabels,axes=FALSE)
  arrows(x,Sp$Mean-Sp$StdErr,x,Sp$Mean+Sp$StdErr,code=3,angle=90,length=.1)
  box()
  text(.25,2.4,unique(Sp$EnglishName),cex=2,adj=0)
  axis(2,at=1:2,labels=TRUE,las=1.5)
}
  
  #windows(10,7.5)
  plotAggr(Aggr=Birds$Abbr,15)
  
  
  Aggr=Birds$Area
  i=1
    a<-unique(Aggr)
    Sp<-Birds[Birds$Abbr==a[i],]
    xlabels=Sp$Abbr
    x<-barplot(Sp$Mean,ylim=c(0,max(areaBirds$Mean+areaBirds$StdEr+.1)),names.arg=xlabels,axes=FALSE)
    arrows(x,Sp$Mean-Sp$StdErr,x,Sp$Mean+Sp$StdErr,code=3,angle=90,length=.1)
    box()
    text(.25,2.4,unique(Sp$EnglishName),cex=2,adj=0)
    axis(2,at=1:2,labels=TRUE,las=1.5)
  }
  
  
  
  x<-barplot(Sp$Mean,ylim=c(0,max(areaBirds$Mean+areaBirds$StdEr)),names.arg=Sp$Area, ylab="Relative Abundance (+/- 1 stdErr)",xlab='Site')
  
  text(.75,2.4,'xxxxxxxxxxxxxxx',cex=2,adj=0)
  names.arg=Sp$Area,