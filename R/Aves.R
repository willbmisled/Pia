
#read data
  setwd("data")
  library(RODBC)
  con<-odbcConnectExcel("DatosAvesPia.xls") # open a connection to the Excel file
  sqlTables(con)$TABLE_NAME # get list of all sheets
  Pia<-sqlFetch(con, 'Counts',stringsAsFactors=FALSE) # read a sheet
  Birds<-sqlFetch(con, 'BirdNames',stringsAsFactors=FALSE) # read a sheet
  Plants<-sqlFetch(con, 'PlantNames',stringsAsFactors=FALSE) # read a sheet
  close(con)
#select plant and bird columns
  names(Pia)
  BC<-17:34
  PC<-35:51
  
#aggregate plant and bird data 
  #function to aggregate relative abundance (birds) or percent present (Plants) by site
  Sitio<-function(Sp){
    x<-Pia[,Sp]
    #StdErr function
    se<-function(x) sd(x)/sqrt(length(x))
    #get N
    Out<-aggregate(x,by=list(Pia$SitioAbbr),length)
    names(Out)<-c('Sitio','N')
    #get mean
    Mean<-aggregate(x,by=list(Pia$SitioAbbr),mean)
    names(Mean)<-c('Sitio','Mean')
    Out$Mean<-Mean$Mean
    #get SE
    SE<-aggregate(x,by=list(Pia$SitioAbbr),se)
    names(SE)<-c('Sitio','StdErr')
    Out$StdErr<-SE$StdErr
    #Add Species
    Out$Sp<-Sp
    return(Out)
  }

#birds
  #Loop to aggregate all birds
    #choose species to include: OJO make sure this is correct
      a<-names(Pia)[BC]
    #start loop
      AllSp<-c()
      for(i in c(1:length(a)) ){
        x<-Sitio(a[i])
        AllSp<-rbind(AllSp,x)
      }
  
  #change Missing values to Zeros
   AllSp[is.na(AllSp)]<-0
  
  #reformat results to wide 
    library(reshape2)
    #SiteSp
      #means
        SiteSpMean<-dcast(AllSp, Sitio ~ Sp,value.var='Mean')
      #stderr
        SiteSpSE<-dcast(AllSp, Sitio ~ Sp,value.var='StdErr')
      #check names-should be all true
        table(names(SiteSpMean)[-1]==Birds$Spanish)
          #names(SiteSpMean)[-1]%in%Birds$Spanish
        table(names(SiteSpSE)[-1]==Birds$Spanish)
      #rename
        names(SiteSpMean)[-1]<-paste(Birds$Spanish,'_M',sep='')
        names(SiteSpSE)[-1]<-paste(Birds$Spanish,'_S',sep='')
      #combine and reorder
        SiteSp<-cbind(SiteSpMean,SiteSpSE[,-1])
          a<-names(SiteSp)[-1]
          a<-c(names(SiteSp)[1],a[order(a)])
        SiteSp<-SiteSp[,a]
    #save data
      #con<-odbcConnectExcel("DatosAvesPia.xls") # open a connection to the Excel file
      #sqlSave(con,SiteSp,safer=FALSE)
      write.table(SiteSp, file='SiteSpBirds.csv',row.names=F,sep=',') #write to csv
    
    #SpSite
      #means
        SpSiteMean<-dcast(AllSp, Sp ~ Sitio,value.var='Mean')
      #stderr
        SpSiteSE<-dcast(AllSp, Sp ~ Sitio,value.var='StdErr')
      #rename
        names(SpSiteMean)[-1]<-paste(names(SpSiteMean)[-1],'_M',sep='')
        names(SpSiteSE)[-1]<-paste(names(SpSiteSE)[-1],'_S',sep='')
      #combine and reorder
        SpSite<-cbind(SpSiteMean,SpSiteSE[,-1])
          a<-names(SpSite)[-1]
          a<-c(names(SpSite)[1],a[order(a)])
        SpSite<-SpSite[,a]
      #add species abbreviations
        a<-merge(SpSite,Birds,by.x='Sp',by.y='Spanish',all.x=TRUE)
        SpSite<-cbind(a[,ncol(a)],SpSite)
        names(SpSite)[1]<-'Abbr'
      #save data
        write.table(SpSite, file='SpSiteBirds.csv',row.names=F,sep=',') #write to csv
  
  
  
  
  #plant data
  #Loop to aggregate all birds
  #choose species to include: OJO make sure this is correct
  a<-names(Pia)[PC]
  #start loop
  AllSp<-c()
  for(i in c(1:length(a)) ){
    x<-Sitio(a[i])
    AllSp<-rbind(AllSp,x)
  }
  
  #change Missing values to Zeros
    AllSp[is.na(AllSp)]<-0
  
  #reformat results to wide 
  library(reshape2)
  #means
  SiteSpMean<-dcast(AllSp, Sitio ~ Sp,value.var='Mean')
  #stderr
  SiteSpSE<-dcast(AllSp, Sitio ~ Sp,value.var='StdErr')
  #check names-should be all true
  table(names(SiteSpMean)[-1]==Plants$Spanish)
    #which(names(SiteSpMean)[-1]!=Plants$Spanish)
  table(names(SiteSpSE)[-1]==Plants$Spanish)
  #rename
  names(SiteSpMean)[-1]<-paste(Plants$Spanish,'_M',sep='')
  names(SiteSpSE)[-1]<-paste(Plants$Spanish,'_S',sep='')
  #combine and reorder
  SiteSp<-cbind(SiteSpMean,SiteSpSE[,-1])
  a<-names(SiteSp)[-1]
  a<-c(names(SiteSp)[1],a[order(a)])
  SiteSp<-SiteSp[,a]
  #save data
  write.table(SiteSp, file='SiteSpPlants.csv',row.names=F,sep=',') #write to csv
  
  #SpSite
  #means
    SpSiteMean<-dcast(AllSp, Sp ~ Sitio,value.var='Mean')
  #stderr
    SpSiteSE<-dcast(AllSp, Sp ~ Sitio,value.var='StdErr')
  #rename
    names(SpSiteMean)[-1]<-paste(names(SpSiteMean)[-1],'_M',sep='')
    names(SpSiteSE)[-1]<-paste(names(SpSiteSE)[-1],'_S',sep='')
  #combine and reorder
    SpSite<-cbind(SpSiteMean,SpSiteSE[,-1])
      a<-names(SpSite)[-1]
      a<-c(names(SpSite)[1],a[order(a)])
    SpSite<-SpSite[,a]
  #add species abbreviations
    a<-merge(SpSite,Plants,by.x='Sp',by.y='Spanish',all.x=TRUE)
    SpSite<-cbind(a[,ncol(a)],SpSite)
    names(SpSite)[1]<-'Abbr'
  #save data
    write.table(SpSite, file='SpSitePlants.csv',row.names=F,sep=',') #write to csv
  
  



#in progress

#Add error bars
  AllSp$Lower<-AllSp$Mean-AllSp$StdErr
  AllSp$Lower[AllSp$Lower<0]<-0
  AllSp$Upper<-AllSp$Mean+AllSp$StdErr
#Plot by Sp
  Sp<-AllSp[AllSp$Sp=="Rayadito",]
  windows(10,7.5)  
  barplot(Sp$Mean,ylim=c(0,max(AllSp$Upper)))
  abline(v=.7)

  
  
  
  
  aggregate(x,by=list(Pia$Sitio),sd)
   
Ray<-Sitio('Rayadito')
  Cometocino<-Sitio('Cometocino')
  
windows(10,7.5)  
  barplot(Ray$Mean)
  Upper<-Ray$Mean+
  
  #save the data
  write.table(Ray, file='Ray.csv',row.names=F,sep=',') #write to csv
  
str(Ray)
  
  aggregate(Pia$Rayadito,by=list(Pia$Sitio),se)
  
  
sp<-'Rayadito'
Pia[,sp]
  
  

#create SpatialPointsDataFrame
  library(sp)
  Pia<-Pia[!is.na(Pia$Lat),]
  coordinates(Pia)<-~Lon+Lat
  proj4string(Pia)<-CRS("+proj=longlat +datum=WGS84")  #ESRI GCS_WGS_1984 
  
#colors
  Pia$Color<-'black'
  Pia$Color[Pia$Sitio=="TouristTrail"]<-'red'
  Pia$Color[Pia$Sitio=="TerminalMoraine"]<-'orange'
  Pia$Color[Pia$Sitio=="UplandLake"]<-'blue'
  Pia$Color[Pia$Sitio=="InteriorForest"]<-'red'
  Pia$Color[Pia$Sitio=="CoastalForest"]<-'pink'
  Pia$Color[Pia$Sitio=="RiverForest"]<-'yellow'
  
  table(Pia$Sitio,useNA='ifany')
  table(Pia$Color,useNA='ifany')
            
   
#plot points
  windows(10,7.5)
  plot(Pia[Pia$Lugar=='Pia',],pch=19,col=Pia$Color,axes=TRUE)
  
  windows(10,7.5)
  plot(Pia[Pia$Sitio=='TouristTrail',],pch=19,axes=TRUE)
  
  windows(10,7.5)
  par(mfrow=c(1,2))
  plot(Pia[Pia$Sitio=='TouristTrail',],axes=TRUE,pch=19)
  plot(Pia[Pia$Sitio=='InteriorForest',],axes=TRUE,pch=19)
  
  windows(10,7.5)
  plot(Pia[Pia$Lugar=='Pia',],pch=19,col=Pia$Color,axes=TRUE)
  plot(Pia[Pia$Sitio=='TouristTrail',],axes=TRUE,add=TRUE)
  
  windows(10,7.5)
  plot(Pia[Pia$Lugar=='Pia',],pch=19,col=Pia$Color,axes=TRUE)
  plot(Pia[Pia$Sitio=='TouristTrail',],add=TRUE)
  plot(Pia[Pia$Sitio=='TerminalMoraine',],add=TRUE)
  plot(Pia[Pia$Sitio=='UplandLake',],add=TRUE)
  plot(Pia[Pia$Sitio=='CoastalForest',],add=TRUE)
  
  
  windows(10,7.5)
  plot(Pia[Pia$Lugar=='Pia',],pch=19,col=NA,axes=TRUE)
  plot(Pia[Pia$Sitio=='TouristTrail',],add=TRUE,col='red',pch=19)
  plot(Pia[Pia$Sitio=='TerminalMoraine',],add=TRUE,col='grey',pch=19)
  plot(Pia[Pia$Sitio=='UplandLake',],add=TRUE,col='blue',pch=19)
  plot(Pia[Pia$Sitio=='CoastalForest',],add=TRUE,col='green',pch=19)
  
  
  plot(Pia[Pia$Sitio=='InteriorForest',],add=TRUE)
  





