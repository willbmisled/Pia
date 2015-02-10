#To Do
#  Add Plant names



#read data
  setwd("~/PortableApps/R/scripts/Pia")
  library(RODBC)
  con<-odbcConnectExcel("DatosAvesPia.xls") # open a connection to the Excel file
  sqlTables(con)$TABLE_NAME # get list of all sheets
  Pia<-sqlFetch(con, 'Counts',stringsAsFactors=FALSE) # read a sheet
  birdNames<-sqlFetch(con, 'BirdNames',stringsAsFactors=FALSE) # read a sheet
  plantNames<-sqlFetch(con, 'PlantNames',stringsAsFactors=FALSE) # read a sheet
  close(con)
#select plant and bird columns
  names(Pia)
  BC<-15:32
  PC<-33:49
  
#aggregate plant and bird data by site
  #function to aggregate relative abundance (birds) or percent present (Plants) by site
  Sitio<-function(Sp,Aggregator){
    x<-Pia[,Sp]
    #StdErr function
    se<-function(x) sd(x)/sqrt(length(x))
    #get N
    Out<-aggregate(x,by=list(Aggregator),length)
    names(Out)<-c('SiteCode','N')
    #get mean
    Mean<-aggregate(x,by=list(Aggregator),mean)
    names(Mean)<-c('SiteCode','Mean')
    Out$Mean<-Mean$Mean
    #get SE
    SE<-aggregate(x,by=list(Aggregator),se)
    names(SE)<-c('SiteCode','StdErr')
    Out$StdErr<-SE$StdErr
    #Add Species
    Out$Sp<-Sp
    return(Out)
  }
  
Sp<-a[i]
  Aggregator<-Pia$Area
 i<-1
  Sitio(a[i],Pia$Area)
  

#birds
  #Loop to aggregate all birds by Area
    a<-names(Pia)[BC]
  #start loop
  areaBirds<-c()
  for(i in c(1:length(a)) ){
    x<-Sitio(a[i],Pia$Area)
    areaBirds<-rbind(areaBirds,x)
  }
  names(areaBirds)[1]<-'Area'
  
  

  #Loop to aggregate all birds by Site
    #choose species to include: OJO make sure this is correct
      a<-names(Pia)[BC]
    #start loop
      AllSp<-c()
      for(i in c(1:length(a)) ){
        x<-Sitio(a[i],Pia$SiteCode)
        AllSp<-rbind(AllSp,x)
      }
  
  #change Missing values to Zeros
   AllSp[is.na(AllSp)]<-0
  

  #reformat results to wide 
    library(reshape2)
    #SiteSp
      #means
        SiteSpMean<-dcast(AllSp, SiteCode ~ Sp,value.var='Mean')
      #stderr
        SiteSpSE<-dcast(AllSp, SiteCode ~ Sp,value.var='StdErr')
      #check names-should be all true
        table(names(SiteSpMean)[-1]==birdNames$ColumnName)
          #names(SiteSpMean)[-1]%in%birdNames$ColumnName
        table(names(SiteSpSE)[-1]==birdNames$ColumnName)
      #rename
        names(SiteSpMean)[-1]<-paste(birdNames$Abbr,'_M',sep='')
        names(SiteSpSE)[-1]<-paste(birdNames$Abbr,'_S',sep='')
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
        SpSiteMean<-dcast(AllSp, Sp ~ SiteCode,value.var='Mean')
      #stderr
        SpSiteSE<-dcast(AllSp, Sp ~ SiteCode,value.var='StdErr')
      #rename
        names(SpSiteMean)[-1]<-paste(names(SpSiteMean)[-1],'_M',sep='')
        names(SpSiteSE)[-1]<-paste(names(SpSiteSE)[-1],'_S',sep='')
      #combine and reorder
        SpSite<-cbind(SpSiteMean,SpSiteSE[,-1])
          a<-names(SpSite)[-1]
          a<-c(names(SpSite)[1],a[order(a)])
        SpSite<-SpSite[,a]
      #add species abbreviations
        a<-merge(birdNames,SpSite,by.y='Sp',by.x='ColumnName',all.y=TRUE)
        SpSite<-a[,-c(1:2)]
      #save data
        write.table(SpSite, file='SpSiteBirds.csv',row.names=F,sep=',') #write to csv
  
  #keep the bird data 
  siteBirds<-AllSp  

  
  #plant data
  #Loop to aggregate all plants by SiteCode
  #choose species to include: OJO make sure this is correct
  a<-names(Pia)[PC]
  #start loop
  AllSp<-c()
  for(i in c(1:length(a)) ){
    x<-Sitio(a[i],Pia$SiteCode)
    AllSp<-rbind(AllSp,x)
  }
  
  #change Missing values to Zeros
    AllSp[is.na(AllSp)]<-0
  

  
  #reformat results to wide 
  library(reshape2)
  #means
  SiteSpMean<-dcast(AllSp, SiteCode ~ Sp,value.var='Mean')
  #stderr
  SiteSpSE<-dcast(AllSp, SiteCode ~ Sp,value.var='StdErr')
  #check names-should be all true
  table(names(SiteSpMean)[-1]==plantNames$ColumnName)
    #which(names(SiteSpMean)[-1]!=plantNames$ColumnName)
  table(names(SiteSpSE)[-1]==plantNames$ColumnName)
  #rename
  names(SiteSpMean)[-1]<-paste(plantNames$Abbr,'_M',sep='')
  names(SiteSpSE)[-1]<-paste(plantNames$Abbr,'_S',sep='')
  #combine and reorder
  SiteSp<-cbind(SiteSpMean,SiteSpSE[,-1])
  a<-names(SiteSp)[-1]
  a<-c(names(SiteSp)[1],a[order(a)])
  SiteSp<-SiteSp[,a]
  #save data
  write.table(SiteSp, file='SiteSpPlants.csv',row.names=F,sep=',') #write to csv
  
  #SpSite
  #means
    SpSiteMean<-dcast(AllSp, Sp ~ SiteCode,value.var='Mean')
  #stderr
    SpSiteSE<-dcast(AllSp, Sp ~ SiteCode,value.var='StdErr')
  #rename
    names(SpSiteMean)[-1]<-paste(names(SpSiteMean)[-1],'_M',sep='')
    names(SpSiteSE)[-1]<-paste(names(SpSiteSE)[-1],'_S',sep='')
  #combine and reorder
    SpSite<-cbind(SpSiteMean,SpSiteSE[,-1])
      a<-names(SpSite)[-1]
      a<-c(names(SpSite)[1],a[order(a)])
    SpSite<-SpSite[,a]
  #add species abbreviations
    a<-merge(plantNames,SpSite,by.y='Sp',by.x='ColumnName',all.y=TRUE)
    SpSite<-a[,-c(1:2)]
  
  #save data
    write.table(SpSite, file='SpSitePlants.csv',row.names=F,sep=',') #write to csv

  #save plants
    sitePlants<-AllSp
  

  
  rawBirds<-Pia
  ##Save the data    
  save(rawBirds,siteBirds,sitePlants,areaBirds,birdNames,plantNames,file='CountSummaries.rda') 
  


