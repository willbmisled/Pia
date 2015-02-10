#This script uses SPtransform to add missing DD and UTM locations

#read data
  setwd("~/PortableApps/R/scripts/Pia")
  library(RODBC)
  con<-odbcConnectExcel("DatosAvesPia.xls") # open a connection to the Excel file
  sqlTables(con)$TABLE_NAME # get list of all sheets
  Pia<-sqlFetch(con, 'Counts',stringsAsFactors=FALSE) # read a sheet
  close(con)
  
#CRS
  library(sp)
  library(rgdal)
  DD<-CRS("+proj=longlat +datum=WGS84")  #decimal degrees
  UTM<-CRS("+init=epsg:32719") #UTM Zone 19S

#add missing UTM and LonLat coordinates
    #decimal degrees
      dd<-Pia[which(!is.na(Pia$Easting) & !is.na(Pia$Latitude)),]
      coordinates(dd)<-~Longitude+Latitude
      proj4string(dd)<-DD
    #UTM
      utm<-Pia[which(!is.na(Pia$Easting) & !is.na(Pia$Latitude)),]
      coordinates(utm)<-~Easting+Northing
      proj4string(utm)<-UTM
    #transform
        dd2utm<-spTransform(dd,UTM) #reproject
        utm2dd<-spTransform(utm,DD) #reproject
    #compare
      max(coordinates(dd)[,1]-coordinates(utm2dd)[,1])
      max(coordinates(dd)[,2]-coordinates(utm2dd)[,2])
      max(coordinates(utm)[,1]-coordinates(dd2utm)[,1])
      max(coordinates(utm)[,2]-coordinates(dd2utm)[,2])
  #get missing values
    #decimal degrees
      dd<-Pia[which(!is.na(Pia$Latitude)),]
      coordinates(dd)<-~Longitude+Latitude
      proj4string(dd)<-DD
    #UTM
      utm<-Pia[which(!is.na(Pia$Easting)),]
      coordinates(utm)<-~Easting+Northing
      proj4string(utm)<-UTM
    #transform
    dd2utm<-spTransform(dd,UTM) #reproject
      dd1<-data.frame(dd2utm@data$Order,round(coordinates(dd2utm)))
        names(dd1)<-c('Order','Easting1','Northing1')
    utm2dd<-spTransform(utm,DD) #reproject
      utm1<-data.frame(utm2dd@data$Order,coordinates(utm2dd))
  names(utm1)<-c('Order','Longitude1','Latitude1')
    Out<-merge(Pia,dd1,by='Order',all.x=TRUE)
    Out<-Out[,c('Order','Easting1','Northing1')]
    Out<-merge(Out,utm1,by='Order',all.x=TRUE)
 
 
write.table(Out,file='LocationsSPtrans.csv',row.names=F,sep=',') 
