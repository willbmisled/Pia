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
  
#SpatialPointsDataframes
  #decimal degrees
    dd<-Pia[!is.na(Pia$Latitude),]
      coordinates(dd)<-~Longitude+Latitude
      proj4string(dd)<-DD
  #UTM
    utm<-Pia[!is.na(Pia$Easting),]
      coordinates(utm)<-~Easting+Northing
      proj4string(utm)<-UTM
  
#create KML files for each SiteCode
  a<-unique(dd$SiteCode)
    for(i in c(1:length(a))){
      xy<-dd[dd$SiteCode==a[i],]
      writeOGR(xy["Station"], paste(a[i],'.kml',sep=''), xy$Station, "KML",overwrite_layer=TRUE)#create kml file
    }  
      
  
#Save SHP
  writeOGR(dd,getwd(),'PiaSitesDD',"ESRI Shapefile")   #create shapefile
  writeOGR(utm,getwd(),'PiaSitesUTM',"ESRI Shapefile")   #create shapefile
  
 