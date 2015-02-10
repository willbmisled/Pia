#read data
setwd("~/PortableApps/R/scripts/Pia")
library(RODBC)
con<-odbcConnectExcel("DatosAvesPia.xls") # open a connection to the Excel file
Pia<-sqlFetch(con, 'Counts',stringsAsFactors=FALSE) # read a sheet
close(con)

names(Pia)

StartTime<-aggregate(Pia$Time,by=list(Pia$SiteCode),min)
EndTime<-aggregate(Pia$Time,by=list(Pia$SiteCode),max)
Stations<-aggregate(Pia$Time,by=list(Pia$SiteCode),length)
a<-merge(Stations,StartTime,by='Group.1')
a<-merge(a,EndTime,by='Group.1')
names(a)<-c('SiteCode','N','StartTime','EndTime')
Sites<-unique(Pia[,c(2:5,7)])
Sites<-merge(Sites,a,by='SiteCode')
Sites<-Sites[order(Sites$Observer,Sites$Date,Sites$StartTime),]

write.table(Sites, file='Sites.csv',row.names=F,sep=',') #write to csv

