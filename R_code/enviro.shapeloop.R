#testloop

crs.geo<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  #define projection

for (mmm in sort(unique(junjul$year))){
  mmm
  yrfile <- dplyr::filter(junjul,year==mmm)
   
  yrfile.tempmid<-select(yrfile,STATIONID,DECSLON,DECSLAT,TEMPMID,year,month,day)
  yrfile.salmid<-select(yrfile,STATIONID,DECSLON,DECSLAT,SALMID,year,month,day)
  yrfile.chlorsurf<-select(yrfile,STATIONID,DECSLON,DECSLAT,CHLORSURF,year,month,day)
  yrfile.oxymax<-select(yrfile,STATIONID,DECSLON,DECSLAT,OXYMAX,year,month,day)
  #yrfile.turbmid<-select(yrfile,STATIONID,DECSLON,DECSLAT,TURBMID,year,month,day)
  
  yrfile.tempmid.nona<-filter(yrfile.tempmid,!TEMPMID==-9999)
  yrfile.salmid.nona<-filter(yrfile.salmid,!SALMID==-9999)
  yrfile.chlorsurf.nona<-filter(yrfile.chlorsurf,!CHLORSURF==-9999)
  yrfile.oxymax.nona<-filter(yrfile.oxymax,!OXYMAX==-9999)
  #yrfile.turbmid.nona<-filter(yrfile.turbmid,~TURBMID==-9999)
  
  
  
  
  coordinates(yrfile.tempmid.nona)<-~DECSLON+DECSLAT
  coordinates(yrfile.salmid.nona)<-~DECSLON+DECSLAT
  coordinates(yrfile.chlorsurf.nona)<-~DECSLON+DECSLAT
  coordinates(yrfile.oxymax.nona)<-~DECSLON+DECSLAT
  #coordinates(yrfile.turbmid.nona)<-~DECSLON+DECSLAT
  
  
  
  
  #write projection from above onto SpatialDataFrame
  proj4string(yrfile.tempmid.nona) <- crs.geo
  proj4string(yrfile.salmid.nona) <- crs.geo
  proj4string(yrfile.chlorsurf.nona) <- crs.geo
  proj4string(yrfile.oxymax.nona) <- crs.geo
  #proj4string(yrfile.turbmid.nona) <- crs.geo
  
  #string of path to write Shapefile to, referencing year-month
  strfiletmp<-paste("Enviroshp/varsep/env_varseptempmid",paste(mmm),".shp",sep="")
  strfilesal<-paste("Enviroshp/varsep/env_varsepsalmid",paste(mmm),".shp",sep="")
  strfilechl<-paste("Enviroshp/varsep/env_varsepchlamid",paste(mmm),".shp",sep="")
  strfileoxy<-paste("Enviroshp/varsep/env_varsepoxymax",paste(mmm),".shp",sep="")
  #strfileturb<-paste("Enviroshp/varsep/env_varseptempmid",paste(mmm),".shp",sep="")
  
  
  
  #write Shapefile
  writeOGR(yrfile.tempmid.nona,strfiletmp,"*",driver="ESRI Shapefile")
  writeOGR(yrfile.salmid.nona,strfilesal,"*",driver="ESRI Shapefile")
  writeOGR(yrfile.chlorsurf.nona,strfilechl,"*",driver="ESRI Shapefile")
  writeOGR(yrfile.oxymax.nona,strfileoxy,"*",driver="ESRI Shapefile")
  
  #remove dataframe for next loop iteration
  rm(yrfile.tempmid.nona)
  rm(yrfile)
  rm(yrfile.tempmid)
  rm(yrfile.chlorsurf)
  rm(yrfile.chlorsurf.nona)
  rm(yrfile.salmid.nona)
  rm(yrfile.salmid)
  rm(yrfile.oxymax)
  rm(yrfile.oxymax.nona)
  
  
  
  
}
