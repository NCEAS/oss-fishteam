## Seamap code to clean and join  locational data and environmental data from SEAMAP product
## http://www.gsmfc.org/seamap.php 
## Code writer/creator: Alexander E. Sacco (c) current version: July 2017

#change to your own working directory...
setwd("/home/sacco/seamap2/data")
#setwd("C:/Users/Alex/Desktop/nceas.oss.2017/Group.project/Data.sets")

#Pull in required packages
#library(ggplot2)
#library(ggmap)
#library(maps)
#library(mapdata)
#library(rworldmap)
library(tidyverse)

#######################################################################################
## Read in data and join tables for environmental data and Biological data

#read in environmental and locational data

env<-read.csv("env.IDs.update.csv",stringsAsFactors=FALSE)
loc<-read.csv("loc.IDs.update.csv",stringsAsFactors=FALSE)

sm.env.all<-dplyr::left_join(loc,env,by="STATIONID","CRUISEID",na_matches="never")

#remove na in lat/lon
sm.env.nona<-subset(sm.env.all,!is.na(sm.env.all$DECSLAT)) 
#remove zeros in lat/lon
sm.env.nona0<-filter(sm.env.nona,!DECSLAT==0)

test<-sm.env.nona0

test$Date <- as.POSIXct(test$MO_DAY_YR,format='%m/%d/%Y')
test$Time <- sapply(strsplit(as.character(test$MO_DAY_YR), " "), "[", 2)
testdates<-data.frame(toNumerics(test$Date))
test$year<-testdates$year
test$month<-testdates$month
test$day<-testdates$day

write.csv(test,"testenviroymd.csv")


#change missing values "na" to -9999
testnona<-test
testnona1<-select(testnona,-Date,-MO_DAY_YR,-MO_DAY_YR.D)

#change all factor columns to characters so you can use it in GIS symbology for visualization
cols.num <- c("CTDFILE", "COMENV", "STA_LOC", "CLD_TYPE", 
              "HAULVALUE", "END_DATE", "START_DATE", "DBTYPE", "DATA_CODE", "COMSTAT", "GEARS", "E_LONH", "E_LATH", "S_LATH", "S_LONH", "S_STA_NO")
testnona1[cols.num] <- sapply(testnona1[cols.num],as.character)
sapply(testnona1, class)

#now data are all characters or numeric, so we can change na to -9999

testnona1[is.na(testnona1)] <- -9999

#now export dataset without any NAs so we can use in GIS
write.csv(testnona1,"testenviroymdNOna.csv")

envdata<-testnona1
rm(testnona1)
###################################################################
## pull each unique month-year of data and create CSV file for each
envdata<-read.csv("testenviroymdNOna.csv")
moyr<-select(envdata,year,month)
moyr$interaction<-(as.character(interaction(moyr,sep="")))
envdata$moyr<-(moyr$interaction)

countr<-0
require(raster)
library(sp)
library(rgdal)

envdata1<-filter(envdata,!DECSLAT==-9999)

#set up WGS84 projection for Shapefiles
crs.geo<-CRS("+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")  #define projection

#initialize loop check
countr<-0
for (moyrl in sort(unique(moyr$interaction))) {
  #print year-mo
  print(moyrl)
  #iterative count to check the loop
  countr=countr+1
  #filter by year-month and write to new dataframe
  xyzfile <- dplyr::filter(envdata1,year==as.numeric(substr(moyrl,1,4)),month==as.numeric(substr(moyrl,5,6)))
  #print loop check to verify filter worked
  print(countr)
  
  #write corrdinates to filtered dataframe and create SpatialDataFrame, referencing (lon,lat)
  coordinates(xyzfile)<-~DECSLON+DECSLAT
  
  #write projection from above onto SpatialDataFrame
  proj4string(xyzfile) <- crs.geo
  #string of path to write Shapefile to, referencing year-month
  strfile<-paste("Enviroshp/env_all",paste(moyrl),".shp",sep="")
  
  #write Shapefile
  writeOGR(xyzfile,strfile,"*",driver="ESRI Shapefile")
  #remove dataframe for next loop iteration
  rm(xyzfile)
}

#zip files into an archive
zip(zipfile = 'envshpzip', files = 'Enviroshp/')

##########################################################################################
## Everything below here may have been used but didn't contribute to the final data output
##########################################################################################








#convert dms to dd - Don't need this anymore
#lat.info$S_dd<-lat.info$S_LATD+(lat.info$S_LATM/60)


####################################
## Don't need this
#CRUISEID

#crstID<-read.csv("CRUISESTATIONUNIQUE.csv",header=TRUE)
#crstID<-mutate(crstID,TRAWLID=paste(STATIONID,CRUISEID,sep="_"))



# #merge the two datasets
# locmerged_data<-latstar %>% right_join(crstID,by="CRUISEID","STATIONID")
# write.csv(locmerged_data,"loc.IDs.csv")
# rm(crstID)
# rm(latstar)
# 
# loc2<-crstID %>% right_join(envrecsml,by="CRUISEID","STATIONID")



#locenvmrg_data<-locmerged_data %>% right_join(loc2,by="TRAWLID")

# ##############################
# 
# #joining lat/lon data with environmental data collected in SEAMAP data
# lat.info2<-lat.info %>% select(STATIONID:HAULVALUE)
# lat.env.info<-lat.info2 %>% right_join(lat.info2,env.info,by="CRUISEID")
# 
# #remove unneeded data for now
# #rm(lat.info)
# #rm(lat.info2)
# #rm(env.info)
# 
# #write the environmental data (with locations) to CSV and clear it
# write.csv(lat.env.info,"seamap.env.locsincl.csv")
# #rm(lat.env.info)
# 
# #garbage collection to free up Windows memory after clearing large datsets
# gc()
# 
# #read in biological and locational data
# bio.info<-read.table("BGSREC.txt",header=TRUE,sep=",")
# 
# 
# #filter out various species
# #LUTJANU CAMPEC --> Red Snaper
# campec<-filter(bio.info,SPEC_BGS=="CAMPEC")
# #pink shrimp Penaeus duorar
# duorar<-filter(bio.info,SPEC_BGS=="DUORAR")
# #brown shrimp Farfantepenaeus aztecus
# aztecus<-filter(bio.info,SPEC_BGS=="AZTECU")
# #white shrimp Penaeus setiferus
# setife<-filter(bio.info,SPEC_BGS=="SETIFE")
# 
# #rm(bio.info)
# 
# lat.info<-read.table("STAREC.txt",header=TRUE,sep=",")
# 
# #join lat/lon with bio count data collected in SEAMAP
# lat.info2<-lat.info %>% select(STATIONID:HAULVALUE)
# #rm(lat.info)
# 
# lat.aztecus<-lat.info2 %>% right_join(lat.info2,aztecus,by="CRUISEID")
# write.csv(lat.aztecus,"seamap.aztecus.locsincl.csv")
# #rm(lat.aztecus)
# #rm(aztecus)
# 
# lat.campec<-lat.info2 %>% right_join(lat.info2,campec,by="CRUISEID")
# write.csv(lat.campec,"seamap.campec.locsincl.csv")
# #rm(lat.campec)
# #rm(campec)
# 
# lat.duorar<-lat.info2 %>% right_join(lat.info2,duorar,by="CRUISEID")
# write.csv(lat.duorar,"seamap.duorar.locsincl.csv")
# #rm(lat.duorar)
# #rm(duorar)
# 
# lat.setife<-lat.info2 %>% right_join(lat.info2,setife,by="CRUISEID")
# write.csv(lat.setife,"seamap.setife.locsincl.csv")
# #rm(lat.setife)
# #rm(setife)
# 
# 
# 
# #rm(lat.info)
# #rm(lat.info2)
# #rm(bio.info)
# 
# #write the bio data (with locations) to CSV and clear it
# write.csv(lat.bio.info,"seamap.bio.locsincl.csv")
# #rm(lat.bio.info)
# 
# #garbage collection to free up Windows memory after clearing large datsets
# gc()
# 
# 
# #Pulling data for specific species
# #######################################################################################
# #read in bio with locs csv lth fo
# 
# 
# 
# #faster than above for some reason, but still seems to get the job done
# #lat.env.info<-lat.info2 %>% left_join(env.info,by="STATIONID")
# 
# 
# 
# 
# ## Mapping data in R
# #######################################################################################
# #One way to create a map in R
# newmap<-getMap(resolution="low")
# #plotting the map and focusing the bounding borever thte past.present.future.comaterehbounding_box_first_last_every_
# #lnew<getMap(resolut the forever used remove obojects from a specific environemtnation of both x
# plot(newmap,xlim=c(-99,-70),ylim=c(10,35),asp=1)
# #plotting lat/lon points
# points(lat.env.info$DECSLON,lat.env.info$DECSLAT)
# 
# 
# #########
# #new method to plot a map using ggmap and google maps pulled from the internet
# #create bounding box from vector list of lat/lons
# 
# bbox1 <- make_bbox(lat = LATITUDE.x, lon = LONGITUDE.x, data = ctdenvmerged_data)
# 
# 
# 
# # grab the maps from google
# terrmap <- get_map(location = bbox, source = "google", maptype = "terrain")
# 
# # plot the points and color them by sector
# 
# #####################################
# starec<-read.table("STAREC.txt",header=TRUE,sep=",")
# env.info<-read.table("ENVREC.txt",header=TRUE,sep=",")
# test<-dplyr::left_join(starec,env.info,by="STATIONID")
# test$MO_DAY_YR.D<-as.Date(test$MO_DAY_YR,format="%m-%d-%Y")
# write.csv(test,"testenvirodata.csv")

# 
# ########################################
# #compare ENVRECID and CTDCAST_ID to see if everything is summarized
# envrec<-read.table("ENVREC.txt",header=TRUE,sep=",",stringsAsFactors = FALSE)
# ctdcastcomp<-ctdcastrec %>% select(CRUISEID,CTDCAST_ID)
# envcomp<-envrec %>% select(ENVRECID,CRUISEID)
# 
# 
# ctdenvmerged_data<-latctd%>% right_join(envrec,by="CRUISEID")
# 
# sum(is.na(ctdcastenvcomp$ENVRECID))
# sum(is.na(ctdcastenvcomp$CTDCAST_ID))
# sum(is.na(ctdcastenvcomp$CRUISEID))
# #for each ENVRECID there's one CTDCAST_ID
# #################
# 
# starec<-read.table("STAREC.txt",header=TRUE,sep=",")
# cruises<-read.table("CRUISES.txt",header=TRUE,sep=",")
# stareccrst<-starec %>% select(CRUISEID,STATIONID)
# cruisescrst<-cruises %>% select(CRUISEID)
# crstidmrg1<-cruisescrst %>% right_join(stareccrst,by="CRUISEID")
# crstidmrg1$TRAWLID<-1:56547

#check for duplicate TRAWLIDs
#loc.ids.update<-subset(loc.ids.sml,!duplicated(TRAWLID))
