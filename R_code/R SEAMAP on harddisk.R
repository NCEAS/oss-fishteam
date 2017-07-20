setwd("/Users/kimdemutsert")
cruises <-read.table("CRUISES.txt", header=TRUE,sep=",")
cruises
newbiocodesbig <-read.table("NEWBIOCODESBIG.txt", header=TRUE,sep=",")
ctdrec <-read.table("CTDREC.txt", header=TRUE,sep=",")
vessels <-read.table("VESSELS.txt", header=TRUE,sep=",")
shrrec <-read.table("SHRREC.txt", header=TRUE,sep=",")
invrec <-read.table("INVREC.txt", header=TRUE,sep=",")
istrec <-read.table("ISTREC.txt", header=TRUE,sep=",")
envrec <-read.table("ENVREC.txt", header=TRUE,sep=",")
starec <-read.table("STAREC.txt", header=TRUE,sep=",")
bgsrec <-read.table("BGSREC.txt", header=TRUE,sep=",")
ctdcastrec <-read.table("CTDCASTREC.txt", header=TRUE,sep=",")
glfrec <-read.table("GLFREC.txt", header=TRUE,sep=",")

library(tidyverse)
fishdata <- dplyr::select(bgsrec,STATIONID,CRUISEID,GENUS_BGS,SPEC_BGS,CNTEXP)
#fishdatasubset <- fishdata %>% filter(CRUISEID>1055)
## check if CRUISEID is unique
checkunique <- fishdata %>% distinct(CRUISEID,STATIONID)
## it is not, there are multiple stations per cruiseid, so need to create a TRAWLID which is a merge of STATIONID and CRUISEID
# trawlID does not add more information than STATIONID does, so stick to STATIONID.
location <- dplyr::select(starec,STATIONID,CRUISEID,MO_DAY_YR,DECSLAT,DECSLON,DECELAT,DECELON,HAULVALUE)
# don't use this: location2 <- dplyr::mutate(location,TRAWLID=paste(STATIONID,CRUISEID,sep="_"))

#try and join with invrec to get the right gear used. after filter on ST which is shrimp trawl, which is how mostly red snapper are collected, and know when more trawls are collected per gear.
gear <- dplyr::select(invrec,STATIONID, CRUISEID, GEAR_SIZE, GEAR_TYPE, MESH_SIZE, MIN_FISH)
gearshrimptrawl <- dplyr::filter(gear, GEAR_TYPE=="ST")
locationgear <- dplyr::inner_join(location,select(gearshrimptrawl,-CRUISEID),by="STATIONID")
#fishdatasubset2 <- dplyr::mutate(fishdatasubset,TRAWLID=paste(STATIONID,CRUISEID,sep="_"))
#fishdata2 <- dplyr::mutate(fishdata,TRAWLID=paste(STATIONID,CRUISEID,sep="_"))
joinbytrawlfish <- dplyr::left_join(locationgear,select(fishdata,-CRUISEID),by="STATIONID")
##this is the merged dataset we want. To now select out the red snapper only, we can select on red snapper in fishdata, then left join the merged dataset (joinbytrawlfish) with red snapper, and we'll gte the trawls without red snapper catches as well.

redsnapper <- dplyr::filter(fishdata,GENUS_BGS=="LUTJANU",SPEC_BGS=="CAMPEC")
redsnapper2 <- dplyr::left_join(locationgear,select(redsnapper,-CRUISEID),by="STATIONID")
##I now have the red snapper catch (in abundance), included also trawls that didn't catch red snapper. now select on haul, and remove all the bad hauls.
summary(redsnapper2$HAULVALUE)

redsnapper2 <- dplyr::filter(redsnapper2,HAULVALUE!="B")
summary(redsnapper2$HAULVALUE)
#this is the correct red snapper file, write ot csv and upload to googledrive
write_csv(redsnapper2,"Red_Snapper.csv")

