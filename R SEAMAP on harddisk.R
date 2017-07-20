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
# the following is to convert separate date columns to a string: toDate <- function(dates1) {ISOdate(year, month, day)}
# below what we'll use: convert a data string to separate columns. First the function:
toNumerics <- function(Date) {
  stopifnot(inherits(Date, c("Date", "POSIXt")))
  day <- as.numeric(strftime(Date, format = "%d"))
  month <- as.numeric(strftime(Date, format = "%m"))
  year <- as.numeric(strftime(Date, format = "%Y"))
  list(year = year, month = month, day = day)
}

# now the code that uses that function:
starec$Date <- as.POSIXct(starec$MO_DAY_YR,format='%m/%d/%Y')
starec$Time <- sapply(strsplit(as.character(starec$MO_DAY_YR), " "), "[", 2)
testdates<-data.frame(toNumerics(starec$Date))
starec$year<-testdates$year
starec$month<-testdates$month
starec$day<-testdates$day

# select the columns from starec that we find useful:
location <- dplyr::select(starec,STATIONID,CRUISEID,MO_DAY_YR,DECSLAT,DECSLON,DECELAT,DECELON,HAULVALUE,year,month,day)
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

# now complete the rows that have NA (i.e. a shrimp trawl without red snapper in the catch, name the species red snapper like rest, and the abundnace zero.)
redsnapper2$GENUS_BGS <-"LUTJANU"
redsnapper2$SPEC_BGS <-"CAMPEC"
redsnapper2[is.na(redsnapper2$CNTEXP),"CNTEXP"] <-0

#control shift C to make a block of text a comment
#the following code is to test a few different 
# things to see what combination of mesh size and gear size is used. 
# Gear size 40 and mesh size 1.63 is prevalent over all years, we're going to 
# test spatially in QGIS what the differecne is between choosing all gear and mesh sizes 
# theme(output in the monitoring advice analysis: what gaps do using differetn gears create?)'

unique(redsnapper2$GEAR_SIZE)
summary(redsnapper2$GEAR_SIZE)
counts<-table(redsnapper2$GEAR_SIZE)
barplot(counts)

summary(redsnapper2$MESH_SIZE)
counts<-table(redsnapper2$MESH_SIZE)
barplot(counts)

#remove the lines that don't have year, mesh size of gear size completed
redsnapper_allyears <- na.omit(subset(redsnapper2,select=c(year,MESH_SIZE,GEAR_SIZE)))
plot(redsnapper2$year,redsnapper2$GEAR_SIZE)
plot(redsnapper2$GEAR_SIZE,redsnapper2$MESH_SIZE)

redsnapper_gear <-data.frame(redsnapper_allyears$MESH_SIZE,redsnapper_allyears$GEAR_SIZE)
redsnapper_gear$interaction<-(as.character(interaction(redsnapper_gear)))
counts<-table(redsnapper_gear$interaction)
barplot(counts)

#above datasets only have 3 valiables,
# now run omit NA on the whole dataset so that we have have all columns 
# in a dataset without NA's
redsnapper3 <- na.omit(redsnapper2)
#now code to keep only gear size 40 mesh size 1.63
#the code can be commented out to run it all:

redsnapper_samegear <- dplyr::filter(redsnapper3,MESH_SIZE=="1.63",GEAR_SIZE=="40")

#we only lose 5442 observations of the 27912 by focusing on grear and meash combo of 40, 1.63
#so we are probably in good shape sticking to gear type ST, gear size 40 and mesh size 1.63

#Now calculate CPUE where one unit of effort is one minute trawled

redsnapper_samegear$CPUE <-redsnapper_samegear$CNTEXP/redsnapper_samegear$MIN_FISH
write_csv(redsnapper_samegear,"Red_SnapperCPUE.csv")




