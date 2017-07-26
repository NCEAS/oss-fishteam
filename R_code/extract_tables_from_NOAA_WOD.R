setwd("~/oss/oss-fishteam/Data/NOAA/")

library(parallel)

library(stringr)

library(data.table)

library(dplyr)

########################################################################

#Make a function that can be repeated for each data type

#Set the number of cores to be used (parallel processing)
mc <- getOption("mc.cores", 20)

extract_loc_date=function(data_type="CTD"){
  
csv_file=paste("ocldb1499972358.22646.",data_type,".csv",sep="")
char_lines=readLines(csv_file)

#Make table with cast location and time information called loc_date

lat<-str_match(char_lines,"(Latitude)\\s+,,\\s+(\\d+.\\d+)")
lon<-str_match(char_lines,"(Longitude)\\s+,,\\s+(-\\d+.\\d+)")
year<-str_match(char_lines,"(Year)\\s+,,\\s+(\\d+)")
month<-str_match(char_lines,"(Month)\\s+,,\\s+(\\d+)")
day<-str_match(char_lines,"(Day)\\s+,,\\s+(\\d+)")
cast<-str_match(char_lines,"(CAST)\\s+,,\\s+(\\d+)")
# Save the latitude data in a matrix with col one lat and col 2 the value of the lat
lat_df<- data.frame(na.omit(lat[,-1:-2]),stringsAsFactors=FALSE)
lon_df<- data.frame(na.omit(lon[,-1:-2]),stringsAsFactors=FALSE)
year_df<-data.frame(na.omit(year[,-1:-2]),stringsAsFactors=FALSE)
month_df<-data.frame(na.omit(month[,-1:-2]),stringsAsFactors=FALSE)
day_df<-data.frame(na.omit(day[,-1:-2]),stringsAsFactors=FALSE)
cast_df<-data.frame(na.omit(cast[,-1:-2]),stringsAsFactors=FALSE)
colnames(lat_df)<-c("lat")
colnames(lon_df)<-c("lon")
colnames(year_df)<-c("year")
colnames(month_df)<-c("month")
colnames(day_df)<-c("day")
colnames(cast_df)<-c("cast")

# Combine the cast, location and date data into one table
loc_date<-cbind(cast_df,lat_df,lon_df,year_df,month_df,day_df)
return(loc_date)
}
  

########################################################################
extract_tables=function(data_type="CTD"){
  
csv_file=paste("ocldb1499972358.22646.",data_type,".csv",sep="")
char_lines=readLines(csv_file)
  
#Extract beginning and ending lines for each table

beg=which(str_detect(char_lines, "^VARIABLES*"))
end=which(str_detect(char_lines, "END OF VARIABLES*"))
casts=which(str_detect(char_lines, "^CAST*"))
length(beg)

########################################################################

#Extract each table and add unique id column (Meas_ID) and cast column (Cast)

#Make a list to store the output temporarily (great for debugging)
table_list=vector("list", length(beg)) 

#Make a function to read in using data.tables() in case there is an error in fread()
alternative_read=function(i,col_numbers=col_numbers){
  alt_data=read.table(csv_file,skip=beg[i]+2,
                      nrows=end[i]-beg[i]-3,sep=',',header=FALSE)
  alt_data=alt_data[,col_numbers]
  return(alt_data)
}

#Record system time
t = Sys.time()
data_extract=mclapply(1:length(beg),function(i){
  
  #Figure out how many columns
  ncol_raw_data=length(fread(csv_file,skip=beg[i]+2,
                             nrows=1,sep=',',header=FALSE))
  
  #Store which ones to extract
  col_numbers=c(1,seq(2,ncol_raw_data-1,3))
  
  #Pulls out the data
  data=tryCatch(fread(csv_file,skip=beg[i]+2,
                      nrows=end[i]-beg[i]-3,sep=',',header=FALSE,
                      select=col_numbers),error = function(e) alternative_read(i,col_numbers))
  
  #Pulls out the header
  header=stringi::stri_extract_all_words(char_lines[beg[i]])[[1]][seq(2,ncol_raw_data-1,3)]
  
  #Adds the cast number
  data$Cast=rep(str_extract(char_lines[casts[i]],"[0-9]+"),nrow(data))
  
  #Adds header names
  names(data)=c("Meas_ID",header,"Cast")
  
  return(data)
})
print(Sys.time()-t) #Prints out run time

return(data_extract)
}

########################################################################
loc_date_CTD=extract_loc_date()
table_list_CTD=extract_tables()

loc_date_XBT=extract_loc_date(data_type="XBT")
table_list_XBT=extract_tables(data_type="XBT")

loc_date_PFL=extract_loc_date(data_type="PFL")
table_list_PFL=extract_tables(data_type="PFL")

save(table_list_CTD, file="./table_list_CTD.Rdata") #saves variable table_list to file
save(table_list_XBT, file="./table_list_XBT.Rdata") 
save(table_list_PFL, file="./table_list_PFL.Rdata") 

load("table_list_CTD.Rdata")
load("table_list_XBT.Rdata")
load("table_list_PFL.Rdata")

NOAA_env_data_CTD=rbindlist(table_list_CTD,fill=TRUE)
NOAA_env_data_XBT=rbindlist(table_list_XBT,fill=TRUE)
NOAA_env_data_PFL=rbindlist(table_list_PFL,fill=TRUE)

NOAA_env_data_CTD$Type=rep("CTD",nrow(NOAA_env_data_CTD))
NOAA_env_data_XBT$Type=rep("XBT",nrow(NOAA_env_data_XBT))
NOAA_env_data_PFL$Type=rep("PFL",nrow(NOAA_env_data_PFL))

NOAA_env_data=rbind(NOAA_env_data_CTD,NOAA_env_data_XBT,fill=TRUE)
NOAA_env_data=rbind(NOAA_env_data,NOAA_env_data_PFL,fill=TRUE)

str(NOAA_env_data)
head(NOAA_env_data)

save(NOAA_env_data, file="NOAA_env_data.Rdata") 

load("NOAA_env_data.Rdata")

NOAA_env_data[,2] <- lapply(NOAA_env_data[,2], function(x) as.numeric(as.character(x)))
NOAA_env_data[,3] <- lapply(NOAA_env_data[,3], function(x) as.numeric(as.character(x)))
NOAA_env_data[,4] <- lapply(NOAA_env_data[,4], function(x) as.numeric(as.character(x)))
NOAA_env_data[,5] <- lapply(NOAA_env_data[,5], function(x) as.numeric(as.character(x)))
NOAA_env_data[,6] <- lapply(NOAA_env_data[,6], function(x) as.numeric(as.character(x)))
NOAA_env_data[,7] <- lapply(NOAA_env_data[,7], function(x) as.numeric(as.character(x)))
NOAA_env_data[,8] <- lapply(NOAA_env_data[,8], function(x) as.numeric(as.character(x)))
NOAA_env_data[,9] <- lapply(NOAA_env_data[,9], function(x) as.numeric(as.character(x)))
NOAA_env_data[,10] <- lapply(NOAA_env_data[,10], function(x) as.numeric(as.character(x)))

save(NOAA_env_data, file="NOAA_env_data.Rdata") 

length(unique(NOAA_env_data$Cast[which(is.na(NOAA_env_data$Depth))]))
#2712

bad_data=NOAA_env_data[which(is.na(NOAA_env_data$Temperatur)),]

length(unique(bad_data$Cast[bad_data$Type=="CTD"]))
#844
length(unique(bad_data$Cast[bad_data$Type=="XBT"]))
#1873
length(unique(bad_data$Cast[bad_data$Type=="PFL"]))
#27

length(unique(NOAA_env_data$Cast[which(!is.na(NOAA_env_data$Temperatur))]))
#71716

(844+1873+27)/((844+1873+27)+71716)
#Casts deleted: 2744
#0.036852

NOAA_env_data_clean=NOAA_env_data[which(!is.na(NOAA_env_data$Temperatur)),]
nrow(NOAA_env_data_clean)

save(NOAA_env_data_clean, file="NOAA_env_data_clean.Rdata") 

write.csv(NOAA_env_data_clean,"NOAA_env_data_clean.csv")

#3236993 CTD
#15837546 PFL


NOAA_env_data_clean=read.csv("NOAA_env_data_clean.csv",sep=",",header=TRUE)

##########################################################################

NOAA.data=read.csv("env_loc_date_depth_all.csv")
head(NOAA.data)
str(NOAA.data)
nrow(NOAA.data)

b=as.numeric(as.character(NOAA.data[,6]))
NOAA.data[,6]=b
c=as.numeric(as.character(NOAA.data[,10]))
NOAA.data[,10]=c

head(NOAA.data)
NOAA.data$Temperatur[NOAA.data$Temperatur<0]=NA
NOAA.data$Temperatur[NOAA.data$Temperatur>40]=NA

NOAA.data.clean=NOAA.data[!is.na(NOAA.data$Temperatur),]
nrow(NOAA.data.clean)
NOAA.data.clean=NOAA.data.clean[NOAA.data.clean$month<8&NOAA.data.clean$month>4,]
nrow(NOAA.data.clean)
NOAA.data.clean=NOAA.data.clean[NOAA.data.clean$year>=1980,]
nrow(NOAA.data.clean)
casts=unique(NOAA.data.clean$Cast)
length(casts)
final.data.frame=data.frame("cast"=casts,"temp"=rep(NA,length(casts)),
                            "sal"=rep(NA,length(casts)),"do"=rep(NA,length(casts)),
                            "chl"=rep(NA,length(casts)),"lat"=rep(NA,length(casts)),
                            "lon"=rep(NA,length(casts)),"year"=rep(NA,length(casts)),
                            "month"=rep(NA,length(casts)),"day"=rep(NA,length(casts)))
head(final.data.frame)
head(NOAA.data.clean)
str(NOAA.data.clean)
summary(NOAA.data.clean$Chlorophyl)

NOAA.data.clean$Oxygen[NOAA.data.clean$Oxygen<0]=NA
NOAA.data.clean$Oxygen[NOAA.data.clean$Oxygen>16]=NA

NOAA.data.clean$Chlorophyl[NOAA.data.clean$Chlorophyl<0]=NA
NOAA.data.clean$Chlorophyl[NOAA.data.clean$Chlorophyl>100]=NA

library(dplyr)
cols.left=NOAA.data.clean %>% group_by(Cast) %>% dplyr::summarize(lat=mean(lat,na.rm=TRUE),lon=mean(lon,na.rm=TRUE),year=mean(year,na.rm=TRUE),month=mean(month,na.rm=TRUE),day=mean(day,na.rm=TRUE),temp=mean(Temperatur,na.rm=TRUE),sal=mean(Salinity,na.rm=TRUE))
cols.max=NOAA.data.clean %>% group_by(Cast) %>% top_n(1,Depth)
cols.min=NOAA.data.clean %>% group_by(Cast) %>% top_n(-1,Depth)
col.right=cols.max[,c(2,5,8,13,27)]
col.chl=cols.min[,c(2,10)]
col.right2=left_join(col.right,col.chl,by="Cast")
predict.data=left_join(cols.left,col.right2,by="Cast")
head(predict.data)

#O2 ml l-1
#Chl ug l-1

write.csv(predict.data,"predict_data.csv")
getwd()
?geom_point()
