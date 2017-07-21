setwd("~/oss/NGOMEXcastdata")

library(parallel)

library(stringr)

library(data.table)

library(dplyr)

########################################################################

#Make table with cast location and time information called loc_date

char_lines=readLines("ocldb1499972358.22646.OSD.csv")

lat<-str_match(char_lines,"(Latitude)\\s+,,\\s*(\\d+.\\d*)")
lon<-str_match(char_lines,"(Longitude)\\s+,,\\s*(-\\d+.\\d*)")
year<-str_match(char_lines,"(Year)\\s+,,\\s*(\\d*)")
month<-str_match(char_lines,"(Month)\\s+,,\\s*(\\d*)")
day<-str_match(char_lines,"(Day)\\s+,,\\s*(\\d*)")
cast<-str_match(char_lines,"(CAST)\\s+,,\\s*(\\d*)")
beg<-str_match(char_lines,"VARIABLES ,")
end<-str_match(char_lines,"END OF VARIABLES SECTION")
# Save the latitude data in a matrix with col one lat and col 2 the value of the lat
lat_df<- data.frame(na.omit(lat[,-1:-2]),stringsAsFactors=FALSE)
lon_df<- data.frame(na.omit(lon[,-1:-2]),stringsAsFactors=FALSE)
year_df<-data.frame(na.omit(year[,-1:-2]),stringsAsFactors=FALSE)
month_df<-data.frame(na.omit(month[,-1:-2]),stringsAsFactors=FALSE)
day_df<-data.frame(na.omit(day[,-1:-2]),stringsAsFactors=FALSE)
cast_df<-data.frame(na.omit(cast[,-1:-2]),stringsAsFactors=FALSE)
beg_df<-data.frame(na.omit(beg),stringsAsFactors=FALSE)
end_df<-data.frame(na.omit(end),stringsAsFactors=FALSE)

colnames(lat_df)<-c("lat")
colnames(lon_df)<-c("lon")
colnames(year_df)<-c("year")
colnames(month_df)<-c("month")
colnames(day_df)<-c("day")
colnames(cast_df)<-c("cast")
colnames(beg_df)<-c("beg")
colnames(end_df)<-c("end")

# Combine the cast, location and date data into one table
loc_date<-cbind(cast_df,lat_df,lon_df,year_df,month_df,day_df)
head(loc_date)
########################################################################

#Extract beginning and ending lines for each table

beg=which(str_detect(char_lines, "VARIABLES"))
end=which(str_detect(char_lines, "END*"))

L<-length(beg)

########################################################################

#Extract each table and add unique id column (Meas_ID) and cast column (Cast)

#Initial options to...

#Set the number of cores to be used (parallel processing)
mc <- getOption("mc.cores", 10)

#Make a list to store the output temporarily (great for debugging)
table_list_CSV=vector("list", length(beg)) 

#Make a function to read in using data.tables() in case there is an error in fread()
alternative_read=function(i,col){
  alt_data=read.table("ocldb1499972358.22646.MRB.csv",skip=beg[i]+2,
                      nrows=end[i]-beg[i]-3,sep=',',header=FALSE)
  alt_data=alt_data[,col]
  return(alt_data)
}
measure<- data.frame(flag=integer(),
                     Depth=character(), 
                     Temperatur=character(),
                     Cast=integer(),
                     stringsAsFactors=FALSE)
#Record system time
t = Sys.time()
Seq<-1:1:L
 
for (i in Seq) {
  print(i)
  #Figure out how many columns
  ncol_raw_data=length(fread("ocldb1499972358.22646.MRB.csv",skip=beg[i]+2,
                             nrows=1,sep=',',header=FALSE,data.table = FALSE))
  
  #Store which ones to extract
  col_numbers=c(1,seq(2,ncol_raw_data-1,3))
  
  #Pulls out the data
  data<-read.table("ocldb1499972358.22646.MRB.csv",skip=beg[i]+2,
                   nrows=end[i]-beg[i]-3,sep=',',header=FALSE)
  data=data[,col_numbers]


  #Pulls out the header
  header=stringi::stri_extract_all_words(char_lines[beg[i]])[[1]][seq(2,ncol_raw_data-1,3)]
  
  #find cast number for this measurement
  if (i<=2){
   castid<-cast_df$cast[i] 
  }
  else{
    castid<-max(which(str_detect(char_lines[(begind[i]-50):(begind[i])], "CAST")))
  }
  #Adds the cast number
  data$Cast=rep(castid,times=nrow(data))
  
  #Adds header names
  names(data)=c("flag",header,"Cast")
  

  measure<-rbind.fill(measure,data)
  
}
 
Sys.time()-t #Prints out run time


#save(table_list_CSV, "./table_list_MRB.Rdata") #saves variable table_list to file
# load("table_list.Rdata") #loads saved variable into environment


# detectCores()
# 
