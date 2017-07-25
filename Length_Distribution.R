#Using datatables GLFREC and INVREC, determine what the length frequency of red snapper is caught with gear (GEAR_TYPE) shrimp trawl (ST)
#Not looking at sex of fish because "male and female red snapper grow rapidly and at about the same rate until about 8 years old and about 28 inches in length."- Seagrant 

rm(list=ls()) #clear workspace
library(tidyverse)
setwd("~/oss/Synthesis")#Merge the tables
INVREC<- read.table("Seamap/INVREC.txt", sep=",", stringsAsFactors = FALSE, header = TRUE)
GLFREC<- read.table("Seamap/GLFREC.txt", sep=",", stringsAsFactors = FALSE, header = TRUE)

INV<- INVREC %>% select(CRUISEID, STATIONID, INVRECID, GEAR_TYPE, GEAR_SIZE, MESH_SIZE) %>% filter(GEAR_TYPE=="ST") #Remove some columns and filter for Shrimp Trawl

GLF<- GLFREC %>% select(CRUISEID, STATIONID, GLFID, SPEC_GLF, LEN_GLF, MEASCD_GLF) %>% filter(SPEC_GLF=="CAMPEC") #Remove some columns and filter for red snapper

length(unique(INV$STATIONID))==length(INV$STATIONID) #Station ID is unique after filtering in tows of INV

length_freq<- left_join(select(GLF, CRUISEID, STATIONID, LEN_GLF, MEASCD_GLF), select(INV, STATIONID, GEAR_TYPE, GEAR_SIZE, MESH_SIZE), by="STATIONID")

length(length_freq$STATIONID)==length(GLF$STATIONID) #New size matches GLF so not repating length values. Merge successful.

#Check freqency of each gear type
length_freq<- length_freq %>% mutate(GEAR_MERGE=paste(GEAR_TYPE,GEAR_SIZE,MESH_SIZE, sep="_"))
Gear_Counts<- length_freq %>% count(GEAR_MERGE)
Percent_ST401.63<- ((filter(Gear_Counts, GEAR_MERGE=="ST_40_1.63") %>% select(n))/length(length_freq$STATIONID)*100)[[1]]
      # 91.78001     Almost 92% of red snapper catches are with Shrimp trawl of size 40 and mesh size 1.63

#Extract most common gear type and remove ones that are size of NA
length_freq_gs40_ms163<-length_freq %>% filter(GEAR_SIZE==40, MESH_SIZE==1.63) %>% filter(!is.na(LEN_GLF)) #Only looking at Shrimp trawl, GEAR_SIZE=40 and MESH_SIZE=1.63


#What type of lengths are they measuring
find(length_freq_gs40_ms163$MEASCD_GLF==1)

#Von Bert Size at Age
len=c(1:5) #dimension variable of typical length at age
for (t in 1:5){
  len[t]=856.4*(1-exp(-.19*(t--0.395))) #Von Bert growth, coefficients from Brad's website
  rm(t)
}

Lm= 230 #Length at maturity Red Snapper
        #Maturity obtained at year 2, but estimate for size at year 2 is greater than 230cm

#Frequency distribution of Red Snapper

qplot(length_freq_gs40_ms163$LEN_GLF,
geom="histogram",
binwidth = 50,
main = "Red Snapper Length Frequency",
xlab = "Length (mm)",
fill=I("blue"),
col=I("red"),
alpha=I(.2),
xlim=c(0,400))
data.fram

unique(length_freq_gs40_ms163$MEASCD_GLF)


data.frame(matrix(NA,4,1), row.names = c("Fork", "Standard", "Total", "NA"), colnames("n"))

#Questions
  #Find indices
  #How to rename axes, add more points to plot, change axes to percent of total in bin







