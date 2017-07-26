#Using datatables GLFREC and INVREC, determine what the length frequency of red snapper is caught with gear (GEAR_TYPE) shrimp trawl (ST)
#Not looking at sex of fish because "male and female red snapper grow rapidly and at about the same rate until about 8 years old and about 28 inches in length."- Seagrant 

rm(list=ls()) #clear workspace
library(tidyverse)
setwd("~/oss/Synthesis")#Merge the tables
INVREC<- read.table("Seamap/INVREC.txt", sep=",", stringsAsFactors = FALSE, header = TRUE)
GLFREC<- read.table("Seamap/GLFREC.txt", sep=",", stringsAsFactors = FALSE, header = TRUE)

INV<- INVREC %>% select(CRUISEID, STATIONID, INVRECID, GEAR_TYPE, GEAR_SIZE, MESH_SIZE) %>% filter(GEAR_TYPE=="ST") #Remove some columns and filter for Shrimp Trawl

GLF<- GLFREC %>% select(CRUISEID, STATIONID, GLFID, SPEC_GLF, LEN_GLF, MEASCD_GLF) %>% filter(SPEC_GLF=="CAMPEC") #Remove some columns and filter for red snapper

#Make Sure these are all true!
length(unique(INV$STATIONID))==length(INV$STATIONID) #Station ID is unique after filtering in tows of INV
length_freq<- left_join(select(GLF, CRUISEID, STATIONID, LEN_GLF, MEASCD_GLF), select(INV, STATIONID, GEAR_TYPE, GEAR_SIZE, MESH_SIZE), by="STATIONID")
length(length_freq$STATIONID)==length(GLF$STATIONID) #New size matches GLF so not repating length values. Merge successful.

#Check freqency of each gear type
length_freq<- length_freq %>% mutate(GEAR_MERGE=paste(GEAR_TYPE,GEAR_SIZE,MESH_SIZE, sep="_"))
(table(length_freq$GEAR_MERGE)/length(length_freq$GEAR_MERGE))*100 #Table of gear frequency in percent
# NA_NA_NA  ST_16_0.25   ST_20_1.5  ST_40_1.58  ST_40_1.63  ST_40_1.65 
# 5.53870931  0.00880758  1.43689369  0.90340602 91.78001183  0.33217157 
# Almost 92% of red snapper catches are with Shrimp trawl of size 40 and mesh size 1.63


#Extract most common gear type and remove ones that are size of NA
length_freq_gs40_ms163<-length_freq %>% filter(GEAR_SIZE==40, MESH_SIZE==1.63) %>% filter(!is.na(LEN_GLF)) #Only looking at Shrimp trawl, GEAR_SIZE=40 and MESH_SIZE=1.63


#What type of lengths are they measuring
meas_type<- data.frame(table(length_freq_gs40_ms163$MEASCD_GLF))
meas_type<- meas_type %>% mutate(meas= "empty")

meas_type$meas[meas_type$Var1==1]="Fork"
meas_type$meas[meas_type$Var1==51]="Fork"
meas_type$meas[meas_type$Var1==2]="Standard"
meas_type$meas[meas_type$Var1==11]="Total"
meas_type$meas[meas_type$Var1==18]="Total"
meas_type$meas[meas_type$Var1==53]="Total"
meas_freq<- meas_type %>% group_by(meas) %>% summarize(Freq=sum(Freq))
meas_freq<- meas_freq %>% mutate(percent=(Freq/sum(Freq))*100)

# meas  Freq     percent
# <chr> <int>       <dbl>
#   1     Fork 70243 96.32489064
# 2 Standard    14  0.01919833
# 3    Total  2666  3.65591103

#Approximately 96% are fork length, but leaving all measurements b/c all used for catch and probably not that different


Lm= 230 #Length at maturity Red Snapper
        #Maturity obtained at year 2, but estimate for size at year 2 is greater than 230cm

#Frequency distribution of Red Snapper

ggplot(length_freq_gs40_ms163, aes(length_freq_gs40_ms163$LEN_GLF))+
  geom_histogram()+
  annotate(geom="text",x=235, y=20000, label="Lm", hjust=0)+
  geom_vline(xintercept=230)+
  labs(x="Length (mm)", y="counts", title="Length Distribution")+
  theme_update(plot.title = element_text(hjust = 0.5))

percent_juv= (sum(length_freq_gs40_ms163$LEN_GLF<230)/length(length_freq_gs40_ms163$LEN_GLF))*100
# 94.30103 are Juveniles

#Von Bert Size at Age
len=c(1:5) #dimension variable of typical length at age
for (t in 1:5){
  len[t]=856.4*(1-exp(-.19*(t--0.395))) #Von Bert growth, coefficients from Brad's website
  rm(t)
}






