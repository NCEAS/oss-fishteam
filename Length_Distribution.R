#Using datatables GLFREC and INVREC, determine what the length frequency of red snapper is caught with gear (GEAR_TYPE) shrimp trawl (ST)

library(tidyverse)

#Merge the tables
INVREC<- read.table("Seamap/INVREC.txt", sep=",", stringsAsFactors = FALSE, header = TRUE)
GLFREC<- read.table("Seamap/GLFREC.txt", sep=",", stringsAsFactors = FALSE, header = TRUE)

INV<- INVREC %>% select(CRUISEID, STATIONID, INVRECID, GEAR_TYPE, GEAR_SIZE, MESH_SIZE) %>% filter(GEAR_TYPE=="ST") #Remove some columns and filter for Shrimp Trawl

GLF<- GLFREC %>% select(CRUISEID, STATIONID, GLFID, SPEC_GLF, LEN_GLF) %>% filter(SPEC_GLF=="CAMPEC") #Remove some columns and filter for red snapper

length(unique(INV$STATIONID))==length(INV$STATIONID) #Station ID is unique after filtering in tows of INV

length_freq<- left_join(select(GLF, CRUISEID, STATIONID, LEN_GLF), select(INV, STATIONID, GEAR_TYPE, GEAR_SIZE, MESH_SIZE), by="STATIONID")

length(length_freq$STATIONID)==length(GLF$STATIONID) #New size matches GLF so not repating length values. Merge successful.

#Frequency of red snapper length
                          #Only looking at Shrimp trawl, GEAR_SIZE=40 and MESH_SIZE=1.63


#Frequency of Mesh Size and Gear Size







