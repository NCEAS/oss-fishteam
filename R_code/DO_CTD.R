rm(list=ls())
library(tidyverse)
setwd("~/oss/Synthesis")
env<- tbl_df(read.csv("./env_loc_date_depth_all.csv", stringsAsFactors = FALSE, header = TRUE))
env_trim<- env %>% select(X,Cast, Depth, Oxygen, lon, lat, Bottom.depth, year, month)
length(unique(env_trim$X))==length(env_trim$X)
#[1] TRUE # X is a unique ID for row
DO<- env_trim %>% group_by(Cast) %>% summarize(max_depth=max(Depth))

#add X, lat, long, year, month
