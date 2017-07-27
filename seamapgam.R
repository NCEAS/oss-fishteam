#GLM/GAM code for joined fishy-enviro data

#SEAMAP DATA UNITS:
#TEMPMID: degrees C [1 decimal point]
#SALMID: psu [1 decimal point]
#CHLORSURF: mg/m^3
#OXYMAX: ppm
#LAT/LON: decimal degrees

#Conversion info for Chla - 1 g/m^3 = 1 mg/L so 1mg/m^3 = 1 ug/L so these are equivalent
#Converstion info for O2 - 1 g/m^3 = 1 mg/L = 1 ppm so these are equivalent


#########################
#DO THIS LATER---->take out everything before florida was added
#put in season summer/fall
#########################


#View(fishenvdata)
library(mgcv)
#install.packages("gridExtra")
library(base)
library(bbmle)
library(chron)
library(tidyverse)
library(gridExtra)
library(dplyr)
library(tidyr)
library(gstat)
library(rgdal)
library(raster)
library(rgeos)
library(scales)
#install.packages("ggpubr")
library(ggpubr)
options(stringsAsFactors = FALSE)

setwd("C:/Users/Alex/Documents/spatial-data/")
fishenvdata<-read.csv("fishyenvdata.csv")


sum(is.na(dataonly$CPUE))
#ifelse(fishenvdata$month.x==6 | fishenvdata$month.x==7,fishenvdata$season<-1,fishenvdata$season<-2)

for (i in fishenvdata$STATIONID){
  ifelse(fishenvdata$month.x[i]==6 || fishenvdata$month.x[i]==7,fishenvdata$season[i]<-1,fishenvdata$season[i]<-2)
}
#ifelse(fishenvdata$month.x[i]==6 || fishenvdata$month.x[i]==7,fishenvdata$season[i]<-1,fishenvdata$season[i]<-2)#{}   fishenvdata$season<-1
# } else {
#   fishenvdata$season<-2
# }

#min(fishenvdata$season)

fishenvdata[fishenvdata==-9999]<-NA
dataonlycorr<-dplyr::select(dataonly,-X,-year.x,-month.x,-day.x)
# sum(is.na(dataonly))
# testdropna<-fishenvdata
# testdropna<-drop_na(testdropna)
# attach(fishenvdata)
# lm1<-glm(CPUE~TEMPMID+SALMID+CHLORSURF+OXYMAX)

corrfishvars<-cor(dataonlycorr,y=NULL,use="pairwise.complete.obs",method=c("pearson","kendall","spearman"))
#attach(dataonly)
par(mfrow=c(2,2))
#Tempmid plot
mod1<-lm(CPUE~TEMPMID,data=dataonly)
newx<-seq(min(dataonly$TEMPMID,na.rm=T),max(dataonly$TEMPMID,na.rm=T),length.out=100)
preds<-predict(mod1,newdata=data.frame(TEMPMID=newx),interval='confidence')
plot(CPUE~TEMPMID,data=dataonly)
polygon(c(rev(newx),newx),c(rev(preds[,3]),preds[,2]),col="grey80",border=NA)
abline(mod1)
lines(newx,preds[,3],lty='dashed',col='red')

#Salinity mid plot
mod2<-lm(CPUE~SALMID,data=dataonly)
newx<-seq(min(dataonly$SALMID,na.rm=T),max(dataonly$SALMID,na.rm=T),length.out=100)
preds<-predict(mod2,newdata=data.frame(SALMID=newx),interval='confidence')
plot(CPUE~SALMID,data=dataonly)
polygon(c(rev(newx),newx),c(rev(preds[,3]),preds[,2]),col="grey80",border=NA)
abline(mod2)
lines(newx,preds[,3],lty='dashed',col='red')

#Chl-a surface
mod3<-lm(CPUE~CHLORSURF,data=dataonly)
newx<-seq(min(dataonly$CHLORSURF,na.rm=T),max(dataonly$CHLORSURF,na.rm=T),length.out=100)
preds<-predict(mod3,newdata=data.frame(CHLORSURF=newx),interval='confidence')
plot(CPUE~CHLORSURF,data=dataonly)
polygon(c(rev(newx),newx),c(rev(preds[,3]),preds[,2]),col="grey80",border=NA)
abline(mod3)
lines(newx,preds[,3],lty='dashed',col='red')

#DO-maxdepth
mod4<-lm(CPUE~OXYMAX,data=dataonly)
newx<-seq(min(dataonly$OXYMAX,na.rm=T),max(dataonly$OXYMAX,na.rm=T),length.out=100)
preds<-predict(mod4,newdata=data.frame(OXYMAX=newx),interval='confidence')
plot(CPUE~OXYMAX,data=dataonly)
polygon(c(rev(newx),newx),c(rev(preds[,3]),preds[,2]),col="grey80",border=NA)
abline(mod4)
lines(newx,preds[,3],lty='dashed',col='red')

#season
mod5<-lm(CPUE~season,data=dataonly)
newx<-seq(min(dataonly$season,na.rm=T),max(dataonly$season,na.rm=T),length.out=100)
preds<-predict(mod5,newdata=data.frame(season=newx),interval='confidence')
plot(CPUE~season,data=dataonly)
polygon(c(rev(newx),newx),c(rev(preds[,3]),preds[,2]),col="grey80",border=NA)
abline(mod5)
lines(newx,preds[,3],lty='dashed',col='red')

#looking at interaction between CPUE and envirovariables
par(mfrow=c(2,2))
plot(CPUE~TEMPMID,data=dataonly)
abline(mod1)
plot(CPUE~SALMID,data=dataonly)
abline(mod2)
plot(CPUE~CHLORSURF,data=dataonly)
abline(mod3)
plot(CPUE~OXYMAX,data=dataonly)
abline(mod4)

#Looking at interaction between variables
# par(mfrow=c(2,3))
# mod5<-lm(TEMPMID~SALMID,data=dataonly)
# plot(TEMPMID,SALMID,data=dataonly)
# abline(mod5)
# mod6<-lm(TEMPMID~CHLORSURF,data=dataonly)
# plot(TEMPMID,CHLORSURF,data=dataonly)
# abline(mod6)
# mod7<-lm(SALMID~CHLORSURF,data=dataonly)
# plot(SALMID,CHLORSURF,data=dataonly)
# abline(mod7)
# mod8<-lm(TEMPMID~OXYMAX,data=dataonly)
# plot(TEMPMID,OXYMAX,data=dataonly)
# abline(mod8)
# mod9<-lm(SALMID~OXYMAX,data=dataonly)
# plot(SALMID,OXYMAX,data=dataonly)
# abline(mod9)
# mod10<-lm(CHLORSURF~OXYMAX,data=dataonly)
# plot(CHLORSURF,OXYMAX,data=dataonly)
# abline(mod10)




###########pretty tables

pdf("Correlation_data_outputfin.pdf", height=8.5, width=11)
grid.table(round(corrfishvars,digits=2))
dev.off()


#group code
#testrrrr<-cassminJJON %>% group_by(year.x) %>% summarize(count=sum(is.na(OXYMAX)))

################################
#account for time/space -> relation based on these vars
#convert mo/da/yr to julian, day zero being Jan 1, 1970
#attach(fishenvdata)
dataonly$juldate<-julian(dataonly$month.x,dataonly$day.x,dataonly$year.x)

cassiepredictdata<-read.csv("predict_data.csv")
colnames(cassiepredictdata)<-c("X","Cast","DECSLAT.x","DECSLON.x","year.x","month.x","day.x","TEMPMID","SALMID","Depth","OXYMAX","Type","Bottom.depth","CHLORSURF")
cassmin<-dplyr::select(cassiepredictdata,-X,-Cast,-Depth,-Type,-Bottom.depth)

cassmin$juldate<-julian(cassmin$month.x,cassmin$day.x,cassmin$year.x)
#normalize data: (x-min(x))/(max(x)-min(x))
dataonlys1<-dataonly

dataonlynorm<-(dataonlys1-min(dataonlys1,na.rm=TRUE))/(max(dataonlys1,na.rm=TRUE)-min(dataonlys1,na.rm=TRUE))
#(e^(dataonlynorm))*(xMax-xMin)+xMin=original data

#detach(dataonly)
#attach(dataonlynorm)




#normalize data, poss log-transform



full.additive.modelpois<-gam(CPUE~TEMPMID+SALMID+CHLORSURF+OXYMAX+season+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlynorm,family="poisson")
f.a.mpois<-gam(CPUE~s(TEMPMID,k=-1,bs="cs")+s(SALMID,k=-1,bs="cs")+season+s(CHLORSURF,k=-1,bs="cs")+s(OXYMAX,k=-1,bs="cs")+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlynorm,family="poisson")
full.additive.modelgaus<-gam(CPUE~TEMPMID+SALMID+CHLORSURF+season+OXYMAX+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlynorm,family="gaussian")

f.a.mgaus<-gam(CPUE~s(TEMPMID,k=-1,bs="cs")+s(SALMID,k=-1,bs="cs")+season+s(CHLORSURF,k=-1,bs="cs")+s(OXYMAX,k=-1,bs="cs")+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlynorm,family="gaussian")

full.additive.modelgamma<-gam(CPUE~s(TEMPMID,k=-1,bs="cs")+season+s(SALMID,k=-1,bs="cs")+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlynorm,family="Gamma",link="inverse")
f.a.mgamma<-gam(CPUE~TEMPMID+SALMID+CHLORSURF+OXYMAX+season+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlynorm,family="Gamma",link="inverse")
full.mult.modpois<-gam(CPUE~TEMPMID*SALMID*CHLORSURF*OXYMAX+season+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlynorm,family="poisson")
full.mult.modgaus<-gam(CPUE~TEMPMID*SALMID*CHLORSURF*OXYMAX+season+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlynorm,family="gaussian")
full.mult.modgamma<-gam(CPUE~TEMPMID*SALMID*CHLORSURF*OXYMAX+season+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlynorm,family="Gamma")

linmod.additive.gaus<-glm(CPUE~TEMPMID+SALMID+CHLORSURF+season+OXYMAX,data=dataonlynorm,family="gaussian")
linmod.mult.gaus<-glm(CPUE~TEMPMID*SALMID*CHLORSURF*OXYMAX*season,data=dataonlynorm,family="gaussian")
linmod.additive.gamma<-glm(CPUE~TEMPMID+SALMID+CHLORSURF++season+OXYMAX,data=dataonlynorm,family="Gamma")
linmod.mult.gamma<-glm(CPUE~TEMPMID*SALMID*CHLORSURF**season*OXYMAX,data=dataonlynorm,family="Gamma")


#AICctab(f.a.mgaus,full.additive.modelgaus)

#Hypothesis-driven
gam1<-gam(CPUE~TEMPMID*SALMID+CHLORSURF+season+OXYMAX+s(juldate,k=-1,bs="cc"),data=dataonlynorm,family="gaussian")
#gam1a<-gam(CPUE~s(TEMPMID,k=-1,bs="cs")*s(SALMID,k=-1,bs="cs")+s(CHLORSURF,k=-1,bs="cs")+s(OXYMAX,k=-1,bs="cs")+s(juldate,k=-1,bs="cc"),data=dataonlynorm,family="gaussian")
gam2<-gam(CPUE~s(juldate,k=-1,bs="cc")+season+s(DECSLAT.x,DECSLON.x),data=dataonlynorm,family="gaussian")
gam2a<-gam(CPUE~s(juldate,k=-1,bs="cc")+season+s(DECSLAT.x,DECSLON.x),data=dataonlynorm,family="Gamma")
gam3<-gam(CPUE~TEMPMID*SALMID+s(juldate,k=-1,bs="cc"),data=dataonlynorm,family="gaussian")
#gam3a<-gam(CPUE~s(TEMPMID,k=-1,bs="cs")*s(SALMID,k=-1,bs="cs")+s(juldate,k=-1,bs="cc"),data=dataonlynorm,family="gaussian")
gam4<-gam(CPUE~TEMPMID+OXYMAX+season+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlynorm,family="gaussian")
gam4a<-gam(CPUE~s(TEMPMID,k=-1,bs="cs")+s(OXYMAX,k=-1,bs="cs")+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlynorm,family="gaussian")
gam5<-gam(CPUE~TEMPMID+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlynorm,family="gaussian")
gam5a<-gam(CPUE~s(TEMPMID,k=-1,bs="cs")+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlynorm,family="gaussian")
gam6<-gam(CPUE~TEMPMID+s(juldate,k=-1,bs="cc"),data=dataonlynorm,family="gaussian")
gam6a<-gam(CPUE~s(TEMPMID,k=-1,bs="cs")+s(juldate,k=-1,bs="cc"),data=dataonlynorm,family="gaussian")

#AICctab(f.a.mgamma,f.a.mgaus,f.a.mpois,base=T,delta=T,logLik=T,weights=T)

#AICctab(full.mult.modgamma,full.additive.modelgamma,full.mult.modgaus,full.additive.modelgaus,full.additive.modelpois,full.mult.modpois,base=T,weights=TRUE,delta=TRUE,logLik=T)
#AICctab(gam2,gam2a,weights=T,base=T,delta=T,logLik=T)

#look at all models
#aicallmods<-AIC(linmod.additive.gamma,linmod.mult.gamma,linmod.additive.gaus,linmod.mult.gaus,full.mult.modgamma,full.additive.modelgamma,full.mult.modgaus,full.additive.modelgaus,full.additive.modelpois,full.mult.modpois,gam1,gam2,gam3,gam4,gam5,gam6)
#View(aicallmods)


##################################################
##log-transformed
dataonlyNlogtransform<-log1p(abs(dataonlys1))
dataonlyNlogtransform$OLON<-dataonly$DECSLON.x
dataonlyNlogtransform$OLAT<-dataonly$DECSLAT.x
dataonlyNlogtransform$Ojulian<-dataonly$juldate
##################################################
#detach(dataonlynorm)
#attach(dataonlyNlogtransform)

full.additive.modelpoislog<-gam(CPUE~TEMPMID+SALMID+CHLORSURF+OXYMAX+season+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlyNlogtransform,family="poisson")
f.a.mpoislog<-gam(CPUE~s(TEMPMID,k=-1,bs="cs")+s(SALMID,k=-1,bs="cs")+season+s(CHLORSURF,k=-1,bs="cs")+s(OXYMAX,k=-1,bs="cs")+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlyNlogtransform,family="poisson")
full.additive.modelgauslog<-gam(CPUE~TEMPMID+SALMID+CHLORSURF+OXYMAX+season+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlyNlogtransform,family="gaussian")

f.a.mgauslog<-gam(CPUE~s(TEMPMID,k=-1,bs="cs")+s(SALMID,k=-1,bs="cs")+
                    s(CHLORSURF,k=-1,bs="cs")+season+s(OXYMAX,k=-1,bs="cs")+
                    s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),
                  data=dataonlyNlogtransform,family="gaussian")

#full.additive.modelgammalog<-gam(CPUE~s(TEMPMID,k=-1,bs="cs")+s(SALMID,k=-1,bs="cs")+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlyNlogtransform,family="Gamma",link="inverse")
#f.a.mgammalog<-gam(CPUE~TEMPMID+SALMID+CHLORSURF+OXYMAX+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlyNlogtransform,family="Gamma",link="inverse")
full.mult.modpoislog<-gam(CPUE~TEMPMID*SALMID*CHLORSURF*season*OXYMAX+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlyNlogtransform,family="poisson")
full.mult.modgauslog<-gam(CPUE~TEMPMID*SALMID*CHLORSURF*OXYMAX*season+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlyNlogtransform,family="gaussian")
#full.mult.modgammalog<-gam(CPUE~TEMPMID*SALMID*CHLORSURF*OXYMAX+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlyNlogtransform,family="Gamma")

linmod.additive.gauslog<-glm(CPUE~TEMPMID+SALMID+CHLORSURF+season+OXYMAX,data=dataonlyNlogtransform,family="gaussian")
linmod.mult.gauslog<-glm(CPUE~TEMPMID*SALMID*CHLORSURF*season*OXYMAX,data=dataonlyNlogtransform,family="gaussian")
#linmod.additive.gammalog<-glm(CPUE~TEMPMID+SALMID+CHLORSURF+OXYMAX,data=dataonlyNlogtransform,family="Gamma")
#linmod.mult.gammalog<-glm(CPUE~TEMPMID*SALMID*CHLORSURF*OXYMAX,data=dataonlyNlogtransform,family="Gamma")

gam1log<-gam(CPUE~TEMPMID*SALMID+CHLORSURF+OXYMAX+s(juldate,k=-1,bs="cc"),data=dataonlyNlogtransform,family="gaussian")
#gam1alog<-gam(CPUE~s(TEMPMID,k=-1,bs="cs")*s(SALMID,k=-1,bs="cs")+s(CHLORSURF,k=-1,bs="cs")+s(OXYMAX,k=-1,bs="cs")+s(juldate,k=-1,bs="cc"),data=dataonlyNlogtransform,family="gaussian")
gam2log<-gam(CPUE~s(juldate,k=-1,bs="cc")+season+s(DECSLAT.x,DECSLON.x),data=dataonlyNlogtransform,family="gaussian")
#gam2alog<-gam(CPUE~s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlyNlogtransform,family="Gamma")
gam3log<-gam(CPUE~TEMPMID*SALMID+season+s(juldate,k=-1,bs="cc"),data=dataonlyNlogtransform,family="gaussian")
#gam3alog<-gam(CPUE~s(TEMPMID,k=-1,bs="cs")*s(SALMID,k=-1,bs="cs")+s(juldate,k=-1,bs="cc"),data=dataonlyNlogtransform,family="gaussian")
gam4log<-gam(CPUE~TEMPMID+OXYMAX+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlyNlogtransform,family="gaussian")
gam4alog<-gam(CPUE~s(TEMPMID,k=-1,bs="cs")+s(OXYMAX,k=-1,bs="cs")+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlyNlogtransform,family="gaussian")
gam5log<-gam(CPUE~TEMPMID+season+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlyNlogtransform,family="gaussian")
gam5alog<-gam(CPUE~s(TEMPMID,k=-1,bs="cs")+s(juldate,k=-1,bs="cc")+s(DECSLAT.x,DECSLON.x),data=dataonlyNlogtransform,family="gaussian")
gam6log<-gam(CPUE~TEMPMID+s(juldate,k=-1,bs="cc"),data=dataonlyNlogtransform,family="gaussian")
gam6alog<-gam(CPUE~s(TEMPMID,k=-1,bs="cs")+s(juldate,k=-1,bs="cc"),data=dataonlyNlogtransform,family="gaussian")


##############################################
#create AICc table manually
#######
#See AICtable.R THEN run
#    batchGAMfigures.R
#######

# top 3 Models per type
#       GausLog
# gam1
# f.a.mgauslog
# full.additive.modelgauslog
# 
#       GammaNorm
# linmod.mult.gamma
# gam2a
# full.additive.modelgamma
# 
#       GausNorm
# f.a.mgaus
# full.mult.modgaus
# full.additive.modelgaus

#################



par(mfrow=c(1,2))
plot(residuals(f.a.mgauslog),main="Log Transformed Normalized Gaussian full-model GAM")
plot(residuals(f.a.mgaus),main="Normalized Gaussian full-model GAM")
#plotfigure and get all vars
plot(f.a.mgauslog)




################################################################################

summary(full.additive.model)
plot(residuals(full.additive.model))
gam.check(full.additive.model)
# 
# #3d visualization of GAM
# vis.gam(full.additive.modelgaus,n.grid=50,theta=35,phi=32,zlab=" ",
#         ticktype="detailed",color="topo",main="CPUE GAM from Seamap Data")
# #contour visualization of GAM
# vis.gam(full.additive.modelgaus,main="CPUE GAM from Seamap data",
#         plot.type="contour",color="topo",contour.col="black",lwd=2)


#make a grid includes lat/lon and time temp sal oxy chla
#use predict 
#new dataframe -> lat,lon,TMSMCHla,OXY,time
#newdata input
full.additive.modelgaus$fitted.values



#now on gdrive under data: final data frame from the NOAA data. 
#it's called predict_data.csv. it can (hopefully) be used to make predictions from our model. 
#fyi O2 is in ml l-1 and Chl is in ug l-1.


# x_range<-as.numeric(c(min(cassiepredictdata$lon),max(cassiepredictdata$lon)))
# y_range<-as.numeric(c(min(cassiepredictdata$lat),max(cassiepredictdata$lat)))
# grd<-expand.grid(x = seq(from = x_range[1], to = x_range[2], by = 0.1), y = seq(from = y_range[1],to = y_range[2], by = 0.1))  # expand points to grid
# class(grd)
###################

# coordinates(grd) <- ~x + y
# gridded(grd)<-TRUE
# plot(grd, cex = 1.5, col = "grey")

#make column names same between both

####
#Xp<-predict(f.a.mgauslog,grd,type="lpmatrix")
######


#DECSLAT.x DECSLON.x year.x month.x day.x CPUE TEMPMID SALMID CHLORSURF OXYMAX juldate

###########################################
cassminJJON1<-dplyr::select(cassminJJON,-X)
cassminJJON1<-log10(abs(cassminJJON1)+1)
cassminJJON1$OLON<-cassminJJON$DECSLON.x
cassminJJON1$OLAT<-cassminJJON$DECSLAT.x
cassminJJON1$Ojulian<-cassminJJON$juldate
Castdatafinal<-cassminJJON1
cc5<-predict.gam(f.a.mgauslog,newdata=Castdatafinal,type="response",se=T)
Castdatafinal$fit<-as.numeric(cc5$fit)
Castdatafinal$se.fit<-as.numeric(cc5$se.fit)


coordinates(Castdatafinal)<-~OLON+OLAT
crs(Castdatafinal)<-crs.geo

strfile<-paste("results/CastGAM.shp")

writeOGR(Castdatafinal,strfile,"*",driver="ESRI Shapefile")


#############################################
seamapdatfinal<-dataonlyNlogtransform
#cassmin$juldate<-julian(cassmin$month.x,cassmin$day.x,cassmin$year.x)
cc4<-predict.gam(f.a.mgauslog,newdata=seamapdatfinal,type="response",se=TRUE)
View(cc4)
seamapdatfinal$fit<-cc4$fit
seamapdatfinal$se.fit<-cc4$se.fit

seamapdatfinalretransform<-dplyr::select(seamapdatfinal,-OLON,-OLAT,-Ojulian)
seamapdatfintest<-expm1(seamapdatfinalretransform)

par(mfrow=c(1,1))
plot(seamapdatfintest$juldate,seamapdatfintest$fit,type="l")
lines(seamapdatfintest$juldate,seamapdatfintest$fit+2*seamapdatfintest$se.fit,col=2)
lines(seamapdatfintest$juldate,seamapdatfintest$fit-2*seamapdatfintest$se.fit,col=2)



#####make shp
crs.geo<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  #define projection

coordinates(seamapdatfinal)<-~OLON+OLAT
crs(seamapdatfinal)<-crs.geo

strfile<-paste("results/BestGAM.shp")

writeOGR(seamapdatfinal,strfile,"*",driver="ESRI Shapefile")