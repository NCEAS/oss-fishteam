#indepmod<-c(5,2,2,3,4,4,3,3,2,2,5,2,3,4,4,3,3,2,2,6,6,6,4,4,6,6,6,6,6,6,4,4,4,4)

##AIC and plot residuals
##Gamma model comparison, all normalized only
aicgamma<-data.frame(c(gam2a$aic,full.additive.modelgamma$aic,f.a.mgamma$aic,full.mult.modgamma$aic,linmod.additive.gamma$aic,linmod.mult.gamma$aic))
colnames(aicgamma)[1]<-"aic"
aicgamma$mname<-c(deparse(substitute(gam2a)),deparse(substitute(full.additive.modelgamma)),deparse(substitute(f.a.mgamma)),deparse(substitute(full.mult.modgamma)),deparse(substitute(linmod.additive.gamma)),deparse(substitute(linmod.mult.gamma)))
aicgamma$aicabs<-abs(aicgamma$aic)
aicgamma$aicc<-indepmod[1:6]
aicgamma$daicc<-indepmod[1:6]
indepmodgamma<-c(2,6,6,6,4,4)
for (i in 1:6){
  K<-indepmodgamma[i]
  n<-21208
  aicgamma$aicc[i]<-aicgamma$aic[i]+2*K*(K+1)/(n-K-1)
  #aichyp$daicc[i]<-abs(aichyp$aicc[i+1])-abs(aichyp$aicc[i])
}
aicgammasort<-aicgamma[order(abs(aicgamma$aicc),decreasing=T),]
aicgammasort$daicc[1]<-0
aicgammasort$aicc<-abs(aicgammasort$aicc)
for (i in 1:5){
  aicgammasort$daicc[i+1]<-aicgammasort$aicc[i+1]-aicgammasort$aicc[1]
}
aicgammasort$daicc<-abs(aicgammasort$daicc)

aicgammasort$daicc<-abs(aicgammasort$daicc)
aicgammasort<-dplyr::select(aicgammasort,-aicabs,-aic)
aicgammasort$aicc<-round(aicgammasort$aicc,digits=2)
aicgammasort$daicc<-round(aicgammasort$daicc,digits=2)

pdf("AICc_tableGamma.pdf",height=11,width=8.5)
grid.table(aicgammasort)
dev.off()

####################################################################################
##Gaussian-distribution model comparison, mix of normalized-only and log-transformed datasets
aicgausnorm<-data.frame(c(full.additive.modelgaus$aic,f.a.mgaus$aic,full.mult.modgaus$aic,linmod.additive.gaus$aic,linmod.mult.gaus$aic,gam1$aic,gam2$aic,gam3$aic,gam4$aic,gam4a$aic,gam5$aic,gam5a$aic,gam6$aic,gam6a$aic))
colnames(aicgausnorm)[1]<-"aic"
aicgausnorm$mname<-c(deparse(substitute(full.additive.modelgaus)),deparse(substitute(f.a.mgaus)),deparse(substitute(full.mult.modgaus)),deparse(substitute(linmod.additive.gaus)),deparse(substitute(linmod.mult.gaus)),deparse(substitute(gam1)),deparse(substitute(gam2)),deparse(substitute(gam3)),deparse(substitute(gam4)),deparse(substitute(gam4a)),deparse(substitute(gam5)),deparse(substitute(gam5a)),deparse(substitute(gam6)),deparse(substitute(gam6a)))
aicgausnorm$aicabs<-abs(aicgausnorm$aic)
aicgausnorm$aicc<-indepmod[1:14]
aicgausnorm$daicc<-indepmod[1:14]
indepmodgaus<-c(6,6,6,4,4,5,2,3,4,4,3,3,2,2)
for (i in 1:6){
  K<-indepmodgaus[i]
  n<-21208
  aicgausnorm$aicc[i]<-aicgausnorm$aic[i]+2*K*(K+1)/(n-K-1)
  #aichyp$daicc[i]<-abs(aichyp$aicc[i+1])-abs(aichyp$aicc[i])
}
aicgausnsort<-aicgausnorm[order(abs(aicgausnorm$aicc),decreasing=T),]
aicgausnsort$daicc[1]<-0
aicgausnsort$aicc<-abs(aicgausnsort$aicc)
for (i in 1:13){
  aicgausnsort$daicc[i+1]<-aicgausnsort$aicc[i+1]-aicgausnsort$aicc[1]
}
aicgausnsort$daicc<-abs(aicgausnsort$daicc)

aicgausnsort$daicc<-abs(aicgausnsort$daicc)
aicgausnsort<-dplyr::select(aicgausnsort,-aicabs,-aic)
aicgausnsort$aicc<-round(aicgausnsort$aicc,digits=2)
aicgausnsort$daicc<-round(aicgausnsort$daicc,digits=2)

pdf("AICc_tableGausNormalized.pdf",height=11,width=8.5)
grid.table(aicgausnsort)
dev.off()

#######################
aicgauslog<-data.frame(c(f.a.mgauslog$aic,full.additive.modelgauslog$aic,full.mult.modgauslog$aic,linmod.additive.gauslog$aic,linmod.mult.gauslog$aic,gam1log$aic,gam2log$aic,gam3log$aic,gam4log$aic,gam4alog$aic,gam5log$aic,gam5alog$aic,gam6log$aic,gam6alog$aic))
colnames(aicgauslog)[1]<-"aic"
aicgauslog$mname<-c(deparse(substitute(f.a.mgauslog)),deparse(substitute(full.additive.modelgauslog)),deparse(substitute(full.mult.modgauslog)),deparse(substitute(linmod.additive.gauslog)),deparse(substitute(linmod.mult.gauslog)),deparse(substitute(gam1log)),deparse(substitute(gam2log)),deparse(substitute(gam3log)),deparse(substitute(gam4log)),deparse(substitute(gam4alog)),deparse(substitute(gam5log)),deparse(substitute(gam5alog)),deparse(substitute(gam6log)),deparse(substitute(gam6alog)))
aicgauslog$aicabs<-abs(aicgauslog$aic)
aicgauslog$aicc<-indepmod[1:14]
aicgauslog$daicc<-indepmod[1:14]
indepmodgaus<-c(6,6,6,4,4,5,2,3,4,4,3,3,2,2)
for (i in 1:6){
  K<-indepmodgaus[i]
  n<-21208
  aicgauslog$aicc[i]<-aicgauslog$aic[i]+2*K*(K+1)/(n-K-1)
  #aichyp$daicc[i]<-abs(aichyp$aicc[i+1])-abs(aichyp$aicc[i])
}
aicgauslsort<-aicgauslog[order(abs(aicgauslog$aicc),decreasing=T),]
aicgauslsort$daicc[1]<-0
aicgauslsort$aicc<-abs(aicgausnsort$aicc)
for (i in 1:13){
  aicgauslsort$daicc[i+1]<-aicgauslsort$aicc[i+1]-aicgauslsort$aicc[1]
}
#aicgauslsort$daicc<-abs(aicgauslsort$daicc)
aicgauslsort<-dplyr::select(aicgauslsort,-aicabs,-aic)
aicgauslsort$aicc<-round(aicgauslsort$aicc,digits=2)
aicgauslsort$daicc<-round(aicgauslsort$daicc,digits=2)

pdf("AICc_tableGausLog.pdf",height=11,width=8.5)
grid.table(aicgauslsort)
dev.off()




###################################################################################
#All models
aichyp<-data.frame(c(gam1$aic,gam2$aic,gam2a$aic,gam3$aic,gam4$aic,gam4a$aic,gam5$aic,gam5a$aic,gam6$aic,gam6a$aic,gam1log$aic,gam2log$aic,gam3log$aic,gam4log$aic,gam4alog$aic,gam5log$aic,gam5alog$aic,gam6log$aic,gam6alog$aic,full.additive.modelgauslog$aic,f.a.mgauslog$aic,full.mult.modgauslog$aic,linmod.additive.gauslog$aic,linmod.mult.gauslog$aic,full.additive.modelgaus$aic,f.a.mgaus$aic,full.additive.modelgamma$aic,f.a.mgamma$aic,full.mult.modgaus$aic,full.mult.modgamma$aic,linmod.additive.gaus$aic,linmod.mult.gaus$aic,linmod.additive.gamma$aic,linmod.mult.gamma$aic))
aichyp$mname<-c(deparse(substitute(gam1)),deparse(substitute(gam2)),deparse(substitute(gam2a)),
                deparse(substitute(gam3)),deparse(substitute(gam4)),deparse(substitute(gam4a)),
                deparse(substitute(gam5)),deparse(substitute(gam5a)),deparse(substitute(gam6)),
                deparse(substitute(gam6a)),deparse(substitute(gam1log)),deparse(substitute(gam2log)),
                deparse(substitute(gam3log)),deparse(substitute(gam4log)),deparse(substitute(gam4alog)),
                deparse(substitute(gam5log)),deparse(substitute(gam5alog)),deparse(substitute(gam6log)),
                deparse(substitute(gam6alog)),deparse(substitute(full.additive.modelgauslog)),
                deparse(substitute(f.a.mgauslog)),
                deparse(substitute(full.mult.modgauslog)),
                deparse(substitute(linmod.additive.gauslog)),deparse(substitute(linmod.mult.gauslog)),
                deparse(substitute(full.additive.modelgaus)),deparse(substitute(f.a.mgaus)),
                deparse(substitute(full.additive.modelgamma)),deparse(substitute(f.a.mgamma)),
                deparse(substitute(full.mult.modgaus)),
                deparse(substitute(full.mult.modgamma)),deparse(substitute(linmod.additive.gaus)),
                deparse(substitute(linmod.mult.gaus)),deparse(substitute(linmod.additive.gamma)),
                deparse(substitute(linmod.mult.gamma)))



aichyp$aicabs<-abs(aichyp$aic)
aichyp[order(aichyp$aicabs,decreasing=T),]
aichyp$aicc<-indepmod
aichyp$daicc<-indepmod

#convert AIC to AICC
#K is number of parameters in model, n is number of observations
#equation: AICc<-AIC+2*K*(K+1)/(n-K-1)
#test gam2a

for (i in 1:34){
  K<-indepmod[i]
  n<-21208
  aichyp$aicc[i]<-aichyp$aic[i]+2*K*(K+1)/(n-K-1)
  #aichyp$daicc[i]<-abs(aichyp$aicc[i+1])-abs(aichyp$aicc[i])
}
aicsortaicc<-aichyp[order(abs(aichyp$aicc),decreasing=T),]
aicsortaicc$daicc[1]<-0
aicsortaicc$aicc<-abs(aicsortaicc$aicc)
for (i in 2:33){
  aicsortaicc$daicc[i+1]<-aicsortaicc$aicc[i+1]-aicsortaicc$aicc[1]
}
aicsortaicc$daicc<-abs(aicsortaicc$daicc)

#gam2a_aicc<-aichyp$aicabs[3]+2*3*(3+1)/(21208-3-1)
#gam2_aicc<-aichyp$aicabs[2]+2*3*(3+1)/(21208-3-1)
#daiccgam2_2a<-gam2a_aicc-gam2_aicc

#look at AICctable
aicctable<-dplyr::select(aicsortaicc,-aicabs,-aic)
aicctable$aicc<-round(aicctable$aicc,digits=2)
aicctable$daicc<-round(aicctable$daicc,digits=2)

pdf("AICc_table.pdf",height=11,width=8.5)
grid.table(aicctable)
dev.off()

