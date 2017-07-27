#cross validation test


if(require(sp)){
  library(mgcv)
  cvgamtest<-CVgam(formula=CPUE~s(TEMPMID)+s(SALMID)+s(CHLORSURF)+s(OXYMAX)+season+s(juldate)+s(DECSLON.x,DECSLAT.x),
        data=dataonlyNlogtransform,nfold=10,debug.level=0,method="GCV.Cp",printit=T,cvparts=NULL,gamma=1,seed=29)
}




#write resid hist of list of characters of model names
par(mfrow=c(4,4))
for (i in 1:34) {
  #print(i)
  hist(residuals.gam(eval(parse(text=paste0(aicsortaicc$mname[i])))),main=aicsortaicc$mname[i],xlab="")
}


#write QQ plot for gam
type<-"deviance"
par(mfrow=c(4,4))
for (i in 1:34) {
  qq.gam(eval(parse(text=paste0(aicsortaicc$mname[i]))),type=type,main=aicsortaicc$mname[i])
}

par(mfrow=c(4,4))
for (i in 1:34) {
  observed.y <- napredict(eval(parse(text=paste0(aicsortaicc$mname[i])))$na.action, eval(parse(text=paste0(aicsortaicc$mname[i])))$y)
  plot(fitted(eval(parse(text=paste0(aicsortaicc$mname[i]))), observed.y, xlab = "Fitted Values", 
       ylab = "Response", main = aicsortaicc$mname[i]))
}



par(mfrow=c(4,4))
for (i in 1:34) {
  resid <- residuals(eval(parse(text=paste0(aicsortaicc$mname[i]))), type = type)
  linpred <- napredict(eval(parse(text=paste0(aicsortaicc$mname[i])))$na.action, eval(parse(text=paste0(aicsortaicc$mname[i])))$linear.predictors)
  plot(linpred, resid, main = aicsortaicc$mname[i], 
       xlab = "linear predictor", ylab = "residuals")
}


##top 3 from each
par(mfrow=c(3,3))
for (i in 1:3) {
  resid <- residuals(eval(parse(text=paste0(aicgammasort$mname[i]))), type = type)
  linpred <- napredict(eval(parse(text=paste0(aicgammasort$mname[i])))$na.action, eval(parse(text=paste0(aicgammasort$mname[i])))$linear.predictors)
  plot(linpred, resid, main = aicgammasort$mname[i], 
       xlab = "linear predictor", ylab = "residuals")
  
  resid <- residuals(eval(parse(text=paste0(aicgauslsort$mname[i]))), type = type)
  linpred <- napredict(eval(parse(text=paste0(aicgauslsort$mname[i])))$na.action, eval(parse(text=paste0(aicgauslsort$mname[i])))$linear.predictors)
  plot(linpred, resid, main = aicgauslsort$mname[i], 
       xlab = "linear predictor", ylab = "residuals")
  
  resid <- residuals(eval(parse(text=paste0(aicgausnsort$mname[i]))), type = type)
  linpred <- napredict(eval(parse(text=paste0(aicgausnsort$mname[i])))$na.action, eval(parse(text=paste0(aicgausnsort$mname[i])))$linear.predictors)
  plot(linpred, resid, main = aicgausnsort$mname[i], 
       xlab = "linear predictor", ylab = "residuals")
  
}

#QQGAM PLOT
type<-"deviance"
par(mfrow=c(3,3))
for (i in 1:3) {
  qq.gam(eval(parse(text=paste0(aicgammasort$mname[i]))),type=type,main=aicgammasort$mname[i])
  qq.gam(eval(parse(text=paste0(aicgauslsort$mname[i]))),type=type,main=aicgauslsort$mname[i])
  qq.gam(eval(parse(text=paste0(aicgausnsort$mname[i]))),type=type,main=aicgausnsort$mname[i])
}

#fitted response
par(mfrow=c(3,3))
for (i in 1:3) {
  observed.y <- napredict(eval(parse(text=paste0(aicgammasort$mname[i])))$na.action, eval(parse(text=paste0(aicgammasort$mname[i])))$y)
  plot(fitted(eval(parse(text=paste0(aicgammasort$mname[i]))), observed.y, xlab = "Fitted Values", 
              ylab = "Response", main = aicgammasort$mname[i]))
  
  observed.y <- napredict(eval(parse(text=paste0(aicgauslsort$mname[i])))$na.action, eval(parse(text=paste0(aicgauslsort$mname[i])))$y)
  plot(fitted(eval(parse(text=paste0(aicgauslsort$mname[i]))), observed.y, xlab = "Fitted Values", 
              ylab = "Response", main = aicgauslsort$mname[i]))
  
  observed.y <- napredict(eval(parse(text=paste0(aicgausnsort$mname[i])))$na.action, eval(parse(text=paste0(aicgausnsort$mname[i])))$y)
  plot(fitted(eval(parse(text=paste0(aicgausnsort$mname[i]))), observed.y, xlab = "Fitted Values", 
              ylab = "Response", main = aicgausnsort$mname[i]))
}


