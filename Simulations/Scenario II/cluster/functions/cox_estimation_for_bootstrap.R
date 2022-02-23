cox.estimation.for.bootsrap<-function(data, indices,theta,time,confounder){
  
  d <- data[indices,] # allows boot to select sample
  
  if(confounder=="no"){
    cox.reg<-coxph(Surv(time,status)~treatment,data=d)
  }
  if(confounder=="yes"){
    cox.reg<-coxph(Surv(time,status)~treatment,data=d,weights = W)
  }
  beta.hat<-as.numeric(cox.reg$coefficients[1])
  base.haz.estimator<-stepfun(x=basehaz(cox.reg,centered = FALSE)$time,y=c(0,basehaz(cox.reg,centered = FALSE)$hazard))
  base.line.cumsum<- base.haz.estimator(time) 
  
  HR.sp<-exp(beta.hat)*exp(theta*base.line.cumsum*(exp(beta.hat)-1))
 
  
  return(HR.sp)
  
  
}