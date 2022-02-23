
estimation.for.bootsrap<-function(data, indices,confounder,theta, n.est.grid ,b.cor ,end.time,min.time,kern ){

  d <- data[indices,] # allows boot to select sample
  if(confounder=="yes"){
    fit <-muhaz_hr_weights(d,theta, n.est.grid ,b.cor,end.time,min.time,kern)
    cox.reg<-coxph(Surv(time,status)~treatment,data=d,weights = W)
  }
  if(confounder=="no"){
    fit <-muhaz_hr(d,theta, n.est.grid ,b.cor,end.time,min.time,kern)
   
    cox.reg<-coxph(Surv(time,status)~treatment,data=d)
  }
  fit<-fit$muhaz.hr.df
  
  time=fit$time
  beta.hat<-as.numeric(cox.reg$coefficients[1])
  base.haz.estimator<-stepfun(x=basehaz(cox.reg,centered = FALSE)$time,y=c(0,basehaz(cox.reg,centered = FALSE)$hazard))
  base.line.cumsum<- base.haz.estimator(time) 
  
  HR.sp<-exp(beta.hat)*exp(theta*base.line.cumsum*(exp(beta.hat)-1))
  
  
  vector.results<-c(fit$HR,HR.sp)
  
  return(vector.results)
  
  
}

