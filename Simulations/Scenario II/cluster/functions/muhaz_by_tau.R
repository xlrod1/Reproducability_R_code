data.by.tau<-function(tau,data,frailty.type,confounder,n.est.grid,b.cor,end.time,min.time,kern){
  if (frailty.type==1){
    theta01<-theta.gamma(tau=tau)
  }
  
  if (frailty.type!=1){
    tau.theta<-theta.inverse.gaussian(data.tau,tau=tau)
    theta01<-tau.theta$theta
    
  }
  
  bb.sample<-muhaz_hr_for_real_data_example(frailty.type,data,theta01, confounder,n.est.grid =n.est.grid,b.cor=b.cor,end.time=end.time,min.time =min.time,kern=kern)
  muhaz.hr.df.list<-bb.sample$muhaz.hr.df
  muhaz.hr.df.list$tau<-as.character(tau)
  
  if(confounder=="yes"){
    cox.reg<-coxph(Surv(time,status)~treatment,data=data,weights = W)
  }
  if(confounder=="no"){
    cox.reg<-coxph(Surv(time,status)~treatment,data=data)
  }
  beta.hat<-as.numeric(cox.reg$coefficients[1])
  time<-muhaz.hr.df.list$time
  base.haz.estimator<-stepfun(x=basehaz(cox.reg,centered = FALSE)$time,y=c(0,basehaz(cox.reg,centered = FALSE)$hazard))
  base.line.cumsum<- base.haz.estimator(time) 
  
  if(frailty.type==1){
    HR.sp<-exp(beta.hat)*exp(theta01*base.line.cumsum*(exp(beta.hat)-1))
  }
  if(frailty.type!=1){
    
    HR.sp<-exp(beta.hat)*((1+theta01*exp(beta.hat)*base.line.cumsum)/(1+theta01*base.line.cumsum))
  }
  muhaz.hr.df.list$HR.sp<-HR.sp
  
  
  
  muhaz.hr.df.list
}