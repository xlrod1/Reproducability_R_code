

muhaz_hr<-function(data,theta, n.est.grid = 51,b.cor="n",end.time,min.time,kern="e"){
  
  #devide the data for treatmet and control parts
  data.control<-data%>% filter(treatment==0)
  data.treatment<-data%>% filter(treatment==1)
  
  
  # if(is.na(end.time)||is.na(min.time)){
  #   
  #   
  #   sfit <- survfit(Surv(data.treatment$time, data.treatment$status) ~ 1)
  #   #end.treatment <- sfit$time[which.min(abs(sfit$surv-0.2))]
  #   end.treatment <- approx(sfit$n.risk, sfit$time, xout = 20)$y
  #   min.treatment<- sfit$time[min(which(sfit$cumhaz > 0))]
  #   
  #   
  #   sfit <- survfit(Surv(data.control$time, data.control$status) ~ 1)
  #   #end.control <-  sfit$time[which.min(abs(sfit$surv-0.2))]
  #   end.control <- approx(sfit$n.risk, sfit$time, xout = 20)$y
  #   min.control<- sfit$time[min(which(sfit$cumhaz > 0))]
  #   
  #   
  #   end.time<-min(end.treatment ,end.control)
  #   min.time<-max(min.treatment, min.control)
  # }
  # 
  
  
  #not weighted
  haz.treatment <- muhaz(data.treatment$time, data.treatment$status,min.time = min.time,max.time = end.time,n.est.grid = n.est.grid ,b.cor=b.cor)
  
  #not weighted
  haz.control<- muhaz(data.control$time, data.control$status,min.time = min.time,max.time = end.time,n.est.grid = n.est.grid,b.cor=b.cor )
  # the hazard data
  data.hazard<-data.frame(time=haz.treatment$est.grid,
                          haz.1=haz.treatment$haz.est,haz.0=haz.control$haz.est)
  
  #calculate the cumaltive hazard
  
  data.hazard$cumsum1.ver3<-rep(0,nrow(data.hazard))
  data.hazard$cumsum0.ver3<-rep(0,nrow(data.hazard))
  for( i in 2:nrow(data.hazard)){
    temp_func1 = approxfun(data.hazard$time[1:i],data.hazard$haz.1[1:i])
    data.hazard$cumsum1.ver3[i]<-integrate(temp_func1,lower = data.hazard$time[1],upper =  data.hazard$time[i],stop.on.error=FALSE)[[1]]
    
    temp_func1 = approxfun(data.hazard$time[1:i],data.hazard$haz.0[1:i])
    data.hazard$cumsum0.ver3[i]<-integrate(temp_func1,lower = data.hazard$time[1],upper =  data.hazard$time[i],stop.on.error=FALSE)[[1]]
  }
 
  
  df<-data.frame(time=data.hazard$time,HR=causal.hazard(data.hazard$haz.1,data.hazard$haz.0,
                                                              data.hazard$cumsum1.ver3,data.hazard$cumsum0.ver3,
                                                              theta=theta),method="muhaz")
  df.full<-data.frame(time=data.hazard$time,HR=causal.hazard(data.hazard$haz.1,data.hazard$haz.0,
                                                             data.hazard$cumsum1.ver3,data.hazard$cumsum0.ver3,
                                                             theta=theta),haz1=data.hazard$haz.1,haz0=data.hazard$haz.0,cumsum1=data.hazard$cumsum1.ver3,
                                                                    cumsum0=data.hazard$cumsum0.ver3,method="muhaz")
  
  
  return(list(muhaz.hr.df=df,end.time=end.time,df.full=df.full))
  
}