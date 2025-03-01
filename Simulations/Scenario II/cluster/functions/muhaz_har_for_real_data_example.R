

muhaz_hr_for_real_data_example<-function(frailty.type,data,theta, confounder,n.est.grid = 51,b.cor="n",end.time=NA,min.time =NA,kern="e"){
  
  #devide the data for treatmet and control parts
  data.control<-data%>% filter(treatment==0)%>% arrange(time)
  data.treatment<-data%>% filter(treatment==1)%>% arrange(time)
  
  
  if(confounder=="yes"){
    if(is.na(end.time)||is.na(min.time)){
      
      
      sfit <- survfit(Surv(data.treatment$time, data.treatment$status) ~ 1,weights = data.treatment$W)
      #end.treatment <- sfit$time[which.min(abs(sfit$surv-0.2))]
      end.treatment <- approx(sfit$n.risk, sfit$time, xout = 20)$y
      min.treatment<- sfit$time[min(which(sfit$cumhaz > 0))]
      
      
      sfit <- survfit(Surv(data.control$time, data.control$status) ~ 1,weights = data.control$W)
      #end.control <-  sfit$time[which.min(abs(sfit$surv-0.2))]
      end.control <- approx(sfit$n.risk, sfit$time, xout = 20)$y
      min.control<- sfit$time[min(which(sfit$cumhaz > 0))]
      
      
      end.time<-min(end.treatment ,end.control)
      min.time<-max(min.treatment, min.control)
    }
    
  }
  if(confounder=="no"){
    
    if(is.na(end.time)||is.na(min.time)){
      
      
      sfit <- survfit(Surv(data.treatment$time, data.treatment$status) ~ 1)
      #end.treatment <- sfit$time[which.min(abs(sfit$surv-0.2))]
      end.treatment <- approx(sfit$n.risk, sfit$time, xout = 20)$y
      min.treatment<- sfit$time[min(which(sfit$cumhaz > 0))]
      
      
      sfit <- survfit(Surv(data.control$time, data.control$status) ~ 1)
      #end.control <-  sfit$time[which.min(abs(sfit$surv-0.2))]
      end.control <- approx(sfit$n.risk, sfit$time, xout = 20)$y
      min.control<- sfit$time[min(which(sfit$cumhaz > 0))]
      
      
      end.time<-min(end.treatment ,end.control)
      min.time<-max(min.treatment, min.control)
    }
  }
  
  
  if(confounder=="yes"){
    #not weighted
    haz.treatment <- muhaz(data.treatment$time, data.treatment$status,min.time = min.time,max.time = end.time,n.est.grid = n.est.grid ,b.cor=b.cor,w=data.treatment$W,kern=kern)
    
    #not weighted
    haz.control<- muhaz(data.control$time, data.control$status,min.time = min.time,max.time = end.time,n.est.grid = n.est.grid,b.cor=b.cor,w=data.control$W ,kern = kern)
    
  }
  
  if(confounder=="no"){
    #not weighted
    haz.treatment <- muhaz(data.treatment$time, data.treatment$status,min.time = min.time,max.time = end.time,n.est.grid = n.est.grid ,b.cor=b.cor)
    
    #not weighted
    haz.control<- muhaz(data.control$time, data.control$status,min.time = min.time,max.time = end.time,n.est.grid = n.est.grid,b.cor=b.cor )
    
  }
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
  
  
  df<-data.frame(time=data.hazard$time,HR=causal.hazard.frailty.type(frailty.type=frailty.type,data.hazard$haz.1,data.hazard$haz.0,
                                                        data.hazard$cumsum1.ver3,data.hazard$cumsum0.ver3,
                                                        theta=theta),method="muhaz")
  
  
  
  return(list(muhaz.hr.df=df,end.time=end.time))
  
}