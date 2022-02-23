creating.statistics.bias.data<-function(data,variable.name){
  
  all.df<-data
  temp = lapply(all.df, function(x){x[,variable.name]})
  all.muhaz.df= data.frame(do.call(rbind,temp))
  all.muhaz.estimation<-data.frame(t(data.frame(rbind(time=all.df[[1]]$time,all.muhaz.df))))
  
  for( j in 1: length(all.df)){
    names(all.muhaz.estimation)[j+1]<-paste("sim.num.",j,sep="")
  }
  names(all.muhaz.estimation)[1]<-'time'
  #prepare the statistics
  all.muhaz.df<-apply(all.muhaz.df,c(1,2),function(x){ifelse(x%in%c(Inf,-Inf),NA,x)})
  
  mean.HR=apply(all.muhaz.df,2,function(x){mean(x,na.rm = TRUE)})
  SE.HR=apply(all.muhaz.df,2,function(x){sqrt(var(x,na.rm = TRUE))})
  median.HR=apply(all.muhaz.df,2,function(x){median(x,na.rm = TRUE)})
  median.HR=apply(all.muhaz.df,2,function(x){median(x,na.rm = TRUE)})
  quantile.HR=apply(all.muhaz.df,2,function(x){quantile(x, probs = seq(0, 1, 0.1), na.rm = TRUE)})
  quantile2.HR=apply(all.muhaz.df,2,function(x){quantile(x, probs = seq(0, 1, 0.25), na.rm = TRUE)})
  quantile3.HR=apply(all.muhaz.df,2,function(x){quantile(x, probs = c(0.025, 0.075), na.rm = TRUE)})
  
  
  mean.hr.statistics<-rbind(time=all.df[[1]]$time,mean.HR,SE.HR,median.HR,quantile.HR,quantile2.HR,quantile3.HR)
  tt<-data.frame(t(mean.hr.statistics))
  
  
  time<-tt$time
  
  
  return(list(statistics.table=tt,all.muhaz.estimation=all.muhaz.estimation))
  
  
  
  
}




