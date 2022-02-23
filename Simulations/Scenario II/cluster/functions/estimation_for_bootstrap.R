
estimation.for.bootsrap<-function(data, indices,confounder,theta, n.est.grid ,b.cor ,end.time,min.time,kern ){
  
  d <- data[indices,] # allows boot to select sample
  if(confounder=="yes"){
    fit <-muhaz_hr_weights(d,theta, n.est.grid ,b.cor,end.time,min.time,kern)
  }
  if(confounder=="no"){
    fit <-muhaz_hr(d,theta, n.est.grid ,b.cor,end.time,min.time,kern)
  }
  fit<-fit$muhaz.hr.df
  return(fit$HR)
  
  
}

