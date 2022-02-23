causal.hazard<-function(haz1,haz0,cum.haz1,cum.haz0,theta){
  
  (haz1/haz0)*exp(theta*(cum.haz1-cum.haz0))
  
}