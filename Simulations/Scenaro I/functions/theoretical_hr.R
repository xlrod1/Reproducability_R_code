theoretical_hr<-function(potential.times,t,dt){
  
  
  HR<-c()
  for (i in 1:(length(t)-1)){
    always.survivor.risk<-filter(potential.times,T.0>=t[i],T.1>=t[i])
    always.survivor.event.t1<-filter(always.survivor.risk,T.1<t[i+1])
    always.survivor.event.t0<-filter(always.survivor.risk,T.0<t[i+1])
    
    
    
    hazard.1<-nrow(always.survivor.event.t1)/nrow(always.survivor.risk)
    hazard.0<-nrow(always.survivor.event.t0)/nrow(always.survivor.risk)
    
    HR[i]<-   hazard.1/hazard.0
  }
  HR
}