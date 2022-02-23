
#crates the true hazard function
true.hr.function<-function(form.hr=1,time, fixed.treatment.effect,time.varying.treatment.effect=NULL){
  if(!form.hr%in%c(1,2,3,4)) {print("Your model for hazard ratio is undefined")}
  else{
    #if hr is constant 
    if(form.hr==1){
      true.hr<-rep(exp(fixed.treatment.effect),length(time))
    }
    if(form.hr==2){
      true.hr<-exp(time.varying.treatment.effect*time+fixed.treatment.effect)
    }
    #if hr is qudraric
    if(form.hr==3){
      true.hr<-exp(time.varying.treatment.effect*(time-1)^2+fixed.treatment.effect)
    }
    if(form.hr==4){
      true.hr<-exp(time.varying.treatment.effect*log(time)+fixed.treatment.effect)
    }
  }
  
  return(true.hr)
  
}