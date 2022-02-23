simulate.con.x.cox<-function(n=1000,
                             theta=1,
                             beta.x=log(0.5),
                             lamdas=1,
                             beta.a=log(0.5),
                             beta.ax=log(0.5),
                             # rate.C=0.2,
                             # q=0.25,
                             ad.c=10
                             ){
  

    #sim covariate
    x<-rnorm(n)
    # x.0<-rnorm(n)
    # x.1<-rnorm(n)
    
    #sim frailty
    frailty<-rgamma(n,shape = 1/theta,scale = theta)#the frailty \
    
    #sim survival times
    V.0<-runif(n)
    V.1<-runif(n)
    
    
    
    T.0<-(-log(V.0))/(lamdas*exp(log(frailty)+beta.x*x))
    T.1<-(-log(V.1))/(lamdas*exp(log(frailty)+beta.a+beta.x*x))
    
    
    id<-1:n
    
    
    potential.outcomes<-data.frame(id=id,T.1=T.1,T.0=T.0)
  
  
  
  #observed data
  #covariate
  treatment<-rbinom(n, 1, exp(beta.ax * x) / (1 + exp(beta.ax * x))) #confounding
  
  
  T.obs<-(1-treatment)*potential.outcomes$T.0+treatment*potential.outcomes$T.1
  #Censoring
  
  #C0<-C1<-C<-rexp(n,rate=rate.C)
  #censornug uniform-this is the one I used 
  #q.rate<-as.numeric(quantile(T.obs,q))
  #C<-runif(n,0,q.rate)
  
  # T.obs.f<-pmin(T.obs,C)
  # status<-ifelse(T.obs.f==C,0,1)
  
  
 # admistaritive censoring
  if(!is.na(ad.c)){
    T.obs.f<-pmin(T.obs,ad.c)
    status<-ifelse(T.obs.f==ad.c,0,1)
  }
  
  #final ibserved data 

    observed.data<-data.frame(id,time=T.obs.f,treatment=treatment,status=status,confounder=x)
    
  
  
  
  return(list(pot.data=potential.outcomes,obs.data=observed.data))
  
  
}
