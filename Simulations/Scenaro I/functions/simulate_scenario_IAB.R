#function to create observed data
#if the HRC is time varying than the HR_OBS is const, and in addition the marginal modal is Cox model
simulate.scenario.IAB<-function(lamdas=0.2,
                                gamma=1.5,
                               n =10000,
                               theta=0.8,
                               p.a=0.5,
                               beta.a,
                               scenario="IA",
                               rate.C=0.1
                               #ad.c=10
                               
){
  #######################################################################################################
  #in case of no confounder, I need to get hazard from \lamdat(t;v)=exp(logv+beta.a*a+theta*exp(beta.a.*a)*t)
  #the DGM is taken from Martinussen
  #this time varying causal hazard ratio
  if (scenario=="IA"){
    #lamdas=1
    #sim frailty
    frailty<-rgamma(n,shape = 1/theta,scale = theta)#the frailty 
    #sim survival times
    #first stage
    V.0<-rexp(n,rate=lamdas)
    V.1<-rexp(n,rate=lamdas)
    #second stage
    T.0<-(1/(theta*lamdas))*log(theta*V.0/frailty+1) 
    T.1<-(1/(theta*exp(beta.a)*lamdas))*log(theta*V.1/frailty+1) 
    
    #the potential outcome datset 
    id<-1:n
    potential.outcomes<-data.frame(id=id,T.1=T.1,T.0=T.0)
     
    
  }
  #this is constant causal hazard ratio
  #the conditional hazard rate here is cox model
  if (scenario=="IB"){
    #lamdas=1
    #sim frailty
    frailty<-rgamma(n,shape = 1/theta,scale = theta)#the frailty 
    #sim survival times
    #first stage
    V.0<-runif(n)
    V.1<-runif(n)
    #second stage
    #T.0<-(-log(V.0))/(lamdas*exp(log(frailty)))
    #T.1<-(-log(V.1))/(lamdas*exp(log(frailty)+beta.a))
    #gompertz
    T.0<-1/gamma*(log(1-(gamma*log(V.0)/(lamdas*exp(log(frailty))))))
    T.1<-1/gamma*(log(1-(gamma*log(V.1)/(lamdas*exp(log(frailty)+beta.a)))))
    #the potential outcome datset 
    id<-1:n
    potential.outcomes<-data.frame(id=id,T.1=T.1,T.0=T.0)
    
    
  }
  #####################################################################observed data########################################
  

  treatment<-rbinom(n, 1, p.a)
  T.obs<-(1-treatment)*potential.outcomes$T.0+treatment*potential.outcomes$T.1
  # admistaritive censoring
  # if(!is.na(ad.c)){
  #   T.obs.f<-pmin(T.obs,ad.c)
  #   status<-ifelse(T.obs.f==ad.c,0,1)
  # }
  C<-rexp(n,rate=rate.C)
  T.obs.f<-pmin(T.obs,C)
  status<-ifelse(T.obs.f==C,0,1)
  
  #observed data
  observed.data<-data.frame(id,time=T.obs.f,treatment=treatment,status=status)
  
  
  
  return(observed.data)
  
}