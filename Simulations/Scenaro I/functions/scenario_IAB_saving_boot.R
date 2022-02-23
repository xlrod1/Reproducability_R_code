scenario.IAB.saving.boot<-function(confounder="no",
                                        lamdas=0.2,
                                        gamma=1.5,
                                       n =10000,
                                       tau=0.8,
                                       p.a=0.5,
                                       beta.a=log(0.5),
                                       scenario="IA",
                                       #ad.c=10,
                                   rate.C=0.1,
                                       path.results,
                                      n.simulate=1000,
                                      R=300,
                                      parallel="no",
                                      ncpus=8,
                                      kern="e",
                                      end.time=NULL,
                                      min.time=0,
                                      b.cor="b",
                                      n.est.grid=51,
                                   sn=1){

  
  theta<-theta.gamma(tau)
  ##############################################Set the end point estimation#######################
  if(is.null(end.time)){
    a<-NULL
    while(is.null(a)){
      try(
        {
          a<-simulate.scenario.IAB(lamdas=lamdas,
                                   gamma=gamma,
                                       n =n,
                                       theta=theta,
                                       p.a=p.a,
                                       beta.a=beta.a,
                                       scenario=scenario,
                                       rate.C=rate.C
                                       ) 
   
        }
      )
    }
    data<-a

      # to know where to end.time
      data.control<-data%>% filter(treatment==0)
      data.treatment<-data%>% filter(treatment==1)
      
      #what is the end point
      sfit <- survfit(Surv(data.treatment$time, data.treatment$status) ~ 1)
      #end.treatment <- sfit$time[which.min(abs(sfit$surv-0.2))]
      end.treatment <- approx(sfit$n.risk, sfit$time, xout = 10)$y
      min.treatment<- sfit$time[min(which(sfit$cumhaz > 0))]
      
      
      sfit <- survfit(Surv(data.control$time, data.control$status) ~ 1)
      #end.control <-  sfit$time[which.min(abs(sfit$surv-0.2))]
      end.control <- approx(sfit$n.risk, sfit$time, xout = 10)$y
      min.control<- sfit$time[min(which(sfit$cumhaz > 0))]
      
      
      end.time<-min(end.treatment ,end.control)
      min.time<-max(min.treatment, min.control)
      
      
     
    
    
  }
  
  #####################################################Repeats####################################################
  emirical.min.time<-c()
  
 
  muhaz.hr.df.list<-muhaz.hr.df.list.full<-list()
  cen.per<-c()
  BSE<- BSE.cox<-list()
  perc.ci2<-perc.ci.cox<-list()
  errors.file<-data.frame()
  for( i in 1:n.simulate){
    #simulate data 
    a<-NULL
    while(is.null(a)){
      try(
        {
            a<-simulate.scenario.IAB(lamdas=lamdas,
                                     gamma=gamma,
                                     n =n,
                                     theta=theta,
                                     p.a=p.a,
                                     beta.a=beta.a,
                                     scenario=scenario,
                                     rate.C=rate.C
                                    )
                                          
             
        }
      )
    }

    

    #muhaz estimation
    bb.sample<-muhaz_hr(a,theta=theta, n.est.grid = n.est.grid,b.cor=b.cor,end.time =end.time,min.time =min.time,kern=kern)
    #Cox estimatio
    cox.reg<-coxph(Surv(time,status)~treatment,data=a)
    
    
    
    
    #Out files of muhaz
    muhaz.hr.df.list[[i]]<-bb.sample$muhaz.hr.df
    muhaz.hr.df.list.full[[i]]<-bb.sample$df.full  
    
    #Cox regression calculations 
    beta.hat<-as.numeric(cox.reg$coefficients[1])
    time<-muhaz.hr.df.list[[i]]$time
    base.haz.estimator<-stepfun(x=basehaz(cox.reg,centered = FALSE)$time,y=c(0,basehaz(cox.reg,centered = FALSE)$hazard))
    base.line.cumsum<- base.haz.estimator(time) 
    
    HR.sp<-exp(beta.hat)*exp(theta*base.line.cumsum*(exp(beta.hat)-1))
    muhaz.hr.df.list[[i]]$HR.sp<-HR.sp
    
    #Naive
    r.cox<-coxph(Surv(time,status)~treatment,data=a)
    muhaz.hr.df.list[[i]]$con.cox<-rep(exp(coef(r.cox)[1]), n.est.grid)
    
    #save muhaz df 
    path.estimation.results=paste(path.results,"/estimation_results/",sep="")
    write.csv(muhaz.hr.df.list[[i]],paste(path.estimation.results,"/estimation_rep",sn,"_",i,"_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
    
    
    
    #the true hr by the quadratic hazard ratio 
    time<-muhaz.hr.df.list[[1]]$time
    #the true form is also from Martinussen 
    if(scenario=="IA"){
      true.hr<-exp(beta.a)*exp(lamdas*time*theta*(exp(beta.a)-1))
    }
    if(scenario=="IB"){
      true.hr<-rep(exp(beta.a),n.est.grid)
    }
    ###################################
    #Bootstrap results
    if (parallel=="no"){
      results<- boot(data=a, statistic=estimation.for.bootsrap,R=R,confounder=confounder, theta=theta, n.est.grid=n.est.grid ,b.cor=b.cor ,end.time=end.time,min.time=min.time,kern=kern )  
      results$t<-apply(results$t,c(1,2),function(x){ifelse(x%in%c(Inf,-Inf),NA,x)})
      results$t<-apply(results$t,c(1,2),function(x){ifelse(x>10||x<(-10),NA,x)})
      
     errors.file[i,1:n.est.grid]<-data.frame(results$t )%>%
        dplyr::select(everything()) %>%  # replace to your needs
        summarise_all(funs(sum(is.na(.))))
      
    
     # path.results.boot=paste(path.results,"/bootsrap_results/",sep="")
     # write.csv(results$t,paste(path.results.boot,"/boot_results_rep",sn,"_",i,"_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
     # 
     #BSE[[i]]<-apply(results$t[(1:R),(1:n.est.grid)], 2, function(u){sqrt(var(u,na.rm = TRUE))})
     
     #perc.ci2[[i]]<-data.frame(t(apply(results$t[(1:R),(1:n.est.grid)], 2, function(u){quantile(u, probs = c(.025, .975,0.25,0.75), type = 6,na.rm = TRUE)})))
     ###################################
      #Bootstrap for cox
      # results<- boot(data=a, statistic=cox.estimation.for.bootsrap,R=R, theta=theta,  time= time,confounder=confounder)  
      # results$t<-apply(results$t,c(1,2),function(x){ifelse(x%in%c(Inf,-Inf),NA,x)})
      # results$t<-apply(results$t,c(1,2),function(x){ifelse(x>10||x<(-10),NA,x)})
      # 
      #BSE.cox[[i]]<-apply(results$t[(1:R),((n.est.grid+1):(2*n.est.grid))], 2, function(u){sqrt(var(u,na.rm = TRUE))})
      #perc.ci.cox[[i]]<-data.frame(t(apply(results$t[(1:R),((n.est.grid+1):(2*n.est.grid))], 2, function(u){quantile(u, probs = c(.025, .975,.25,.75), type = 6,na.rm = TRUE)})))
     summary.statistics.boot(results$t,true.hr,sn,i)   
    }
    #parallel computation for cluster!
    else{
      results<- boot(data=a, statistic=estimation.for.bootsrap,R=R, confounder=confounder,theta=theta, n.est.grid=n.est.grid ,b.cor=b.cor ,end.time=end.time,min.time=min.time,kern=kern ,parallel = "multicore",ncpus=ncpus)  
      results$t<-apply(results$t,c(1,2),function(x){ifelse(x%in%c(Inf,-Inf),NA,x)})
      results$t<-apply(results$t,c(1,2),function(x){ifelse(x>10||x<(-10),NA,x)})
      
      errors.file[i,1:(2*n.est.grid)]<-data.frame(results$t )%>%
        dplyr::select(everything()) %>%  # replace to your needs
        summarise_all(funs(sum(is.na(.))))
     
      #path.results.boot=paste(path.results,"/bootsrap_results/",sep="")
      #write.csv(results$t,paste(path.results.boot,"/boot_results_rep",sn,"_",i,"_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
      
      #BSE[[i]]<-apply(results$t[(1:R),(1:n.est.grid)], 2, function(u){sqrt(var(u,na.rm = TRUE))})
    
      #perc.ci2[[i]]<-data.frame(t(apply(results$t[(1:R),(1:n.est.grid)], 2, function(u){quantile(u, probs = c(.025, .975,0.25,0.75), type = 6,na.rm = TRUE)})))
      
      ###################################
      #Bootstrap for cox
      #results<- boot(data=a, statistic=cox.estimation.for.bootsrap,R=R, theta=theta,  time= time,confounder=confounder,parallel ="multicore",ncpus=ncpus)  
      #results$t<-apply(results$t,c(1,2),function(x){ifelse(x%in%c(Inf,-Inf),NA,x)})
      #results$t<-apply(results$t,c(1,2),function(x){ifelse(x>10||x<(-10),NA,x)})
      #BSE.cox[[i]]<-apply(results$t[(1:R),((n.est.grid+1):(2*n.est.grid))], 2, function(u){sqrt(var(u,na.rm = TRUE))})
      #perc.ci.cox[[i]]<-data.frame(t(apply(results$t[(1:R),((n.est.grid+1):(2*n.est.grid))], 2, function(u){quantile(u, probs = c(.025, .975,.25,.75), type = 6,na.rm = TRUE)})))
      summary.statistics.boot(results$t,true.hr,sn,i) 
    }
    
    
    
    
    emirical.min.time[i]<-min(a$time)
    cen.per[i]<-sum(a$status==0)/n
    print(i)

    
  }
  
  
  
  
  #save_all_tables(muhaz.hr.df.list,true.hr,path.results,n,tau,BSE,perc.ci2,BSE.cox,perc.ci.cox)

  #the parameter data
  data.of.parameters<-data.frame(parameter=c("lamdas","n" ,"theta",
                                             "p.a","beta.a","n_simulate","scenario","rate.C","b.cor","n.est.grid","cen.per","confounder","tau","parallel","ncpus","emirical.min.time"),value=c(lamdas,
                                                                                                                                                                                        n ,
                                                                                                                                                                                        theta,
                                                                                                                                                                                        p.a,
                                                                                                                                                                                        beta.a,
                                                                                                                                                                                        n.simulate,
                                                                                                                                                                                        scenario,
                                                                                                                                                                                        rate.C,
                                                                                                                                                                                        b.cor,
                                                                                                                                                                                        n.est.grid,
                                                                                                                                                                                        mean(cen.per),
                                                                                                                                                                                        confounder,tau,parallel,ncpus,mean(emirical.min.time)))
  
  #save the results
  write.csv(data.of.parameters,paste(path.results,"/data.of.parameters_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  write.csv(errors.file,paste(path.results,"/error.files_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  
}
 