condition.on.x.saving.no.boot<-function(n=5000,
                                    tau=0.7,
                                     beta.x=log(0.5),
                                     lamdas=1,
                                     beta.a=log(0.5),
                                     beta.ax=log(0.5),
                                     ad.c=10,
                                     path.results,
                                     n.simulate,
                                     kern="e",
                                     end.time=NULL,
                                     min.time=0,
                                     b.cor="b",
                                     n.est.grid=51){

  
  theta<-theta.gamma(tau)
  confounder="yes"
  ##############################################Set the end point estimation#######################
  if(is.null(end.time)){
    a<-NULL
    while(is.null(a)){
      try(
        {
    a<-simulate.con.x.cox(n=n,theta=theta,beta.x=beta.x,lamdas=lamdas,
                          beta.a=beta.a, beta.ax= beta.ax,ad.c=ad.c)
        }
      )
    }
    data<-a$obs.data
    
    
      #calculate weights 
      Pr1 <- glm(treatment~ confounder, data=data,
                 family=binomial(link = "logit"))$fitted.values
      P.A<-sum(data$treatment==1)/n
      data$W <- (data$treatment==1) *P.A*(1/Pr1) + (data$treatment==0) *(1-P.A)*(1)/(1-Pr1)
      # to know where to end.time
      data.control<-data%>% filter(treatment==0)
      data.treatment<-data%>% filter(treatment==1)
      
      
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
            a<-simulate.con.x.cox(n=n,theta=theta,beta.x=beta.x,lamdas=lamdas,
                                  beta.a=beta.a, beta.ax= beta.ax,ad.c=ad.c)
                                          
             
        }
      )
    }
    
    a<-a$obs.data
      #calculate weights 
      Pr1 <- glm(treatment~ confounder, data=a,
                 family=binomial(link = "logit"))$fitted.values
      P.A<-sum(a$treatment==1)/n
      a$W <- (a$treatment==1) *P.A*(1/Pr1) + (a$treatment==0) *(1-P.A)*(1)/(1-Pr1)
      #muhaz estimation
      bb.sample<-muhaz_hr_weights(a,theta=theta, n.est.grid = n.est.grid,b.cor=b.cor,end.time =end.time,min.time =min.time,kern=kern)
      #Cox estimatio
      cox.reg<-coxph(Surv(time,status)~treatment,data=a,weights = W)
    
    
    
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
    r.cox<-coxph(Surv(time,status)~treatment+confounder,data=a)
    muhaz.hr.df.list[[i]]$con.cox<-rep(exp(coef(r.cox)[1]), n.est.grid)
    
    
   
    
    
    
    
    emirical.min.time[i]<-min(a$time)
    cen.per[i]<-sum(a$status==0)/n
    print(i)

    
  }
  
  #the true hr by the quadratic hazard ratio 
  time<-muhaz.hr.df.list[[1]]$time
  #the true form is also from Martinussen 
  true.hr<-rep(exp(beta.a),n.est.grid)
  
  #muhaz.hr.df.list,true.hr,path.results,n,tau
  save_all_tables_no_boot(muhaz.hr.df.list,true.hr,path.results,n,tau)
  
  

  #the parameter data
  data.of.parameters<-data.frame(parameter=c("n",
                                             "tau",
                                             "theta",
                                             "beta.x",
                                             "lamdas",
                                            "beta.a",
                                             "beta.ax",
                                             "ad.c",
                                            "cen.per",
                                             "path.results",
                                             "n.simulate",
                                             "kern",
                                             "end.time",
                                             "min.time",
                                             "b.cor",
                                             "n.est.grid",
                                             "emirical.min.time"),value=c(n=n,
                                                                          tau,
                                                                          theta,
                                                                          beta.x,
                                                                          lamdas,
                                                                          beta.a,
                                                                          beta.ax,
                                                                          ad.c,
                                                                          mean(cen.per),
                                                                          path.results,
                                                                          n.simulate,
                                                                          kern,
                                                                          end.time,
                                                                          min.time,
                                                                          b.cor,
                                                                          n.est.grid,
                                                                          mean(emirical.min.time)))
  
  #save the results
  write.csv(data.of.parameters,paste(path.results,"/data.of.parameters_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  #write.csv(errors.file,paste(path.results,"/error.files_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  
}
  
#   
#   ####################################Creating the statistics table####################################################
#   kernel.results<-creating.statistics.bias.data(muhaz.hr.df.list,"HR")$statistics.table
#   cox.results<-creating.statistics.bias.data(muhaz.hr.df.list,"HR.sp")$statistics.table
#   
#   
#   #The true HR
#   time<-kernel.results$time
#   true.hr<-exp(fixed.treatment.effect+theta*time*(exp(fixed.treatment.effect)-1))
#   kernel.results$true.hr<-cox.results$true.hr<-exp(fixed.treatment.effect+theta*time*(exp(fixed.treatment.effect)-1))
#   
#   # The raw estimatios
#   all.estimations.kernel.results<-creating.statistics.bias.data(muhaz.hr.df.list,"HR")$all.muhaz.estimation
#   all.estimations.cox.results<-creating.statistics.bias.data(muhaz.hr.df.list,"HR.sp")$all.muhaz.estimation
#   #
#   
#   #The bias table
#   mean.bias.table<-data.frame(time=time,mean.kernel=kernel.results$mean.HR,mean.cox=cox.results$mean.HR,
#                               true.HR=cox.results$true.hr,
#                               SE.kernel=kernel.results$SE.HR,SE.cox=cox.results$SE.HR) %>% 
#     mutate(bias.kernel=true.HR-mean.kernel,bias.cox=true.HR-mean.cox)
#   
#   mean.bias.table$MSE.cox<-calculate.MSE(all.estimations.cox.results,true.hr)
#   mean.bias.table$MSE.kernel<-calculate.MSE(all.estimations.kernel.results,true.hr)
#   
#   
#   bias<-c(mean(mean.bias.table$bias.kernel),mean(mean.bias.table$bias.cox))
#   MSE<-c(mean(mean.bias.table$MSE.kernel),mean(mean.bias.table$MSE.cox))
#   SE<-c(mean(kernel.results$SE.HR),mean(cox.results$SE.HR))
#   
#   avrage.bias.all<-data.frame(rbind(bias,MSE,SE))
#   names( avrage.bias.all)<-c("Kernel","Cox")
#   
#   #avrage.bias.without0<-data.frame(Type="without 0",avrage.bias.kernel=mean(mean.bias.table$bias.kernel[2:n.est.grid]),avrage.bias.cox=mean(mean.bias.table$bias.cox[2:n.est.grid]))  
#   #avrage.bias.all<-rbind(avrage.bias.all,avrage.bias.without0)
#   avrage.bias.all<-apply(avrage.bias.all,c(1,2) ,function(u){ formatC(u, digits = 3, format = "f")})
#   row.names(avrage.bias.all)<-c("bias","MSE","SE")
#   #the parametr data
#   data.of.parameters<-data.frame(parameter=c("lamdas","gammas","n" ,"theta",
#                                              "fixed.treatment.effect","rate.C",
#                                              "p.a","n_simulate","b.cor","n.est.grid","cen.per","confounder","m.x","sd.x","beta.x","parametric.family","tau"),value=c(lamdas,
#                                                                                                                                                                gammas,
#                                                                                                                                                                n ,
#                                                                                                                                                                theta,
#                                                                                                                                                                fixed.treatment.effect,
#                                                                                                                                                                rate.C,
#                                                                                                                                                                p.a,
#                                                                                                                                                                n.simulate,
#                                                                                                                                                                b.cor,
#                                                                                                                                                                n.est.grid,
#                                                                                                                                                                mean(cen.per),
#                                                                                                                                                                confounder,
#                                                                                                                                                                m.x,sd.x,beta.x,
#                                                                                                                                                                parametric.family,tau))
#   
#   #save the results
#   write.csv(data.of.parameters,paste(path.results,"/data.of.parameters_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
#   write.csv(all.estimations.kernel.results,paste(path.results,"/all.estimations.kernel.results_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
#   write.csv(all.estimations.cox.results,paste(path.results,"/all.estimations.cox.results_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
#   
#   #the satastics table 
#   write.csv(kernel.results,paste(path.results,"/kernel.results_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
#   write.csv(cox.results,paste(path.results,"/cox.results_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
#   #the bias table
#   write.csv(mean.bias.table,paste(path.results,"/mean.bias.table_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
#   write.csv(avrage.bias.all,paste(path.results,"/avrage.bias.all_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"))
#   
#   
#   #####################################################################################################
#   #ggplot
#   cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
#             "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#   
#   #creating long tables
#   kernel.results_long <- gather(kernel.results, Statistics, value, mean.HR:true.hr, factor_key=TRUE) %>% 
#     filter(Statistics%in%c("mean.HR","median.HR","X25.",'X75.',"true.hr"))%>% mutate(Type="Kernel")
#   
#   cox.results_long <- gather(cox.results, Statistics, value, mean.HR:true.hr, factor_key=TRUE) %>% 
#     filter(Statistics%in%c("mean.HR","median.HR","X25.",'X75.',"true.hr")) %>% mutate(Type="Cox")
#   
#   all.long<-rbind(kernel.results_long,cox.results_long)
#   all.long$Type[all.long$Statistics=="true.hr"]<-"true.hr"
#   
#   
#   #creating plot with only means of estimation
#   all.long %>% filter(Statistics%in%c("mean.HR","true.hr")) %>% 
#     ggplot(aes(x=time,y=value,color=Type))+
#     geom_line(aes(linetype =Statistics))+
#     scale_colour_manual(values = cbp1)+ theme_bw()+
#     theme(panel.background = element_rect(colour = "black"),
#           legend.position = c(0.7, 0.7),legend.background=element_blank())+
#     labs(title="", x ="Time", y = "Causal HR(t)")
#   
#   ggsave(paste(path.results,"/mean_plot_n_",n,'_tau_',tau,Sys.Date(),"_",sep = "",".jpeg"),width = 8)
#   
#   #creating plot with quantiles of estimation
#   all.long %>% filter(Statistics!="mean.HR") %>% 
#     ggplot(aes(x=time,y=value,color=Type))+
#     geom_line(aes(linetype =Statistics))+
#     scale_colour_manual(values = cbp1)+ theme_bw()+
#     theme(panel.background = element_rect(colour = "black"),
#           legend.position = c(0.7, 0.83),legend.background=element_blank())+
#     labs(title="", x ="Time", y = "Causal HR(t)")
#   
#   ggsave(paste(path.results,"/quantiles_plot_n_",n,'_tau_',tau,Sys.Date(),"_",sep = "",".jpeg"),width = 8)
#   
#   #creating bias information
#   mean.bias.table%>% ggplot(aes(x=time,y=bias.kernel,color="Kernel"))+geom_line()+
#     geom_line(aes(x=time,y=bias.cox,color="Cox"))+
#     theme_bw()+
#     theme(panel.background = element_rect(colour = "black"),
#           legend.position = c(0.9, 0.2),legend.background=element_blank())+
#     labs(title="", x ="Time", y = "Bias")+geom_hline(yintercept=0, linetype="dashed", color = "black")
#   
#   ggsave(paste(path.results,"/bias_plot_n_",n,'_tau_',tau,Sys.Date(),"_",sep = "",".jpeg"),width = 8)
#   
#   
#   #creating MSE information
#   mean.bias.table%>% ggplot(aes(x=time,y=MSE.kernel,color="Kernel"))+geom_line()+
#     geom_line(aes(x=time,y=MSE.cox,color="Cox"))+
#     theme_bw()+
#     theme(panel.background = element_rect(colour = "black"),
#           legend.position = c(0.9, 0.2),legend.background=element_blank())+
#     labs(title="", x ="Time", y = "MSE")+geom_hline(yintercept=0, linetype="dashed", color = "black")
#   
#   ggsave(paste(path.results,"/MSE_plot_n_",n,'_tau_',tau,Sys.Date(),"_",sep = "",".jpeg"),width = 8)
#   
#   
#   #creating SE information
#   mean.bias.table%>% ggplot(aes(x=time,y=SE.kernel,color="Kernel"))+geom_line()+
#     geom_line(aes(x=time,y=SE.cox,color="Cox"))+
#     theme_bw()+
#     theme(panel.background = element_rect(colour = "black"),
#           legend.position = c(0.9, 0.2),legend.background=element_blank())+
#     labs(title="", x ="Time", y = "SE")+geom_hline(yintercept=0, linetype="dashed", color = "black")
#   
#   ggsave(paste(path.results,"/SE_plot_n_",n,'_tau_',tau,Sys.Date(),"_",sep = "",".jpeg"),width = 8)
#   
#   
# }
# 
