#input:
#1.muhaz.hr.df.list=this is the list with data frames that contains hazard ratio estimations
#2. true.hr= this is a vector with the same length of the above data frame. this vector contains the true hazard ratio 
#3.path.results= this is the path to save all the tables and the plots
#output: no output-just saving the plots and the tables

#what the functions does: Its creating the bias, MSE and SE takes and than creates the plots


save_all_tables_no_boot<-function(muhaz.hr.df.list,true.hr,path.results,n,tau){
  
  ####################################Creating the statistics table####################################################
  kernel.results<-creating.statistics.bias.data(muhaz.hr.df.list,"HR")$statistics.table
  cox.results<-creating.statistics.bias.data(muhaz.hr.df.list,"HR.sp")$statistics.table
  cox.con.results<-creating.statistics.bias.data(muhaz.hr.df.list,"con.cox")$statistics.table
  
  #add BSE results for kernel and cox results
  kernel.results<-cbind(kernel.results)
  
  #The true HR
  time<-kernel.results$time
  true.hr<-true.hr
  kernel.results$true.hr<-cox.results$true.hr<-true.hr
  #####################################################
  # #Bootsrap results-kernel
  # #BSE matrix
  # BSE.matrix<-data.frame(do.call(cbind,BSE))
  # BSE.matrix$ABSE <- rowMeans(BSE.matrix, na.rm=TRUE)
  # 
  # #add to each list the true hr
  # for( i in 1:length(perc.ci2)){
  #   perc.ci2[[i]]<-data.frame(perc.ci2[[i]],true.hr=true.hr,CI.coverage=rep(0,length(true.hr)))
  #   
  #   for( j in 1:nrow(perc.ci2[[i]])){
  #     if(perc.ci2[[i]]$true.hr[j]>=perc.ci2[[i]][j,1]&&perc.ci2[[i]]$true.hr[j]<=perc.ci2[[i]][j,2]){
  #       perc.ci2[[i]]$CI.coverage[j]<-1
  #     }
  #     
  #   }
  #   #perc.ci2[[i]]$CI.coverage<-ifelse(kernel.results$true.hr>=perc.ci2[[i]][,1]&&kernel.results$true.hr<=perc.ci2[[i]][,2], 1,0)
  # }
  # temp = lapply(perc.ci2, function(x){x[,'CI.coverage']})
  # temp.df= data.frame(do.call(rbind,temp))
  # CI.coverage.percent=apply(temp.df,2,function(x){sum(x)*100/length(muhaz.hr.df.list)})
  # 
  # 
  # tempX2.5. = lapply(perc.ci2, function(x){x[,'X2.5.']})
  # temp.dfX2.5.= data.frame(do.call(rbind,tempX2.5.))
  # mean.cIL.kernel<-apply(temp.dfX2.5.,2,function(x){mean(x,na.rm = TRUE)})
  # tempX97.5. = lapply(perc.ci2, function(x){x[,'X97.5.']})
  # temp.dfX97.5.= data.frame(do.call(rbind,tempX97.5.))
  # mean.cIU.kernel<-apply(temp.dfX97.5.,2,function(x){mean(x,na.rm = TRUE)})
  # 
  # #IQR calculations
  # tempX75. = lapply(perc.ci2, function(x){x[,'X75.']})
  # temp.dfX75.= data.frame(do.call(rbind,tempX75.))
  # mean.q3.kernel<-apply(temp.dfX75.,2,function(x){mean(x,na.rm = TRUE)})
  # tempX25. = lapply(perc.ci2, function(x){x[,'X25.']})
  # temp.dfX25.= data.frame(do.call(rbind,tempX25.))
  # mean.q1.kernel<-apply(temp.dfX25.,2,function(x){mean(x,na.rm = TRUE)})
  
  
  # temp.df<-data.frame(t(data.frame(rbind(time=kernel.results$time,temp.df))))
  # write.csv(temp.df,paste(path.results,"/CI.coverage.per.simulation",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  # write.csv(temp.dfX2.5.,paste(path.results,"/CIL.kernel.per.simulation",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  # write.csv(temp.dfX97.5.,paste(path.results,"/CIU.kernel.per.simulation",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  # write.csv(temp.dfX25.,paste(path.results,"/q1.kernel.per.simulation",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  # write.csv(temp.dfX75.,paste(path.results,"/q3.kernel.per.simulation",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  # 
  # ####################################
  # #bootstrap results-cox
  # BSE.cox.matrix<-data.frame(do.call(cbind,BSE.cox))
  # BSE.cox.matrix$ABSE <- rowMeans(BSE.cox.matrix, na.rm=TRUE)
  # 
  # #add to each list the true hr
  # for( i in 1:length(perc.ci.cox)){
  #   perc.ci.cox[[i]]<-data.frame(perc.ci.cox[[i]],true.hr=true.hr,CI.coverage=rep(0,length(true.hr)))
  #   
  #   for( j in 1:nrow(perc.ci.cox[[i]])){
  #     if(perc.ci.cox[[i]]$true.hr[j]>=perc.ci.cox[[i]][j,1]&&perc.ci.cox[[i]]$true.hr[j]<=perc.ci.cox[[i]][j,2]){
  #       perc.ci.cox[[i]]$CI.coverage[j]<-1
  #     }
  #     
  #   }
  #   
  # }
  # temp = lapply(perc.ci.cox, function(x){x[,'CI.coverage']})
  # temp.df= data.frame(do.call(rbind,temp))
  # cox.CI.coverage.percent=apply(temp.df,2,function(x){sum(x)*100/length(muhaz.hr.df.list)})
  # 
  # 
  # tempX2.5. = lapply(perc.ci.cox, function(x){x[,'X2.5.']})
  # temp.dfX2.5.= data.frame(do.call(rbind,tempX2.5.))
  # mean.cIL.cox<-apply(temp.dfX2.5.,2,function(x){mean(x,na.rm = TRUE)})
  # tempX97.5. = lapply(perc.ci.cox, function(x){x[,'X97.5.']})
  # temp.dfX97.5.= data.frame(do.call(rbind,tempX97.5.))
  # mean.cIU.cox<-apply(temp.dfX97.5.,2,function(x){mean(x,na.rm = TRUE)})
  # 
  # #IQR calculations
  # tempX75. = lapply(perc.ci.cox, function(x){x[,'X75.']})
  # temp.dfX75.= data.frame(do.call(rbind,tempX75.))
  # mean.q3.cox<-apply(temp.dfX75.,2,function(x){mean(x,na.rm = TRUE)})
  # tempX25. = lapply(perc.ci.cox, function(x){x[,'X25.']})
  # temp.dfX25.= data.frame(do.call(rbind,tempX25.))
  # mean.q1.cox<-apply(temp.dfX25.,2,function(x){mean(x,na.rm = TRUE)})
  # 
  # temp.df<-data.frame(t(data.frame(rbind(time=cox.results$time,temp.df))))
  # write.csv(temp.df,paste(path.results,"/cox.CI.coverage.per.simulation",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  # write.csv(temp.dfX2.5.,paste(path.results,"/CIL.cox.per.simulation",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  # write.csv(temp.dfX97.5.,paste(path.results,"/CIU.cox.per.simulation",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  # write.csv(temp.dfX25.,paste(path.results,"/q1.cox.per.simulation",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  # write.csv(temp.dfX75.,paste(path.results,"/q3.cox.per.simulation",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  # 
  ######################################################
  # The raw estimatios
  all.estimations.kernel.results<-creating.statistics.bias.data(muhaz.hr.df.list,"HR")$all.muhaz.estimation
  all.estimations.cox.results<-creating.statistics.bias.data(muhaz.hr.df.list,"HR.sp")$all.muhaz.estimation
  #
  
  #The bias table
  mean.bias.table<-data.frame(time=time,mean.kernel=kernel.results$mean.HR,
                              mean.cox=cox.results$mean.HR,
                              true.HR=cox.results$true.hr,
                              SE.kernel=kernel.results$SE.HR,SE.cox=cox.results$SE.HR,
                              mean.cox.con=cox.con.results$mean.HR,
                              SE.cox.con=cox.con.results$SE.HR) %>% 
    mutate(bias.kernel=mean.kernel-true.HR,bias.cox=mean.cox-true.HR,
           bias.cox.con=mean.cox.con-true.HR,
           r.bias.kernel=bias.kernel*100/true.HR,r.bias.cox=bias.cox*100/true.HR,
           r.bias.cox.con=bias.cox.con*100/true.HR
    )
  
  mean.bias.table$MSE.cox<-calculate.MSE(all.estimations.cox.results,true.hr)
  mean.bias.table$MSE.kernel<-calculate.MSE(all.estimations.kernel.results,true.hr)
  # mean.bias.table$ABSE<-BSE.matrix$ABSE
  # mean.bias.table$CI.coverage.percent<-CI.coverage.percent
  # mean.bias.table$mean.cIL.kernel<-mean.cIL.kernel
  # mean.bias.table$mean.cIU.kernel<-mean.cIU.kernel
  # mean.bias.table$mean.B.q1.kernel<-mean.q1.kernel
  # mean.bias.table$mean.B.q3.kernel<-mean.q3.kernel
  # mean.bias.table$mean.q1.kernel<-kernel.results$X25.
  # mean.bias.table$mean.q3.kernel<-kernel.results$X75.
  
  # mean.bias.table$cox.ABSE<-BSE.cox.matrix$ABSE
  # mean.bias.table$cox.CI.coverage.percent<-cox.CI.coverage.percent
  # mean.bias.table$mean.cIL.cox<-mean.cIL.cox
  # mean.bias.table$mean.cIU.cox<-mean.cIU.cox
  # mean.bias.table$mean.B.q1.cox<-mean.q1.cox
  # mean.bias.table$mean.B.q3.cox<-mean.q3.cox
  # mean.bias.table$mean.q1.kernel<-cox.results$X25.
  # mean.bias.table$mean.q3.kernel<-cox.results$X75.
  
  #IQR
  mean.bias.table$IQR.simulations.cox<-cox.results$X75.-cox.results$X25.
  #mean.bias.table$IQR.boot.cox<-mean.q3.cox-mean.q1.cox
  
  mean.bias.table$IQR.simulations.kernel<-kernel.results$X75.-kernel.results$X25.
  #mean.bias.table$IQR.boot.kernel<-mean.q3.kernel-mean.q1.kernel
  #print("until here")
  bias<-c(mean(mean.bias.table$bias.kernel),mean(mean.bias.table$bias.cox))
  MSE<-c(mean(mean.bias.table$MSE.kernel),mean(mean.bias.table$MSE.cox))
  SE<-c(mean(kernel.results$SE.HR),mean(cox.results$SE.HR))
  #aveage bSE -need to correct
  # ABSE<-c(mean(BSE.matrix$ABSE),mean(BSE.cox.matrix$ABSE))
  # ACI.coverage<-c(mean(CI.coverage.percent),mean(cox.CI.coverage.percent))
  avrage.bias.all<-data.frame(rbind(bias,MSE,SE))
  names( avrage.bias.all)<-c("Kernel","Cox")
  #print("until here 2")
  #avrage.bias.without0<-data.frame(Type="without 0",avrage.bias.kernel=mean(mean.bias.table$bias.kernel[2:n.est.grid]),avrage.bias.cox=mean(mean.bias.table$bias.cox[2:n.est.grid]))  
  #avrage.bias.all<-rbind(avrage.bias.all,avrage.bias.without0)
  avrage.bias.all<-apply(avrage.bias.all,c(1,2) ,function(u){ formatC(u, digits = 3, format = "f")})
  row.names(avrage.bias.all)<-c("bias","MSE","SE")
    write.csv(all.estimations.kernel.results,paste(path.results,"/all.estimations.kernel.results_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  write.csv(all.estimations.cox.results,paste(path.results,"/all.estimations.cox.results_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  
  #the satastics table 
  write.csv(kernel.results,paste(path.results,"/kernel.results_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  write.csv(cox.results,paste(path.results,"/cox.results_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  #the bias table
  write.csv(mean.bias.table,paste(path.results,"/mean.bias.table_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  write.csv(avrage.bias.all,paste(path.results,"/avrage.bias.all_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"))
  
  
  #####################################################################################################
  #ggplot
  cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
            "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  #creating long tables
  kernel.results_long <- gather(kernel.results, Statistics, value, mean.HR:true.hr, factor_key=TRUE) %>% 
    filter(Statistics%in%c("mean.HR","median.HR","X25.",'X75.',"true.hr"))%>% mutate(Type="Kernel",log_value=log(value)) 
  
  write.csv( kernel.results_long,paste(path.results,"/ kernel.results_long_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"))
  
  cox.results_long <- gather(cox.results, Statistics, value, mean.HR:true.hr, factor_key=TRUE) %>% 
    filter(Statistics%in%c("mean.HR","median.HR","X25.",'X75.',"true.hr")) %>% mutate(Type="Cox",log_value=log(value))
  
  all.long<-rbind(kernel.results_long,cox.results_long)
  all.long$Type[all.long$Statistics=="true.hr"]<-"true.hr"
  
  
  #creating plot with only means of estimation
  all.long %>% filter(Statistics%in%c("mean.HR","true.hr")) %>% 
    ggplot(aes(x=time,y=value,color=Type))+
    geom_line(aes(linetype =Statistics))+
    scale_colour_manual(values = cbp1)+ theme_bw()+
    theme(panel.background = element_rect(colour = "black"),
          legend.position = c(0.7, 0.7),legend.background=element_blank())+
    labs(title="", x ="Time", y = "Causal HR(t)")
  
  ggsave(paste(path.results,"/mean_plot_n_",n,'_tau_',tau,Sys.Date(),"_",sep = "",".jpeg"),width = 8)
  
  #creating plot with quantiles of estimation
  all.long %>% filter(Statistics!="mean.HR") %>% 
    ggplot(aes(x=time,y=value,color=Type))+
    geom_line(aes(linetype =Statistics))+
    scale_colour_manual(values = cbp1)+ theme_bw()+
    theme(panel.background = element_rect(colour = "black"),
          legend.position = c(0.7, 0.83),legend.background=element_blank())+
    labs(title="", x ="Time", y = "Causal HR(t)")
  
  ggsave(paste(path.results,"/quantiles_plot_n_",n,'_tau_',tau,Sys.Date(),"_",sep = "",".jpeg"),width = 8)
  
  #creating bias information
  mean.bias.table%>% ggplot(aes(x=time,y=bias.kernel,color="Kernel"))+geom_line()+
    geom_line(aes(x=time,y=bias.cox,color="Cox"))+
    theme_bw()+
    theme(panel.background = element_rect(colour = "black"),
          legend.position = c(0.9, 0.2),legend.background=element_blank())+
    labs(title="", x ="Time", y = "Bias")+geom_hline(yintercept=0, linetype="dashed", color = "black")
  
  ggsave(paste(path.results,"/bias_plot_n_",n,'_tau_',tau,Sys.Date(),"_",sep = "",".jpeg"),width = 8)
  
  
  #creating MSE information
  mean.bias.table%>% ggplot(aes(x=time,y=MSE.kernel,color="Kernel"))+geom_line()+
    geom_line(aes(x=time,y=MSE.cox,color="Cox"))+
    theme_bw()+
    theme(panel.background = element_rect(colour = "black"),
          legend.position = c(0.9, 0.2),legend.background=element_blank())+
    labs(title="", x ="Time", y = "MSE")+geom_hline(yintercept=0, linetype="dashed", color = "black")
  
  ggsave(paste(path.results,"/MSE_plot_n_",n,'_tau_',tau,Sys.Date(),"_",sep = "",".jpeg"),width = 8)
  
  
  #creating SE information
  mean.bias.table%>% ggplot(aes(x=time,y=SE.kernel,color="Kernel"))+geom_line()+
    geom_line(aes(x=time,y=SE.cox,color="Cox"))+
    theme_bw()+
    theme(panel.background = element_rect(colour = "black"),
          legend.position = c(0.9, 0.2),legend.background=element_blank())+
    labs(title="", x ="Time", y = "SE")+geom_hline(yintercept=0, linetype="dashed", color = "black")
  
  ggsave(paste(path.results,"/SE_plot_n_",n,'_tau_',tau,Sys.Date(),"_",sep = "",".jpeg"),width = 8)
  
  # #creating CI bootsrap
  # mean.bias.table%>%ggplot(aes(x = time, y = mean.kernel, group = 1)) + 
  #   geom_line(col='red') + 
  #   geom_ribbon(aes(ymin = mean.cIL.kernel, ymax = mean.cIU.kernel), alpha = 0.5)+
  #   labs(title="", x ="Time", y = "Causal HR(t)")+ theme_bw()
  # 
  # ggsave(paste(path.results,"/CI_kernel_plot_n_",n,'_tau_',tau,Sys.Date(),"_",sep = "",".jpeg"),width = 8)
  # 
  # #creating CI cox bootsrap
  # mean.bias.table%>%ggplot(aes(x = time, y = mean.cox, group = 1)) + 
  #   geom_line(col='red') + 
  #   geom_ribbon(aes(ymin = mean.cIL.cox, ymax = mean.cIU.cox), alpha = 0.5)+
  #   labs(title="", x ="Time", y = "Causal HR(t)")+ theme_bw()
  # 
  # ggsave(paste(path.results,"/CI_cox_plot_n_",n,'_tau_',tau,Sys.Date(),"_",sep = "",".jpeg"),width = 8)
  # 
  # 
  # #creating plot with only means of estimation - in logs HR 
  # all.long %>% filter(Statistics%in%c("mean.HR","true.hr")) %>% 
  #   ggplot(aes(x=time,y=log_value,color=Type))+
  #   geom_line(aes(linetype =Statistics))+
  #   scale_colour_manual(values = cbp1)+ theme_bw()+
  #   theme(panel.background = element_rect(colour = "black"),
  #         legend.position = c(0.7, 0.7),legend.background=element_blank())+
  #   labs(title="", x ="Time", y = "Causal HR(t)")
  
  #ggsave(paste(path.results,"/log_mean_plot_n_",n,'_tau_',tau,Sys.Date(),"_",sep = "",".jpeg"),width = 8)
  #creating the cox+kernel plot toghether
  # create.plot.combined.CI(mean.bias.table)
  # ggsave(paste(path.results,"/CI_cox_kernel_plot_n_",n,'_tau_',tau,Sys.Date(),"_",sep = "",".jpeg"),width = 8,height = 7)
  # 
  #creating plot with only means of estimation - in logs HR 
  all.long %>% filter(Statistics%in%c("mean.HR","true.hr")) %>% 
    ggplot(aes(x=time,y=log_value,color=Type))+
    geom_line(aes(linetype =Statistics))+
    scale_colour_manual(values = cbp1)+ theme_bw()+
    theme(panel.background = element_rect(colour = "black"),
          legend.position = c(0.7, 0.7),legend.background=element_blank())+
    labs(title="", x ="Time", y = "Causal HR(t)")
  
  ggsave(paste(path.results,"/log_mean_plot_n_",n,'_tau_',tau,Sys.Date(),"_",sep = "",".jpeg"),width = 8)
  
  
  #creating relative bias plot 
  mean.bias.table%>% ggplot(aes(x=time,y=r.bias.kernel,color="Kernel"))+geom_line()+
    geom_line(aes(x=time,y=r.bias.cox,color="Cox"))+
    theme_bw()+
    theme(panel.background = element_rect(colour = "black"),
          legend.position = c(0.9, 0.2),legend.background=element_blank())+
    labs(title="", x ="Time", y = "Bias")+geom_hline(yintercept=0, linetype="dashed", color = "black")
  
  ggsave(paste(path.results,"/relative_bias_plot_n_",n,'_tau_',tau,Sys.Date(),"_",sep = "",".jpeg"),width = 8)
  
  #creating relative bias plot-with conditional cox  
  mean.bias.table%>% ggplot(aes(x=time,y=r.bias.kernel,color="Kernel"))+geom_line()+
    geom_line(aes(x=time,y=r.bias.cox,color="Cox"))+
    geom_line(aes(x=time,y=r.bias.cox.con,color="Conditional.Cox"))+
    theme_bw()+
    theme(panel.background = element_rect(colour = "black"),
          legend.position = c(0.9, 0.2),legend.background=element_blank())+
    labs(title="", x ="Time", y = "Bias")+geom_hline(yintercept=0, linetype="dashed", color = "black")
  
  ggsave(paste(path.results,"/relative_bias_w_con_cox_plot_n_",n,'_tau_',tau,Sys.Date(),"_",sep = "",".jpeg"),width = 8)
  
  
}