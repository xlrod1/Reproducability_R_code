#input:
#1.muhaz.hr.df.list=this is the list with data frames that contains hazard ratio estimations
#2. true.hr= this is a vector with the same length of the above data frame. this vector contains the true hazard ratio 
#3.path.results= this is the path to save all the tables and the plots
#output: no output-just saving the plots and the tables

#what the functions does: Its creating the bias, MSE and SE takes and than creates the plots
# 
 # ci.kernel<-perc.ci2
 #  ci.cox<-perc.ci.cox
#  n<-500
#  tau=0.7
save_all_tables<-function(muhaz.hr.df.list,path.results,n,tau,ci.kernel,ci.cox){
  
  
  #####################################Things relating to Bootsrap-kernel####################################################
  #1.Bootsrap BSE
  #BSE matrix
  BSE.matrix<-lapply(ci.kernel, function(x){x[,'BSE']})
  BSE.matrix= data.frame(do.call(cbind,BSE.matrix))
  ABSE <- rowMeans(BSE.matrix, na.rm=TRUE)
  
 #2.CI coverage
  CI.matrix<-lapply(ci.kernel, function(x){x[,'CI.coverage']})
  CI.matrix= data.frame(do.call(cbind,CI.matrix))
  CI.coverage.percent=apply(CI.matrix,1,function(x){sum(x)*100/length(ci.kernel)})
  
 #3. mean of lower bound boot precentile
  X2.5.matrix<-lapply(ci.kernel, function(x){x[,'X2.5.']})
  X2.5.matrix= data.frame(do.call(cbind,X2.5.matrix))
  mean.cIL.kernel=apply(X2.5.matrix,1,function(x){mean(x,na.rm = TRUE)})
  
 #4. mean of upper bound boot  precentile 
  X97.5.matrix<-lapply(ci.kernel, function(x){x[,'X97.5.']})
  X97.5.matrix= data.frame(do.call(cbind,X97.5.matrix))
  mean.cIU.kernel=apply(X97.5.matrix,1,function(x){mean(x,na.rm = TRUE)})
  
  #4. mean of 25% boot  precentile 
  X25.matrix<-lapply(ci.kernel, function(x){x[,'X25.']})
  X25.matrix= data.frame(do.call(cbind,X25.matrix))
  mean.q1.kernel=apply(X25.matrix,1,function(x){mean(x,na.rm = TRUE)})
  
  #4. mean of 75% bound boot  precentile 
  X75.matrix<-lapply(ci.kernel, function(x){x[,'X75.']})
  X75.matrix= data.frame(do.call(cbind,X75.matrix))
  mean.q3.kernel=apply(X75.matrix,1,function(x){mean(x,na.rm = TRUE)})
  
  
  
  write.csv(CI.matrix,paste(path.results,"/CI.coverage.per.simulation",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  write.csv(X2.5.matrix,paste(path.results,"/CIL.kernel.per.simulation",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  write.csv(X97.5.matrix,paste(path.results,"/CIU.kernel.per.simulation",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  write.csv(X25.matrix,paste(path.results,"/q1.kernel.per.simulation",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  write.csv(X75.matrix,paste(path.results,"/q3.kernel.per.simulation",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  
  #####################################Things relating to Bootsrap-Cox####################################################
  #1.Bootsrap BSE
  #BSE matrix
  cox.BSE.matrix<-lapply(ci.cox, function(x){x[,'BSE']})
  cox.BSE.matrix= data.frame(do.call(cbind,cox.BSE.matrix))
  cox.ABSE <- rowMeans(cox.BSE.matrix, na.rm=TRUE)
  
  #2.CI coverage
  CI.matrix<-lapply(ci.cox, function(x){x[,'CI.coverage']})
  CI.matrix= data.frame(do.call(cbind,CI.matrix))
  cox.CI.coverage.percent=apply(CI.matrix,1,function(x){sum(x)*100/length(ci.cox)})
  
  #3. mean of lower bound boot precentile
  X2.5.matrix<-lapply(ci.cox, function(x){x[,'X2.5.']})
  X2.5.matrix= data.frame(do.call(cbind,X2.5.matrix))
  mean.cIL.cox=apply(X2.5.matrix,1,function(x){mean(x,na.rm = TRUE)})
  
  #4. mean of upper bound boot  precentile 
  X97.5.matrix<-lapply(ci.cox, function(x){x[,'X97.5.']})
  X97.5.matrix= data.frame(do.call(cbind,X97.5.matrix))
  mean.cIU.cox=apply(X97.5.matrix,1,function(x){mean(x,na.rm = TRUE)})
  
  #4. mean of 25% boot  precentile 
  X25.matrix<-lapply(ci.cox, function(x){x[,'X25.']})
  X25.matrix= data.frame(do.call(cbind,X25.matrix))
  mean.q1.cox=apply(X25.matrix,1,function(x){mean(x,na.rm = TRUE)})
  
  #4. mean of 75% bound boot  precentile 
  X75.matrix<-lapply(ci.cox, function(x){x[,'X75.']})
  X75.matrix= data.frame(do.call(cbind,X75.matrix))
  mean.q3.cox=apply(X75.matrix,1,function(x){mean(x,na.rm = TRUE)})
  
  
  
  write.csv(CI.matrix,paste(path.results,"/cox.CI.coverage.per.simulation",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  write.csv(X2.5.matrix,paste(path.results,"/CIL.cox.per.simulation",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  write.csv(X97.5.matrix,paste(path.results,"/CIU.cox.per.simulation",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  write.csv(X25.matrix,paste(path.results,"/q1.cox.per.simulation",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  write.csv(X75.matrix,paste(path.results,"/q3.cox.per.simulation",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  
  #######################################Creating the mean bias table#########################################
  ####################################Creating the statistics table####################################################
  kernel.results<-creating.statistics.bias.data(muhaz.hr.df.list,"HR")$statistics.table
  cox.results<-creating.statistics.bias.data(muhaz.hr.df.list,"HR.sp")$statistics.table
  cox.con.results<-creating.statistics.bias.data(muhaz.hr.df.list,"con.cox")$statistics.table
  #add BSE results for kernel and cox results
  kernel.results<-cbind(kernel.results,BSE.matrix)
  cox.results<-cbind(cox.results,cox.BSE.matrix)
  time<-kernel.results$time
  true.hr<-true.hr
  kernel.results$true.hr<-cox.results$true.hr<-true.hr
  
  
  
  # The raw estimatios
  all.estimations.kernel.results<-creating.statistics.bias.data(muhaz.hr.df.list,"HR")$all.muhaz.estimation
  all.estimations.cox.results<-creating.statistics.bias.data(muhaz.hr.df.list,"HR.sp")$all.muhaz.estimation
  #
  
  #The bias table
  mean.bias.table<-data.frame(time=time,mean.kernel=kernel.results$mean.HR,mean.cox=cox.results$mean.HR,
                              true.HR=cox.results$true.hr,
                              SE.kernel=kernel.results$SE.HR,SE.cox=cox.results$SE.HR,
                              mean.cox.con=cox.con.results$mean.HR,SE.cox.con=cox.con.results$SE.HR) %>% 
    mutate(bias.kernel=mean.kernel-true.HR,bias.cox=mean.cox-true.HR,
           bias.cox.con=mean.cox.con-true.HR,
           r.bias.kernel=bias.kernel*100/true.HR,r.bias.cox=bias.cox*100/true.HR,
           r.bias.cox.con=bias.cox.con*100/true.HR
           )
  
  mean.bias.table$MSE.cox<-calculate.MSE(all.estimations.cox.results,true.hr)
  mean.bias.table$MSE.kernel<-calculate.MSE(all.estimations.kernel.results,true.hr)
  mean.bias.table$ABSE<-ABSE
  mean.bias.table$CI.coverage.percent<-CI.coverage.percent
  mean.bias.table$mean.cIL.kernel<-mean.cIL.kernel
  mean.bias.table$mean.cIU.kernel<-mean.cIU.kernel
  mean.bias.table$mean.B.q1.kernel<-mean.q1.kernel
  mean.bias.table$mean.B.q3.kernel<-mean.q3.kernel
  mean.bias.table$mean.q1.kernel<-kernel.results$X25.
  mean.bias.table$mean.q3.kernel<-kernel.results$X75.
  
  mean.bias.table$cox.ABSE<-cox.ABSE
  mean.bias.table$cox.CI.coverage.percent<-cox.CI.coverage.percent
  mean.bias.table$mean.cIL.cox<-mean.cIL.cox
  mean.bias.table$mean.cIU.cox<-mean.cIU.cox
  mean.bias.table$mean.B.q1.cox<-mean.q1.cox
  mean.bias.table$mean.B.q3.cox<-mean.q3.cox
  mean.bias.table$mean.q1.cox<-cox.results$X25.
  mean.bias.table$mean.q3.cox<-cox.results$X75.
  
  
  #IQR
  mean.bias.table$IQR.simulations.cox<-cox.results$X75.-cox.results$X25.
  mean.bias.table$IQR.boot.cox<-mean.q3.cox-mean.q1.cox
  
  mean.bias.table$IQR.simulations.kernel<-kernel.results$X75.-kernel.results$X25.
  mean.bias.table$IQR.boot.kernel<-mean.q3.kernel-mean.q1.kernel
  #print("until here")
  # bias<-c(mean(mean.bias.table$bias.kernel),mean(mean.bias.table$bias.cox))
  # MSE<-c(mean(mean.bias.table$MSE.kernel),mean(mean.bias.table$MSE.cox))
  # SE<-c(mean(kernel.results$SE.HR),mean(cox.results$SE.HR))
  # RB<-c(mean(mean.bias.table$r.bias.kernel),mean(mean.bias.table$r.bias.cox))
  # #aveage bSE -need to correct
  # ABSE<-c(mean(BSE.matrix$ABSE),mean(BSE.cox.matrix$ABSE))
  # ACI.coverage<-c(mean(CI.coverage.percent),mean(cox.CI.coverage.percent))
  # 
  # IQR.average<-c(mean(mean.bias.table$IQR.simulations.kernel),mean(mean.bias.table$IQR.simulations.cox))
  # IQR.boot.average<-c(mean(mean.bias.table$IQR.boot.kernel),mean(mean.bias.table$IQR.boot.cox))
  # 
  # 
  # avrage.bias.all<-data.frame(rbind(bias,MSE,SE,RB,ABSE,ACI.coverage,IQR.average,IQR.boot.average))
  # names( avrage.bias.all)<-c("Kernel","Cox")
  # #print("until here 2")
  # #avrage.bias.without0<-data.frame(Type="without 0",avrage.bias.kernel=mean(mean.bias.table$bias.kernel[2:n.est.grid]),avrage.bias.cox=mean(mean.bias.table$bias.cox[2:n.est.grid]))  
  # #avrage.bias.all<-rbind(avrage.bias.all,avrage.bias.without0)
  # avrage.bias.all<-apply(avrage.bias.all,c(1,2) ,function(u){ formatC(u, digits = 3, format = "f")})
  # row.names(avrage.bias.all)<-c("bias","MSE","SE","RB","ABSE","ACI.coverage","EMP.IQR","Boot.IQR")
  write.csv(all.estimations.kernel.results,paste(path.results,"/all.estimations.kernel.results_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  write.csv(all.estimations.cox.results,paste(path.results,"/all.estimations.cox.results_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  
  #the satastics table 
  write.csv(kernel.results,paste(path.results,"/kernel.results_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  write.csv(cox.results,paste(path.results,"/cox.results_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  #the bias table
  write.csv(mean.bias.table,paste(path.results,"/mean.bias.table_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  #write.csv(avrage.bias.all,paste(path.results,"/avrage.bias.all_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"))
  
  
  
  }