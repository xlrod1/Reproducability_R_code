
summary.statistics.boot<-function(results.boot,true.hr,sn,i){
  
  #1.compute the BSE and the perc.ci
  BSE<-apply(results.boot, 2, function(u){sqrt(var(u,na.rm = TRUE))})
  perc.ci2<-data.frame(t(apply(results.boot, 2, function(u){quantile(u, probs = c(.025, .975,0.25,0.75,0.5,0.1), type = 6,na.rm = TRUE)})))
  
 
  #BSE matrix

  
  #CI.COVERAGE
  perc.ci2<-data.frame(perc.ci2,true.hr=rep(true.hr,2),CI.coverage=rep(0,2*length(true.hr)))
    for( j in 1:nrow(perc.ci2)){
      if(!is.na(perc.ci2[j,1])&&!is.na(perc.ci2[j,2])&&perc.ci2$true.hr[j]>=perc.ci2[j,1]&&perc.ci2$true.hr[j]<=perc.ci2[j,2]){
        perc.ci2$CI.coverage[j]<-1
      }
      if(is.na(perc.ci2[j,1])||is.na(perc.ci2[j,2])){perc.ci2$CI.coverage[j]=NA}
    }

 
  
  perc.ci2$BSE<-BSE

 
  path.results.boot=paste(path.results,"/bootsrap_results/",sep="")
  write.csv(perc.ci2,paste(path.results.boot,"/boot_results_rep",sn,"_",i,"_n_",n,'_tau_',tau,"_",Sys.Date(),sep = "",".csv"),row.names = FALSE)
  
  
}