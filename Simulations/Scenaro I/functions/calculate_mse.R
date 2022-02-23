

calculate.MSE<-function(data,true.hr){
 data<-apply(data,c(1,2),function(x){ifelse(x%in%c(Inf,-Inf),NA,x)})
  
  data_t<-data.frame(t(data))[2:ncol(data),]
  
  mse<-c()
  for( j in 1:ncol(data_t)){
    data_t_1<-data_t[,j]
    sum<-0
    k=1
    for( i in (1:length(data_t_1))){
      if(!is.na(data_t_1[i])){
        sum<-sum+(data_t_1[i]-true.hr[j])^2
        k=k+1
      }
      else{sum<-sum+0}
    }
    mse[j]<-sum/k
  }
  
  mse
}
