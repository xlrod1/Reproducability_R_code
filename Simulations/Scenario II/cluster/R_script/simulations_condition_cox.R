args = commandArgs(trailingOnly = TRUE)
job_id = NULL
if(length(args)>0){
  job_id = as.numeric(args[1])
}else{
  stop(' ERROR no job id given!!!')
}

print(paste0('Job ',job_id,' started.'))
#job_id<-1
#We set simulation arguments according to job ID:
if(job_id >=1 & job_id <=72){
  sim_params_matrix = expand.grid(tau=c(0.5,0.7),exp.beta.x=c(0.1,0.5,0.9),cen.per=c(30,50,70,90),n=c(5000,10000,100000))
  tau_by_job_id = sim_params_matrix[job_id,1]
  beta.x_by_job_id = sim_params_matrix[job_id,2]
  cen.per_by_job_id = sim_params_matrix[job_id,3]
  n_by_job_id = sim_params_matrix[job_id,4]
  print(paste0('tau_by_job_id ',tau_by_job_id))
  print(paste0('beta.x_by_job_id ',beta.x_by_job_id))
}else{
  stop(' Error job id not in 1-9')
}


#########################################################################################################################
library(miceadds)
library(muhaz)
library(dplyr)
library(ggplot2)
library(simsurv)
library(survival)
library(tidyr)
library(boot)
###########################################################################################################################
#all pathes
#path.functions="/a/home/cc/stud_math/axelrod1/simulations_on_cluster/without_confounder/functions"
# path.functions="/a/home/cc/stud_math/axelrod1/simulations_on_cluster/with_confounder/functions"
# path.results="/a/home/cc/stud_math/axelrod1/simulations_on_cluster/with_confounder/SimResults/scenario2_linear_hr"
path.functions2="/a/home/cc/stud_math/axelrod1/conditional_hazard_of_Cox/functions"
file.name<-paste("tau_",tau_by_job_id,
                 "_beta.x_",beta.x_by_job_id,
                 "_cp_",cen.per_by_job_id,
                 "_n_",n_by_job_id ,sep = "")
dir.create(file.path("/a/home/cc/stud_math/axelrod1/conditional_hazard_of_Cox/results/",
                                  paste(file.name)), showWarnings = FALSE)
path.results=paste("/a/home/cc/stud_math/axelrod1/conditional_hazard_of_Cox/results/",file.name,sep="")
#results.specific=paste("tau_",tau_by_job_id,'_beta_x_',beta.x_by_job_id,"_cen_",cen.per_by_job_id,sep = "")

#path.results="/a/home/cc/stud_math/axelrod1/simulations_on_cluster/without_confounder/SimResults/scenario2_log_hr"
source.all(path.functions2)

#finde the q 

lambda.data<-read.csv("/a/home/cc/stud_math/axelrod1/conditional_hazard_of_Cox/lambda_data.csv")
lambda<-lambda.data %>% 
  filter(.,tau==tau_by_job_id ,beta.x==beta.x_by_job_id,cen.per==cen.per_by_job_id ) %>% 
  select(.,lambda)
lamdas<-as.numeric(lambda)
#set seeed
set.seed(11)
n=n_by_job_id
tau=tau_by_job_id
beta.x=log(beta.x_by_job_id)
beta.a=log(0.5)
beta.ax=log(0.5)
R=300
n.simulate=1000
parallel="yes"
ncpus=10
kern="e"
ad.c=10
end.time=ad.c
min.time=0
b.cor="b"
n.est.grid=51


start_time <- Sys.time()
condition.on.x.saving.boot(n=n,
                           tau=tau,
                           beta.x=beta.x,
                           lamdas=lamdas,
                           beta.a=beta.a,
                           beta.ax=beta.ax,
                           ad.c=ad.c,
                           path.results=path.results,
                           n.simulate=n.simulate,
                           R=R,
                           parallel=parallel,
                           ncpus=ncpus,
                           kern=kern,
                           end.time=end.time,
                           min.time=min.time,
                           b.cor=b.cor,
                           n.est.grid=n.est.grid)
  

  end_time <- Sys.time()
  print(end_time - start_time)

  
  
