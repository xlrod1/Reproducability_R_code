args = commandArgs(trailingOnly = TRUE)
job_id = NULL
if(length(args)>0){
  job_id = as.numeric(args[1])
}else{
  stop(' ERROR no job id given!!!')
}

print(paste0('Job ',job_id,' started.'))

#We set simulation arguments according to job ID:
if(job_id >=1 & job_id <=12){
  sim_params_matrix = expand.grid(n=c(500,1000,5000),tau=c(0.1,0.3,0.5,0.7))
  n_by_job_id = sim_params_matrix[job_id,1]
  tau_by_job_id = sim_params_matrix[job_id,2]
  
  print(paste0('n_by_job_id ',n_by_job_id))
  print(paste0('tau_by_job_id ',tau_by_job_id))
}else{
  stop(' Error job id not in 1-9')
}


rate.c.df<-data.frame(tau=c(0.1,0.3,0.5,0.7),rate.C=c(0.34,0.27,0.19,0.099))
rate.C=rate.c.df$rate.C[match(tau_by_job_id ,rate.c.df$tau)]

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
path.functions2="please specify the function path"
path.results="please specify the results path"


source.all(path.functions2)


#set seeed
set.seed(123)
#parametrs matrix
#sim_params_matrix = expand.grid(n=c(500),tau=c(0.1))
n= n_by_job_id
tau = tau_by_job_id
rate.C=rate.C#Or 0.15
lamdas=1
gammas=1.5
fixed.treatment.effect=log(1)
time.varying.treatment.effect=-0.4
p.a=0.5
b.cor="b"
n.simulate<-1000
n.est.grid<-51
confounder<-"no"
m.x<-0
sd.x<-1
beta.x<-3
R<-500
parallel<-"yes"
ncpus<-8
alpha<-1
beta.ax=log(1)
kern="e"
beta.a<-log(0.5)

# run the simulation all over the parametrs
print(paste0('confounder ',confounder))




start_time <- Sys.time()
const.HRC.saving.boot(lamdas=lamdas,
                        alpha=alpha,
                       gammas=gammas,
                       n =n,
                       theta=theta,
                       fixed.treatment.effect=fixed.treatment.effect,
                       rate.C=rate.C,
                       p.a=p.a,confounder=confounder,beta.ax=beta.ax,beta.a=beta.a,
                       path.results,
                       n.simulate,
                       R=R,
                       parallel=parallel,
                       ncpus=ncpus,
                       kern=kern,
                       end.time=NULL,
                      min.time=NA,
                      b.cor="b",n.est.grid=n.est.grid)
  

  end_time <- Sys.time()
  print(end_time - start_time)

