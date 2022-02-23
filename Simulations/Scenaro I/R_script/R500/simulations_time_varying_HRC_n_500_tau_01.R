

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
path.functions2="/a/home/cc/stud_math/axelrod1/simulations_on_cluster/New_only_two_scenarios/functions2"
path.results="/a/home/cc/stud_math/axelrod1/simulations_on_cluster/New_only_two_scenarios/without_confounder/SimResults/scenario1_time_varying_HRC/special"

#path.results="/a/home/cc/stud_math/axelrod1/simulations_on_cluster/without_confounder/SimResults/scenario2_log_hr"
source.all(path.functions2)


#set seeed
set.seed(123)
#parametrs matrix
#sim_params_matrix = expand.grid(n=c(500),tau=c(0.1))
n= 500
tau = 0.1
rate.C=0.18#Or 0.15
lamdas=0.1
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
R<-1000
parallel<-"yes"
ncpus<-20
alpha<-1
beta.ax=log(1)
kern="e"
beta.a<-log(0.5)
# run the simulation all over the parametrs===
print(paste0('confounder ',confounder))




start_time <- Sys.time()
time.varying.HRC.saving.boot(lamdas=lamdas,alpha=alpha,
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
                       b.cor=b.cor)
  
  
  end_time <- Sys.time()
  print(end_time - start_time)

