library(latex2exp)
#load packages 
library(survival)
library(ggplot2)
library(dplyr)
library(plotly)
library(statmod)
library(miceadds)


results="C:\\Users\\xlrod\\Documents\\Phd\\R code\\conditional hazard of Cox\\cluster\\results\\n50000\\"

#bias.table.function
bias.table<-function(tau,beta.x,cp,n,minus=FALSE){
  if(minus==TRUE){
    name.job<-paste("tau_",tau,
                    "minus_beta.x_",beta.x,
                    "_cp_",cp,
                    "_n_",n ,sep = "")
  }
  if(minus==FALSE){
    name.job<-paste("tau_",tau,
                    "_beta.x_",beta.x,
                    "_cp_",cp,
                    "_n_",n ,sep = "")
  }
  
  results.job=paste(results,name.job,sep = "")
  parametr.file.name<-list.files(path=results.job,pattern='data.of.parameters')
  bias.file.name<-list.files(path=results.job,pattern='mean.bias.table')
  
  
  data.of.parameters<- read.csv(paste(results.job,"\\",parametr.file.name,sep = ""))
  data.of.bias<- read.csv(paste(results.job,"\\",bias.file.name,sep = ""))
  tau=as.numeric(data.of.parameters$value[match("tau",data.of.parameters$parameter)])
  cen.precent=round(as.numeric(data.of.parameters$value[match("cen.per",data.of.parameters$parameter)]),digits = 2)
  beta.x=round(as.numeric(data.of.parameters$value[match("beta.x",data.of.parameters$parameter)]),digits = 2)
  
  bias.table<-data.of.bias %>% 
    select(time,r.bias.cox,r.bias.kernel,r.bias.cox.con,
           mean.kernel,mean.cox,true.HR,
           #mean.cIL.cox,mean.cIU.cox,mean.cIL.kernel,mean.cIU.kernel
           ) %>% 
    mutate(tau=tau,cen.precent=cen.precent,beta.x=beta.x)
  
return(bias.table)  
  
}
#xx<-bias.table(tau=0.5,beta.x=0.1,cp=30,n=5000)
# = expand.grid(tau=c(0.5,0.7),exp.beta.x=c(0.1,0.5,0.9),cen.per=c(30,50,70,90),n=c(5000))
sim_params_matrix = expand.grid(tau=c(0.7),exp.beta.x=c(0.1,0.5,0.9),cen.per=c(70,90,95,97,99),n=c(50000))

#all bias tables 
#i=1
bias.table.list<-list()
for( i in 1:nrow(sim_params_matrix)){
 tau<-sim_params_matrix[i,"tau"]
 beta.x<-sim_params_matrix[i,"exp.beta.x"]
 cp<-sim_params_matrix[i,"cen.per"]
 n<-sim_params_matrix[i,"n"]
  try(bias.table.list[[i]]<-bias.table(tau=tau,beta.x=beta.x,cp=cp,n=n,minus = FALSE))
  #bias.table.list[[i]]<-bias.table(tau=tau,beta.x=beta.x,cp=cp,n=n)
  print(i)
}
# bias.table.list.minus<-list()
# for( i in 1:nrow(sim_params_matrix)){
#   tau<-sim_params_matrix[i,"tau"]
#   beta.x<-sim_params_matrix[i,"exp.beta.x"]
#   cp<-sim_params_matrix[i,"cen.per"]
#   n<-sim_params_matrix[i,"n"]
#   try(bias.table.list.minus[[i]]<-bias.table(tau=tau,beta.x=beta.x,cp=cp,n=n,minus = TRUE))
#   #bias.table.list[[i]]<-bias.table(tau=tau,beta.x=beta.x,cp=cp,n=n)
#   print(i)
# }
bias.table.long<-do.call(rbind,bias.table.list)
#bias.table.long.minus<-do.call(rbind,bias.table.list.minus)
bias.table.long$cen.precent<-ifelse(bias.table.long$cen.precent==0.91,0.9,bias.table.long$cen.precent)
bias.table.long$cen.precent<-ifelse(bias.table.long$cen.precent==0.71,0.7,bias.table.long$cen.precent)
#unique(bias.table.long.total$cen.precent)


bias.table.long.total<-rbind(bias.table.long)
#Lables of plot
bias.table.long.total$event.precent<-as.factor(1-bias.table.long.total$cen.precent)
bias.table.long.total$cen.precent<-as.factor(bias.table.long.total$cen.precent)

#unique(bias.table.long.total$event.precent)

# levels(bias.table.long$cen.precent) <- c("0.90" = TeX("$CR=90%$"),
#                                          "0.95" = TeX("$CR=95%$"),
#                                         "0.97" = TeX("$CR=97%$"),
#                                         "0.99" = TeX("$CR=99%$"))
levels(bias.table.long.total$cen.precent) <- c("0.70" = TeX("$CR=70%$"),
                                        "0.90" = TeX("$CR=90%$"),
                                        "0.95" = TeX("$CR=95%$"),
                                        "0.97" = TeX("$CR=97%$"),
                                         "0.99" = TeX("$CR=99%$"))
levels(bias.table.long.total$event.precent) <- c("0.30" = TeX("$ER=30%$"),
                                               "0.10" = TeX("$ER=10%$"),
                                               "0.05" = TeX("$ER=5%$"),
                                               "0.03" = TeX("$ER=3%$"),
                                               "0.01" = TeX("$ER=1%$"))
bias.table.long.total$beta.x<-as.factor(bias.table.long.total$beta.x)
levels(bias.table.long.total$beta.x) <- c("-2.3" = TeX("$\\beta_z=log(0.1)$"),
                                    "-0.69" = TeX("$\\beta_z=log(0.5)$"),
                                    "-0.11" = TeX("$\\beta_z=log(0.9)$"))

#make plot


bias.table.long.total %>% filter(tau==0.7) %>% 
  ggplot(aes(x=time,y=r.bias.cox))+geom_line(size=1.2,aes(linetype="Cox"))+
  geom_line(aes(x=time,y=r.bias.cox.con,linetype="Conditional Cox"),size=1.2)+
  geom_line(aes(x=time,y=r.bias.kernel,linetype="Kernel"),size=1.2)+
 labs(title="", x ="Time", y = "Relative bias(%)")+ theme_bw()+
  theme_bw()+
  theme(panel.background = element_rect(colour = "black"),
        legend.background=element_blank(),
        axis.text = element_text( size = 20 ),
        axis.text.x = element_text( size = 15 ),
        axis.title = element_text( size = 20, face = "bold" ),
        strip.text = element_text(size = 20),
        legend.text =element_text( size = 20 ),
        legend.title =element_text( size = 20 ),
        legend.position="bottom",
        legend.key.width = unit(3, "line"))+
 geom_hline(yintercept=0,linetype = "dashed")+
    facet_grid(as.factor(beta.x)~as.factor(event.precent),labeller=label_parsed)+
  labs(linetype= "")+
  scale_linetype_manual(values=c("longdash","dotted", "solid"),name="Estimation method")+
  ylim(-20,60)

ggsave("C:\\Users\\xlrod\\Documents\\Phd\\R code\\conditional hazard of Cox\\cluster\\table_results_n50000_with70_event_rate.png",height = 10,width = 15)


############################################################################################
###################################Real bias nad not bias

#plot with means+true+CI
bias.table.long %>% filter(tau==0.7) %>%
  ggplot(aes(x=time,y=mean.cox,color="Cox"))+geom_line(size=1.5)+
  geom_ribbon(aes(ymin = mean.cIL.cox, ymax = mean.cIU.cox), alpha = 0.2,color="gray77")+
  geom_line(aes(x=time,y=mean.kernel,color="kernel"),size=1.5)+
  geom_ribbon(aes(ymin = mean.cIL.kernel, ymax = mean.cIU.kernel), alpha = 0.2,color="gray48")+
  geom_line(aes(x=time,y=true.HR),size=1,linetype = "dashed",color="black")+
  labs(title="", x ="Time (years)", y = "Causal HR(t)")+ theme_bw()+
  #theme(panel.background = element_rect(colour = "black"),text = element_text(size=10),legend.position="bottom" )+
  scale_color_manual("Estimatiom method",values=c("gray77","gray48"))+
  theme_bw()+
  theme(panel.background = element_rect(colour = "black"),
        legend.background=element_blank(),
        axis.text = element_text( size = 20 ),
        axis.text.x = element_text( size = 20 ),
        axis.title = element_text( size = 20, face = "bold" ),
        strip.text = element_text(size = 20),
        legend.title = element_text( size = 20 ),legend.text =element_text( size = 20 ),
        legend.position="bottom")+
  # geom_hline(yintercept=0,linetype = "dashed")+
  # facet_grid(.~as.factor(sceario))+
  facet_wrap(as.factor(beta.x)~as.factor(cen.precent),scales = "free",nrow = 3,labeller=label_parsed)+
  ylim(0,2)+
  xlim(0,10)
  
ggsave("C:\\Users\\xlrod\\Documents\\Phd\\R code\\conditional hazard of Cox\\cluster\\table_results_n50000_CI.png",height = 10,width = 15)

####################################################With points################################
plot_with_points<-bias.table.long.total%>% 
  select(time,r.bias.cox,r.bias.kernel,r.bias.cox.con,tau,event.precent,beta.x) %>% 
  gather(., method.bias, r.bias, r.bias.cox:r.bias.cox.con,factor_key=TRUE) 



plot_with_points%>% filter(tau==0.7) %>% 
  ggplot(aes(x=time,y=r.bias,shape=method.bias))+
  geom_point(size=3.5)+
  geom_line()+
  labs(title="", x ="Time", y = "Relative bias(%)")+ theme_bw()+
  theme_bw()+
  theme(panel.background = element_rect(colour = "black"),
        legend.background=element_blank(),
        axis.text = element_text( size = 20 ),
        axis.text.x = element_text( size = 15 ),
        axis.title = element_text( size = 20, face = "bold" ),
        strip.text = element_text(size = 20),
        legend.text =element_text( size = 20 ),
        legend.title =element_text( size = 20 ),
        legend.position="bottom",
        legend.key.width = unit(3, "line"))+
  geom_hline(yintercept=0,linetype = "dashed")+
  facet_grid(as.factor(beta.x)~as.factor(event.precent),labeller=label_parsed)+
  labs(linetype= "")+
  scale_linetype_manual(values=c("longdash","dotted", "solid"),name="Estimation method")+
  ylim(-20,60)

ggsave("C:\\Users\\xlrod\\Documents\\Phd\\R code\\conditional hazard of Cox\\cluster\\table_results_n50000_with70.png",height = 10,width = 15)
