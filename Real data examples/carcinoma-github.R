set.seed(123)
########################################################################################
#load packages 
library(CausalHR)
library(survival)
library(ggplot2)
library(dplyr)
library(muhaz)
library(boot)
library(survminer)
library(KONPsurv)
#######################################################################################3
#load the data for IG distribution 
data.tau<-read.csv("C:\\Users\\xlrod\\Documents\\Phd\\kandall tau\\tau_inverse_gaussian.csv")
#load the data -carcinoma
data(carcinoma)
names(carcinoma)[3]<-"treatment"##change the name of the treatment 
carcinoma$treatment<-ifelse(carcinoma$treatment==2,0,carcinoma$treatment)


########################################Stage 1: Summary statistics and Kaplan-Meier###################################

table(carcinoma$treatment)
carcinoma %>% filter(time>=12,status==1) %>% group_by(treatment) %>% summarise(n = n())
#change the censoring
carcinoma$status<-ifelse(carcinoma$time>=12,0,carcinoma$status)
carcinoma$time<-ifelse(carcinoma$time>=12,12,carcinoma$time)
#####Now start the analysis
table(carcinoma$treatment)
table(carcinoma$treatment)*100/nrow(carcinoma)
table(carcinoma$status)
table(carcinoma$status,carcinoma$treatment)
#the censored percent
100-sum(carcinoma$status)*100/nrow(carcinoma)
#the event rate
sum(carcinoma$status)*100/nrow(carcinoma)
#stage 1: KM curves
fit <- survfit(Surv(time, status) ~ treatment, data = carcinoma)
#median follow-up time
survfit(Surv(time, status) ~ 1, data = carcinoma)
#just for comperisson
median(carcinoma$time)
p<-ggsurvplot(fit, linetype = "strata", 
              conf.int = FALSE, pval = FALSE,
              palette = "grey",
              legend.labs=c("Chemo", "Atezo"),
              legend=c(0.9, 0.9),
              legend.title = "Treatment group",
              censor=FALSE)+
  labs(title="", x ="Time (months)", y = "Estimated survival probability")
p


###################################################################################
#stage 2: Estimatoion of the Causal hazard ratio with bootsrap CI and standard errord
###################################################################################
#set a min and max time that all tau fill follow
data.control<-carcinoma%>% filter(treatment==0)%>% arrange(time)
data.treatment<-carcinoma%>% filter(treatment==1)%>% arrange(time)

sfit <- survfit(Surv(data.treatment$time, data.treatment$status) ~ 1)
min.treatment<- sfit$time[min(which(sfit$cumhaz > 0))]
sfit <- survfit(Surv(data.control$time, data.control$status) ~ 1)
min.control<- sfit$time[min(which(sfit$cumhaz > 0))]

min.time<-max(min.treatment, min.control)
end.time<-12


#####################The estimation Stage#########################################
# Global values
confounder="no"
n.est.grid=51
max.HR=5
R=500
kern="e"
b.core="b"
#Esimation under Kendalls tau=0.1
tau<-0.1


ci.g.01<-CausalHR.with.bootstrap(data=carcinoma,frailty.type=1,confounder=confounder,tau=tau,min.time=min.time,end.time=end.time,n.est.grid=n.est.grid,max.HR=max.HR,R=R,kern =kern,b.cor = b.core )
ci.ig.01<-CausalHR.with.bootstrap(data=carcinoma,frailty.type=2,confounder=confounder,tau=tau,min.time=min.time,end.time=end.time,
                                  n.est.grid=n.est.grid,max.HR=max.HR,data.tau =data.tau,R=R,kern =kern,b.cor = b.core )

ci.ker.g.01<-ci.g.01$CI.Kernel
ci.cox.g.01<-ci.g.01$CI.Cox


ci.ker.ig.01<-ci.ig.01$CI.Kernel
ci.cox.ig.01<-ci.ig.01$CI.Cox



tau<-0.3

ci.g.03<-CausalHR.with.bootstrap(data=carcinoma,frailty.type=1,confounder=confounder,tau=tau,hr.data=muhaz.hr.df.3,min.time=min.time,end.time=end.time,n.est.grid=n.est.grid,max.HR=max.HR,R=R,kern =kern,b.cor = b.core)
ci.ig.03<-CausalHR.with.bootstrap(data=carcinoma,frailty.type=2,confounder=confounder,tau=tau,hr.data=muhaz.hr.df.3.ig,min.time=min.time,end.time=end.time,n.est.grid=n.est.grid,max.HR=max.HR,data.tau =data.tau,R=R,kern =kern,b.cor = b.core )

ci.ker.g.03<-ci.g.03$CI.Kernel
ci.cox.g.03<-ci.g.03$CI.Cox


ci.ker.ig.03<-ci.ig.03$CI.Kernel
ci.cox.ig.03<-ci.ig.03$CI.Cox

tau<-0.5

ci.g.5<-CausalHR.with.bootstrap(data=carcinoma,frailty.type=1,confounder=confounder,tau=tau,hr.data=muhaz.hr.df.5,min.time=min.time,end.time=end.time,n.est.grid=n.est.grid,max.HR=max.HR,R=R,kern =kern,b.cor = b.core)
ci.ig.5<-CausalHR.with.bootstrap(data=carcinoma,frailty.type=2,confounder=confounder,tau=tau,hr.data=muhaz.hr.df.5.ig,min.time=min.time,end.time=end.time,n.est.grid=n.est.grid,max.HR=max.HR,data.tau =data.tau,R=R,kern =kern,b.cor = b.core )


ci.ker.g.5<-ci.g.5$CI.Kernel
ci.cox.g.5<-ci.g.5$CI.Cox


ci.ker.ig.5<-ci.ig.5$CI.Kernel
ci.cox.ig.5<-ci.ig.5$CI.Cox



tau<-0.7

ci.g.7<-CausalHR.with.bootstrap(data=carcinoma,frailty.type=1,confounder=confounder,tau=tau,hr.data=muhaz.hr.df.7,min.time=min.time,end.time=end.time,n.est.grid=n.est.grid,max.HR=max.HR,R=R,kern =kern,b.cor = b.core)


ci.ker.g.7<-ci.g.7$CI.Kernel
ci.cox.g.7<-ci.g.7$CI.Cox

tau<-0.9

ci.g.9<-CausalHR.with.bootstrap(data=carcinoma,frailty.type=1,confounder=confounder,tau=tau,hr.data=muhaz.hr.df.9,min.time=min.time,end.time=end.time,n.est.grid=n.est.grid,max.HR=max.HR,R=R,kern =kern,b.cor = b.core)


ci.ker.g.9<-ci.g.9$CI.Kernel
ci.cox.g.9<-ci.g.9$CI.Cox

ci.kernel<-rbind(ci.ker.g.01,ci.ker.ig.01,ci.ker.g.03,ci.ker.ig.03,ci.ker.g.5,ci.ker.ig.5,ci.ker.g.7,ci.ker.g.9)
names(ci.kernel)[1:2]<-c('CIL','CIU')

ci.cox<-rbind(ci.cox.g.01,ci.cox.ig.01,ci.cox.g.03,ci.cox.ig.03,ci.cox.g.5,ci.cox.ig.5,ci.cox.g.7,ci.cox.g.9)
names(ci.cox)[1:2]<-c('CIL','CIU')



############################################################
#make only one plot form the two estimation method
library(latex2exp)
all_ci_carcinoma<-rbind(ci.kernel,ci.cox)
y.lim=5
all_ci_carcinoma$CIU<-ifelse(all_ci_carcinoma$CIU>=y.lim,y.lim,all_ci_carcinoma$CIU)

time.df<-data.frame(time=unique(all_ci_carcinoma$time),n.time=c(1:51),w.time=c(rep(c(1,0,0,0,0),10),1))
all_ci_carcinoma<-all_ci_carcinoma %>% left_join(time.df)
all_ci_carcinoma$tau.number<-all_ci_carcinoma$tau
all_ci_carcinoma$tau<-as.factor(all_ci_carcinoma$tau)
levels(all_ci_carcinoma$tau) <- c("0.1" = TeX("$\\tau==0.1$"),
                                  "0.3" = TeX("$\\tau==0.3$"),
                                  "0.5" = TeX("$\\tau==0.5$"),
                                  "0.7" = TeX("$\\tau==0.7$"),
                                  "0.9" = TeX("$\\tau==0.9$"))



##################################Only kernel 
all_ci_carcinoma %>% filter(w.time==1) %>% 
  filter(tau.number!=0.9) %>% 
  filter(method=="Kernel") %>% 
  ggplot(aes(x = time,y=HR)) + 
  geom_point(size=4)+
  geom_line() + 
  #geom_line(aes(linetype=dist)) + 
  geom_errorbar(aes(ymin = HR-BSE, ymax = HR+BSE),alpha=0.5)+
  scale_fill_grey()+
  scale_colour_grey()+
  geom_hline(yintercept = 1,linetype="dashed")+
  labs(title="", x ="Time (months)", y = "Causal HR(t)")+
  theme_bw()+
  theme(panel.background = element_rect(colour = "black"),
        legend.position ="top",legend.background=element_blank(),
        axis.text = element_text( size = 20 ),
        axis.text.x = element_text( size = 20 ),
        axis.title = element_text( size = 20, face = "bold" ),
        strip.text = element_text(size = 20),
        legend.title = element_text( size = 20 ),
        legend.text =element_text( size = 20 ) ,
        panel.spacing.x = unit(1.2, "lines"))+
  facet_grid(rows=vars(dist),
             cols=vars(tau),
             labeller=labeller(tau=label_parsed))+
  scale_x_continuous(breaks = pretty(all_ci_carcinoma$time, n = 5),limits = c(0,12.6))+
  guides(fill=guide_legend("Frailty distribution"),linetype=guide_legend("Frailty distribution"))+
  ylim(-0.08,4)

# Only Cox
all_ci_carcinoma %>% filter(w.time==1) %>% 
  filter(tau.number!=0.9) %>% 
  filter(method=="Cox") %>% 
  ggplot(aes(x = time,y=HR)) + 
  geom_point(size=4)+
  geom_line() + 
  #geom_line(aes(linetype=dist)) + 
  geom_errorbar(aes(ymin = CIL, ymax = CIU),alpha=0.5)+
  scale_fill_grey()+
  scale_colour_grey()+
  geom_hline(yintercept = 1,linetype="dashed")+
  labs(title="", x ="Time (months)", y = "Causal HR(t)")+
  theme_bw()+
  theme(panel.background = element_rect(colour = "black"),
        legend.position ="top",legend.background=element_blank(),
        axis.text = element_text( size = 20 ),
        axis.text.x = element_text( size = 20 ),
        axis.title = element_text( size = 20, face = "bold" ),
        strip.text = element_text(size = 20),
        legend.title = element_text( size = 20 ),
        legend.text =element_text( size = 20 ) ,
        panel.spacing.x = unit(1.2, "lines"))+
  facet_grid(rows=vars(dist),
             cols=vars(tau),
             labeller=labeller(tau=label_parsed))+
  scale_x_continuous(breaks = pretty(all_ci_carcinoma$time, n = 5),limits = c(0,12.6))+
  guides(fill=guide_legend("Frailty distribution"),linetype=guide_legend("Frailty distribution"))+
  ylim(0,1.8)



