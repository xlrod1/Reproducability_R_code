set.seed(123)
########################################################################################
#load packages 
library(RISCA)
library(cobalt)
library(latex2exp)
library(CausalHR)
library(survival)
library(ggplot2)
library(dplyr)
library(muhaz)
library(boot)
library(survminer)
library(nph)
#######################################################################################3
#load the data for IG distribution 
data.tau<-read.csv("C:\\Users\\xlrod\\Documents\\Phd\\kandall tau\\tau_inverse_gaussian.csv")


#load the data -DIVAT
data(dataDIVAT2)
DIVAT<-dataDIVAT2
names(DIVAT)[c(4,5,6)]<-c('treatment','time','status')
DIVAT<-filter(DIVAT,age>=40,age<=70)
#########################################Descriptive statistics##########################################
table(DIVAT$treatment)
table(DIVAT$age,DIVAT$treatment)
table(DIVAT$status)
table(DIVAT$treatment)
table(DIVAT$treatment)*100/nrow(DIVAT)
sum(DIVAT$status==1)*100/nrow(DIVAT)
#the censored percent
100-sum(DIVAT$status)*100/nrow(DIVAT)
########################################Weights calculations###############################################
fit<-glm(treatment~age+hla+ retransplant, data=DIVAT,
         family=binomial(link = "logit"))
Pr1 <- fit$fitted.values
P.A<-sum(DIVAT$treatment==1)/nrow(DIVAT)
DIVAT$W <- (DIVAT$treatment==1) *P.A*(1/Pr1) + (DIVAT$treatment==0) *(1-P.A)*(1)/(1-Pr1)
#correction that I have made to the weights
quntile.99.W<-quantile(DIVAT$W,0.99)
DIVAT$W.old<-DIVAT$W
DIVAT$W<-ifelse(DIVAT$W>quntile.99.W,quntile.99.W,DIVAT$W)
#Summary statistics regrading the weights
tbl.logistic<-round(cbind(exp(cbind(OR = coef(fit), confint(fit))),p.v=coef(summary(fit))[,4]),2)
tbl.logistic
#write.csv(tbl.logistic,"logistic_regression_results.csv")
ggplot(data=DIVAT)+
    geom_histogram(aes(x=W),bins=10)+
  labs(title="", x ="Weights", y = "Counts")+
  theme_bw()+
  theme(panel.background = element_rect(colour = "black"),
        legend.position ="top",legend.background=element_blank(),
        axis.text = element_text( size = 20 ),
        axis.text.x = element_text( size = 15 ),
        axis.title = element_text( size = 20, face = "bold" ),
        strip.text = element_text(size = 20),
        legend.title = element_text( size = 20 ),legend.text =element_text( size = 20 ) )+
  scale_x_continuous(breaks = pretty(c(0,6), n = 5))
#ggsave("weights histogram.png",height = 10,width = 15)
covs <- subset(DIVAT, select = c(age,hla,retransplant))
balance.table<-bal.tab(treatment~covs, data = DIVAT, weights = DIVAT$W,
                       disp.means = TRUE,un = TRUE,s.d.denom="weighted",
                       col_w_smd=TRUE)
balance.table
#write.csv(balance.table[[1]],"balance_table_results.csv")
bal.plot(treatment~covs, data = DIVAT, weights = DIVAT$W, "retransplant", which = "both")
#ggsave("retransplant-balance.png",height = 10,width = 15)
bal.plot(treatment~covs, data = DIVAT, weights = DIVAT$W, "age", which = "both")
#ggsave("age-balance.png",height = 10,width = 15)
bal.plot(treatment~covs, data = DIVAT, weights = DIVAT$W, "hla", which = "both")
#ggsave("hla-balance.png",height = 10,width = 15)
###################################First stage-KM########################################

#fit the surfilt with the truncated weights
fit.W <- survfit(Surv(time, status) ~ treatment, data =DIVAT,weights = W)
#log rank test
survdiff(Surv(time, status) ~ treatment, data =DIVAT)
logrank.test(
  DIVAT$time,
  DIVAT$status,
  DIVAT$treatment,
  alternative ="two.sided",
  rho = 0,
  gamma = 0,
  weights =DIVAT$W
)

#different type of plot-weighted and non weighted curves on the same plot 
fit.n.W <- survfit(Surv(time, status) ~ treatment, data =DIVAT)
list.fit<-list(NW=fit.n.W,W=fit.W)

ggsurvplot_combine(
  list.fit,data=DIVAT, pval = FALSE,
  #palette = "grey",
  palette = c("grey4","gray65","grey5","gray66"),
  legend.labs=c("NW: SCD donors","NW: ECD donors","W: SCD donors","W: ECD donors"),
  legend=c(0.9, 0.9),
  legend.title = "Donors type",
  censor=FALSE,
  #color="treatment",
  linetype=c(2,2,1,1),
  tables.col = "strata")+
  labs(title="", x ="Time (years)", y = "Survival probability (%)")

#ggsave("DIVAT_KP_combine_W_NW.png",width = 8)

##########################################################################################
################################Standard(Cox) analysis##################################

#stage 2 : weighted cox regression
r.cox<-coxph(Surv(time, status) ~ treatment, data =DIVAT,weights = W)
tbl.cox<-round(cbind(exp(cbind(HR = coef(r.cox), confint(r.cox))),
                     p.v=coef(summary(r.cox))[,6]),2)
tbl.cox
#write.csv(tbl.con.cox,"tbl_con_cox.csv")
#test PH assumption
cox.zph(r.cox)


#stage 3 : conditional cox regression
r.con.cox<-coxph(Surv(time, status) ~ treatment+age+hla+ retransplant, data =DIVAT)
tbl.con.cox<-round(cbind(exp(cbind(HR = coef(r.con.cox), confint(r.con.cox))),
                         p.v=coef(summary(r.con.cox))[,5]),5)
tbl.con.cox
#write.csv(tbl.con.cox,"tbl_con_cox.csv")

#PH assumption -conditional cox 
cox.zph(r.cox)

###########################################################################################
##Cox based and kernel-based estimation  ###############################################
##########################################################################################
#set a min and max time that all tau fill follow
data.control<-DIVAT%>% filter(treatment==0)%>% arrange(time)
data.treatment<-DIVAT%>% filter(treatment==1)%>% arrange(time)

sfit <- survfit(Surv(data.treatment$time, data.treatment$status) ~ 1,weights = data.treatment$W)
end.treatment <- approx(sfit$n.risk, sfit$time, xout = 10)$y
min.treatment<- sfit$time[min(which(sfit$cumhaz > 0))]


sfit <- survfit(Surv(data.control$time, data.control$status) ~ 1,weights = data.control$W)
end.control <- approx(sfit$n.risk, sfit$time, xout = 10)$y
min.control<- sfit$time[min(which(sfit$cumhaz > 0))]


end.time<-min(end.treatment ,end.control)
min.time<-max(min.treatment, min.control)

# Global values
confounder="no"
n.est.grid=51
max.HR=5
R=500
kern="e"
b.core="b"
data=DIVAT

#Esimation under Kendalls tau=0.1
tau<-0.1


ci.g.01<-CausalHR.with.bootstrap(data=data,frailty.type=1,confounder=confounder,tau=tau,min.time=min.time,end.time=end.time,n.est.grid=n.est.grid,max.HR=max.HR,R=R,kern =kern,b.cor = b.core )
ci.ig.01<-CausalHR.with.bootstrap(data=data,frailty.type=2,confounder=confounder,tau=tau,min.time=min.time,end.time=end.time,
                                  n.est.grid=n.est.grid,max.HR=max.HR,data.tau =data.tau,R=R,kern =kern,b.cor = b.core )

ci.ker.g.01<-ci.g.01$CI.Kernel
ci.cox.g.01<-ci.g.01$CI.Cox


ci.ker.ig.01<-ci.ig.01$CI.Kernel
ci.cox.ig.01<-ci.ig.01$CI.Cox



tau<-0.3

ci.g.03<-CausalHR.with.bootstrap(data=data,frailty.type=1,confounder=confounder,tau=tau,hr.data=muhaz.hr.df.3,min.time=min.time,end.time=end.time,n.est.grid=n.est.grid,max.HR=max.HR,R=R,kern =kern,b.cor = b.core)
ci.ig.03<-CausalHR.with.bootstrap(data=data,frailty.type=2,confounder=confounder,tau=tau,hr.data=muhaz.hr.df.3.ig,min.time=min.time,end.time=end.time,n.est.grid=n.est.grid,max.HR=max.HR,data.tau =data.tau,R=R,kern =kern,b.cor = b.core )

ci.ker.g.03<-ci.g.03$CI.Kernel
ci.cox.g.03<-ci.g.03$CI.Cox


ci.ker.ig.03<-ci.ig.03$CI.Kernel
ci.cox.ig.03<-ci.ig.03$CI.Cox

tau<-0.5

ci.g.5<-CausalHR.with.bootstrap(data=data,frailty.type=1,confounder=confounder,tau=tau,hr.data=muhaz.hr.df.5,min.time=min.time,end.time=end.time,n.est.grid=n.est.grid,max.HR=max.HR,R=R,kern =kern,b.cor = b.core)
ci.ig.5<-CausalHR.with.bootstrap(data=data,frailty.type=2,confounder=confounder,tau=tau,hr.data=muhaz.hr.df.5.ig,min.time=min.time,end.time=end.time,n.est.grid=n.est.grid,max.HR=max.HR,data.tau =data.tau,R=R,kern =kern,b.cor = b.core )


ci.ker.g.5<-ci.g.5$CI.Kernel
ci.cox.g.5<-ci.g.5$CI.Cox


ci.ker.ig.5<-ci.ig.5$CI.Kernel
ci.cox.ig.5<-ci.ig.5$CI.Cox



tau<-0.7

ci.g.7<-CausalHR.with.bootstrap(data=data,frailty.type=1,confounder=confounder,tau=tau,hr.data=muhaz.hr.df.7,min.time=min.time,end.time=end.time,n.est.grid=n.est.grid,max.HR=max.HR,R=R,kern =kern,b.cor = b.core)


ci.ker.g.7<-ci.g.7$CI.Kernel
ci.cox.g.7<-ci.g.7$CI.Cox

tau<-0.9

ci.g.9<-CausalHR.with.bootstrap(data=data,frailty.type=1,confounder=confounder,tau=tau,hr.data=muhaz.hr.df.9,min.time=min.time,end.time=end.time,n.est.grid=n.est.grid,max.HR=max.HR,R=R,kern =kern,b.cor = b.core)


ci.ker.g.9<-ci.g.9$CI.Kernel
ci.cox.g.9<-ci.g.9$CI.Cox

ci.kernel<-rbind(ci.ker.g.01,ci.ker.ig.01,ci.ker.g.03,ci.ker.ig.03,ci.ker.g.5,ci.ker.ig.5,ci.ker.g.7,ci.ker.g.9)
names(ci.kernel)[1:2]<-c('CIL','CIU')

ci.cox<-rbind(ci.cox.g.01,ci.cox.ig.01,ci.cox.g.03,ci.cox.ig.03,ci.cox.g.5,ci.cox.ig.5,ci.cox.g.7,ci.cox.g.9)
names(ci.cox)[1:2]<-c('CIL','CIU')





#################################Standart analysis#########################################
############################################################
#make only one plot form the two estimation method



all_ci_divat<-rbind(ci.kernel,ci.cox)
time.df<-data.frame(time=unique(all_ci_divat$time),n.time=c(1:51),w.time=c(rep(c(1,0,0,0),12),0,0,0))
all_ci_divat<-all_ci_divat %>% left_join(time.df)
all_ci_divat$tau.number<-all_ci_divat$tau

all_ci_divat$row<-paste(all_ci_divat$method,", ",all_ci_divat$dist,sep = "")

all_ci_divat<-all_ci_divat %>%
  arrange(row) %>%
  mutate(n.name = factor(row, levels=c("Cox, Gamma", "Kernel, Gamma", "Cox, IG", "Kernel, IG")))

y.lim=10
all_ci_divat$CIU<-ifelse(all_ci_divat$CIU>=y.lim,y.lim,all_ci_divat$CIU)
all_ci_divat$tau<-as.factor(all_ci_divat$tau)
levels(all_ci_divat$tau) <- c("0.1" = TeX("$\\tau==0.1$"), "0.3" = TeX("$\\tau==0.3$"),
                                  "0.5" = TeX("$\\tau==0.5$"),
                                   "0.7" = TeX("$\\tau==0.7$"),
                                   "0.9" = TeX("$\\tau==0.9$"))



##################################Only Gamma
all_ci_divat %>% filter(w.time==1) %>% 
  filter(tau.number!=0.9) %>% 
  filter(dist=="Gamma") %>% 
  ggplot(aes(x = time,y=HR)) + 
  geom_point(size=4)+
  geom_line() + 
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
  facet_grid( rows=vars(method),
              cols=vars(tau),
             labeller=labeller(tau=label_parsed))+
  ylim(0,10)+
  scale_x_continuous(breaks = pretty(all_ci_divat$time, n = 10),limits = c(-0.6,10.8))



all_ci_divat %>% filter(w.time==1) %>% 
  filter(tau.number!=0.9) %>% 
  filter(dist=="IG") %>% 
  ggplot(aes(x = time,y=HR)) + 
  geom_point(size=4)+
  geom_line() + 
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
  facet_grid(rows=vars(method),
    cols=vars(tau),
    labeller=labeller(tau=label_parsed))+
  #guides(shape=guide_legend("Frailty distribution"))+
  ylim(0,10)+
  scale_x_continuous(breaks = pretty(all_ci_divat$time, n = 10),limits = c(-0.6,10.8))





