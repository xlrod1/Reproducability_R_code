plot_all_results_3<-function(scenario_IA,name,ylim1=NA,ylim2=NA,ylim3=NA,x.lim=NA){
  
  # plot_m_scenario_IA<-scenario_IA%>% 
  #   select(time,true.HR,mean.cox,mean.kernel,mean.cIL.kernel, mean.cIL.cox,mean.cIU.kernel, mean.cIU.cox) %>% 
  #   gather(., method.mean, mean, mean.cox:mean.kernel,factor_key=TRUE) %>% 
  #   gather(., method.CIL, CIL ,mean.cIL.kernel:mean.cIL.cox,factor_key=TRUE) %>% 
  #   gather(., method.CIU, CIU ,mean.cIU.kernel:mean.cIU.cox,factor_key=TRUE) %>% 
  #   filter((method.mean=="mean.kernel"&method.CIL=="mean.cIL.kernel"&method.CIU=="mean.cIU.kernel")|
  #            (method.mean=="mean.cox"&method.CIL=="mean.cIL.cox"&method.CIU=="mean.cIU.cox")) %>% 
  #   mutate(Sceario=name,method=ifelse(method.mean=="mean.kernel","Kernel","Cox")) %>% 
  #   mutate(CIU=ifelse(CIU>=ylim1,ylim1,CIU))
  plot_m_scenario_IA<-scenario_IA%>%
    select(time,true.HR,mean.cox,mean.kernel,SE.kernel, SE.cox) %>%
    gather(., method.mean, mean, mean.cox:mean.kernel,factor_key=TRUE) %>%
    gather(., method.SD, SD ,SE.kernel:SE.cox,factor_key=TRUE) %>%
   # gather(., method.CIU, CIU ,mean.cIU.kernel:mean.cIU.cox,factor_key=TRUE) %>%
    filter((method.mean=="mean.kernel"&method.SD=="SE.kernel")|
             (method.mean=="mean.cox"&method.SD=="SE.cox")) %>%
    mutate(Sceario=name,method=ifelse(method.mean=="mean.kernel","Kernel","Cox")) %>%
    mutate(SD=ifelse(SD>=ylim1,ylim1,SD))

  p1a<-plot_m_scenario_IA %>% 
    filter(method=="Cox") %>% 
    ggplot(aes(x=time,y=mean))+
    geom_line()+
    geom_errorbar(aes(ymin = mean-SD, ymax =mean+SD),alpha=0.5)+
    geom_line(aes(x=time,y=true.HR),size=2,linetype = "dashed",color="black")+
    labs(title="", x ="Time", y = TeX("$HR_{Cox}^C(t)$"))+
    theme_bw()+
    theme(panel.background = element_rect(colour = "black"),
          legend.background=element_blank(),
          axis.text = element_text( size = 20 ),
          axis.text.x = element_text( size = 20 ),
          axis.title = element_text( size = 20, face = "bold" ),
          strip.text = element_text(size = 20),
          panel.spacing = unit(4.5, "lines"))
  
  if(!is.na(ylim1)){
    p1a=p1a+  ylim(0,ylim1)
  }
  
  
  p2a<-plot_m_scenario_IA %>% 
    filter(method=="Kernel") %>% 
    ggplot(aes(x=time,y=mean))+
    geom_line()+
    geom_errorbar(aes(ymin = mean-SD, ymax = mean+SD),alpha=0.5)+
    geom_line(aes(x=time,y=true.HR),size=2,linetype = "dashed",color="black")+
    labs(title="", x ="Time", y = TeX("$HR_{Kernel}^C(t)$"))+
    theme_bw()+
    theme(panel.background = element_rect(colour = "black"),
          legend.background=element_blank(),
          axis.text = element_text( size = 20 ),
          axis.text.x = element_text( size = 20 ),
          axis.title = element_text( size = 20, face = "bold" ),
          strip.text = element_text(size = 20),
          panel.spacing = unit(4.5, "lines"))
  
  if(!is.na(ylim1)){
    p2a=p2a+  ylim(0,ylim1)
  }
  
  
  
  
  plot_ec_scenario_IA<-scenario_IA%>% 
    select(time,cox.CI.coverage.percent,CI.coverage.percent ) %>% 
    gather(., method, CI.coverage, cox.CI.coverage.percent:CI.coverage.percent, factor_key=TRUE) %>% 
    mutate(Sceario=name)%>% 
    mutate(CI.coverage=ifelse(CI.coverage<=ylim2,ylim2,CI.coverage))
  
  
  # p3a<-ggplot(data=plot_ec_scenario_IA,aes(x=time,CI.coverage,line,shape=method))+
  #   geom_point(size=2.5)+
  #   geom_line(aes(x=time,CI.coverage))+
  #   theme_bw()+
  #   theme(panel.background = element_rect(colour = "black"),
  #         legend.background=element_blank(),
  #         axis.text = element_text( size = 20 ),
  #         axis.text.x = element_text( size = 15 ),
  #         axis.title = element_text( size = 20, face = "bold" ),
  #         strip.text = element_text(size = 20),
  #         legend.title =element_text( size = 20 ),
  #         legend.text =element_text( size = 20 ),
  #         legend.position="bottom")+
  #   labs(title="", x ="Time", y = "Empirical coverage rate (%)")+
  #   scale_shape(labels = c("Cox","Kernel"),name="Estimation method")
  #  geom_hline(yintercept=95,linetype = "dashed")+
  
  # if(!is.na(ylim2)){
  #   p3a=p3a+  ylim(ylim2,100)
  # }
  # 
  
  plot_SD_scenario_IA<-scenario_IA%>% 
    select(time,SE.kernel,SE.cox,ABSE,cox.ABSE) %>% 
    gather(., method.SD, SD, SE.kernel:SE.cox,factor_key=TRUE) %>% 
    gather(., method.SE, boot_SE, ABSE:cox.ABSE,factor_key=TRUE) %>% 
    filter((method.SD=="SE.kernel"&method.SE=="ABSE")|(method.SD=="SE.cox"&method.SE=="cox.ABSE")) %>% 
    mutate(Sceario=name,method=ifelse(method.SD=="SE.kernel","Kernel","Cox")) %>% 
    mutate(SE.difference=SD-boot_SE,SE.ratio=boot_SE/SD) %>% 
    mutate(SE.ratio=ifelse(SE.ratio>=ylim3,ylim3,SE.ratio))
  
  
  
  p4a<-ggplot(data=plot_SD_scenario_IA,aes(x=time,y=SE.ratio,shape=method))+
    geom_point(size=2.5)+
    geom_line()+
    theme_bw()+
    theme(panel.background = element_rect(colour = "black"),
          legend.background=element_blank(),
          axis.text = element_text( size = 20 ),
          axis.text.x = element_text( size = 15 ),
          axis.title = element_text( size = 20, face = "bold" ),
          strip.text = element_text(size = 20),
          legend.title =element_text( size = 15 ),
          legend.text =element_text( size = 15 ),
          legend.position=c(0.78, 0.1))+
    labs(title="", x ="Time", y = "EST.SE/EMP.SD")+
    scale_shape(labels = c("Cox","Kernel"),name="Estimation method")
  
  
  if(!is.na(ylim3)){
    p4a=p4a+  ylim(0.8,ylim3)
  }
  
  if(!is.na(x.lim)){
    
    p1a=p1a+xlim(-0.05,x.lim)
    p2a=p2a+xlim(-0.05,x.lim)
    # p3a=p3a+xlim(0,x.lim)
    p4a=p4a+xlim(0,x.lim)
  }
  
  
  ggarrange(p1a,p2a,p4a,ncol = 3)
}