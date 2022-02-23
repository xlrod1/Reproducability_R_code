
create.plot.combined.CI<-function(data){
  
  cbp1 <- c( "#999999","#E69F00", "#56B4E9", "#009E73",
             "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  
  time<-data$time
  cil<-data$mean.cIL.kernel
  ciu<-data$mean.cIU.kernel
  method<-"Kernel"
  ci.data.frame<-data.frame(time=time,cil=cil,ciu=ciu,method=method)
  
  time<-data$time
  cil<-data$mean.cIL.cox
  ciu<-data$mean.cIU.cox
  method<-"Cox"
  ci.data.frame2<-data.frame(time=time,cil=cil,ciu=ciu,method=method)
  
  
  ci.df<-rbind(ci.data.frame,ci.data.frame2)
  ci.df$true.hr=rep(data$true.HR,2)
  
  
  ci.df%>%ggplot(aes(x = time, y = true.hr)) + 
    geom_line(col="black") + 
    geom_ribbon(aes(ymin = cil, ymax = ciu,fill=method), alpha = 0.5)+
    scale_fill_manual(values = cbp1)+ theme_bw()+
    theme(panel.background = element_rect(colour = "black"),
          legend.position = c(0.7, 0.7),legend.background=element_blank())+
    labs(title="", x ="Time", y = "Causal HR(t)")
 
  
}

