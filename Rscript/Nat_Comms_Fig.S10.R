
library(ggplot2)
library(patchwork)

Fig.S10A<-ggplot(Fig_S10)+
  geom_point(aes(Time,OD_ave,fill=Treatment,color=Treatment),size=5)+
  geom_line(aes(Time, OD_ave,group=Treatment,color=Treatment))+
  geom_errorbar(aes(x=Time,ymin=OD_ave-OD_error, ymax=OD_ave+OD_error,color=Treatment),width=1)+
  word_mytheme+ylab("OD 600")+xlab("Time (hours)")


Fig.S10B<-ggplot(Fig_S10)+
  geom_point(aes(Time,pH_ave,fill=Treatment,color=Treatment),size=5)+
  geom_line(aes(Time, pH_ave,group=Treatment,color=Treatment))+
  geom_errorbar(aes(x=Time,ymin=pH_ave-pH_error, ymax=pH_ave+pH_error,color=Treatment),width=1)+
  word_mytheme+ylab("Medium pH")+xlab("Time (hours)")


OD|pH
graph2ppt(x= OD|pH,file="Figure_pH",margins=c(0,0,0,0),upscale=T,
          append=T,width=14,height=6)
#export ggplot object to PPT for further annotation



