library(ggplot2)

#Define a ggplot theme for figure S1
word_mytheme<-theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor =element_blank(), 
                               legend.key.size = unit(10,"pt"),strip.text.x = element_blank(),
                               panel.border = element_blank(),panel.spacing=unit(0.1,"lines"),
                               
                               strip.background = element_blank(),
                               axis.ticks.length = unit(-0.10,"cm"),
                               axis.title.x = element_text(hjust=0.5,vjust=-2.0,size=10,family = "Arial"),
                               axis.title.y = element_text(hjust=0.5,vjust=1.0,size=10,family = "Arial"),
                               axis.title.y.right = element_text(angle = 90),
                               axis.text.x.bottom = element_text(vjust=0.5,size = 10,colour="black",family = "Arial"),
                               axis.text.y  = element_text(hjust=0.5,size = 10,colour="black",family = "Arial"),
                               legend.text = element_text(size = 10,family = "Arial"),legend.position = "top",
                               legend.title = element_blank())

#plot Fig.S1 using ggplot package
Fig.S1<-ggplot(data=Fig_S1,aes(x=day,y=Substrate))+
  geom_errorbar(aes(ymin=Substrate-E1,ymax=Substrate+E1,color=Indicator),width=Fig_S1$width_error)+
  geom_point(aes(x=day,y=Substrate,shape=Indicator,fill=Indicator,color=Indicator),size=3)+
  geom_line(aes(x=day,y=Substrate,group=Group,color=Indicator,linetype=Indicator),size=0.5,show.legend = F) +
  scale_y_continuous(breaks = c(0,200,400))+
  facet_wrap(~panel,nrow =3,scales = "free_x")+
  xlab("Time (days)")+
  word_mytheme+
  scale_shape_manual(breaks = c("Pyruvate","Acetate",
                                "Formate","CO2","N2O","H2"),values = c(21,24,22,22,21,25))+
  scale_fill_manual(breaks = c("Pyruvate","Acetate",
                               "Formate","CO2","N2O","H2"),values = c("green","red","blue","white","black",
                                                                      "white")
  )+
  scale_color_manual(breaks = c("Pyruvate","Acetate",
                                "Formate","CO2","N2O","H2"),values = c("green","red","blue","black",
                                                                       "black","black"))+
  scale_linetype_manual(breaks = c("Pyruvate","Acetate",
                                   "Formate","CO2","N2O","H2"),values = c("solid","solid","solid","solid","solid","dashed"))+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(nrow = 1),color=guide_legend(nrow = 1),
         shape=guide_legend(nrow = 1))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)




graph2ppt(x=gp1,file="FigureS1",margins=c(0,0,0,0),
          append=T,width=14,height=15)
