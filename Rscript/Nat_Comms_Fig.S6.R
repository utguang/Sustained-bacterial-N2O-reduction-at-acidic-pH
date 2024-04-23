


word_mytheme<-theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor =element_blank(), 
                               legend.key.size = unit(10,"pt"),strip.text.x = element_blank(),
                               panel.border = element_blank(),panel.spacing=unit(0.1,"lines"),
                               
                               strip.background = element_blank(),
                               axis.ticks.length = unit(-0.10,"cm"),
                               axis.title.x = element_text(hjust=0.5,vjust=-2.0,size=10,family = "Times New Roman"),
                               axis.title.y = element_text(hjust=0.5,vjust=1.0,size=10,family = "Times New Roman"),
                               axis.title.y.right = element_text(angle = 90),
                               axis.text.x.bottom = element_text(vjust=0.5,size = 10,colour="red",family = "Times New Roman"),
                               axis.text.y  = element_text(hjust=0.5,size = 10,colour="red",family = "Times New Roman"),
                               legend.text = element_text(size = 10,family = "Times New Roman"),legend.position = "top",
                               legend.title = element_blank())

Fig.S6A<-ggplot(Fig_S6A,aes(Day,OD))+
  facet_wrap(~Strain,scales = "free_y",ncol = 1)+  
  geom_errorbar(mapping=aes(ymin=(OD-error),ymax=(OD+error),color=Treatment),width=0.2)+
  geom_point(aes(Day,OD,color=Treatment,shape=Treatment),size=3)+
  geom_line(aes(Day,OD,color=Treatment),size=0.8)+word_mytheme+
  scale_fill_manual(breaks = c("Sulfate","Sulfate_1","N2O","N2O_1"),
                    values = c("red","red","blue","blue"))+
  scale_color_manual(breaks = c("Sulfate","Sulfate_1","N2O","N2O_1"),
                     values = c("red","red","blue","blue"))+
  scale_shape_manual(breaks = c("Sulfate","Sulfate_1","N2O","N2O_1"),
                     values = c(19,19,19,19))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  scale_x_continuous(breaks = c(0,3,6,9))+
  scale_y_continuous(breaks = c(0,0.025,0.05),limits = c(0,0.06))+
  theme(legend.position = "none")

Fig.S6B<-ggplot(Fig_S6B,aes(Day,OD))+
  facet_wrap(~Strain,scales = "free_y",ncol = 1)+  
  geom_errorbar(mapping=aes(ymin=(OD-error),ymax=(OD+error),color=Treatment),width=0.2)+
  geom_point(aes(Day,OD,color=Treatment,shape=Treatment),size=3)+
  geom_line(aes(Day,OD,color=Treatment),size=0.8)+word_mytheme+
  scale_fill_manual(breaks = c("Sulfate","Sulfate_1","N2O","N2O_1"),
                    values = c("red","red","blue","blue"))+
  scale_color_manual(breaks = c("Sulfate","Sulfate_1","N2O","N2O_1"),
                     values = c("red","red","blue","blue"))+
  scale_shape_manual(breaks = c("Sulfate","Sulfate_1","N2O","N2O_1"),
                     values = c(19,19,19,19))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  scale_x_continuous(breaks = c(0,3,6,9))+
  scale_y_continuous(breaks = c(0,0.03,0.06,0.09,0.12,0.15),limits = c(0,0.15))+
  theme(legend.position = "none")

library(patchwork)
Fig.S6<-(Fig.S6A|Fig.S6B)/plot_spacer()

