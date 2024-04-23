
library(ggplot2)

library(magrittr)
#define theme
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

#Plot amino acid spatial dynamics using ggplot2 package
Fig.S4<-ggplot()+
  geom_point(data = Fig_S4,aes(x=Day,y=Peak,color=Treatment),size=2)+
  geom_smooth(data = Fig_S4,aes(x=Day,y=Peak,color=Treatment),linewidth=0.5)+
  word_mytheme+
  scale_fill_manual(values = c("red","blue"))+
  xlab("Time (days)")+theme(legend.position = c(0.9,0.2))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  facet_wrap(~compound,scales = "free_y",ncol = 4)+
  scale_color_manual(breaks=c("FLG","Serratia"),
                     values=c("Red","Orange"))+
  scale_linetype_manual(breaks = c("FLG","Serratia"),values = c(1,2))+
  scale_y_continuous(labels = function(x) {
    paste0(x, " x 106")  # \u2076 is the unicode character for superscript 6
  })

graph2ppt(x=Fig.S4,file="FigureS4",margins=c(0,0,0,0),upscale=T,
          append=T,width=14,height=10)# export ggplot object to PPT for further annotation

