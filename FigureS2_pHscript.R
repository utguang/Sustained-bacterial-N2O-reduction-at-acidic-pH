library(ggplot2)
library(stringr)
library(export)
library(ggmagnify)


# theme setup
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

pH_curve<-pH_profile_for_paper[c(-49:-52),]
pH_curve_amino_acid<-pH_profile_for_paper[c(49:52),]

pH_data<-ggplot(data=pH_curve,aes(x=Day,y=pH))+
  geom_errorbar(aes(ymin=pH-Error2,ymax=pH+Error2,color=Treatment),width=1)+
  geom_point(aes(x=Day,y=pH,color=Treatment),size=3)+
  geom_smooth(se=F,aes(x=Day,y=pH,group=ID,color=Treatment),size=0.5,show.legend = F) +
  scale_y_continuous(breaks = c(4.25,4.5,4.75,5.0))+
  xlab("Time (days)")+
  word_mytheme+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(nrow = 1),color=guide_legend(nrow = 1),
         shape=guide_legend(nrow = 1))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  scale_color_manual(breaks = c("250 µmol Pyruvate",
                                "50 µmol Pyruvate","No Pyruvate",
                                "No inocula","Amino acid"),
                     values = c("red","blue","green","cyan","Orange"))

pH_data+geom_magnify(aes(from=Treatment=="Amino acid" ),
                         to = c(0,10,5,5.25))




graph2ppt(x=pH_data,file="FigureS2",margins=c(0,0,0,0),
          append=T,width=14,height=15)

save.image(file="FigureS2_pH_profile.Rdata")

