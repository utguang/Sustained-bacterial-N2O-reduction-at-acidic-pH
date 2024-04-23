library(gggenes)
library(ggplot2)
library(export)

word_mytheme<-theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor =element_blank(), 
                               legend.key.size = unit(10,"pt"),strip.text.x = element_blank(),
                               panel.border = element_blank(),panel.spacing=unit(0.1,"lines"),
                               
                               strip.background = element_blank(),
                               axis.ticks.length = unit(-0.10,"cm"),
                               axis.title.x = element_text(hjust=0.5,vjust=-2.0,size=10,family = "Times New Roman"),
                               axis.title.y = element_text(hjust=0.5,vjust=1.0,size=10,family = "Times New Roman"),
                               axis.title.y.right = element_text(angle = 90),
                               axis.text.x.bottom = element_text(vjust=0.5,size = 10,colour="black",family = "Times New Roman"),
                               axis.text.y  = element_text(hjust=0.5,size = 10,colour="black",family = "Times New Roman"),
                               legend.text = element_text(size = 10,family = "Times New Roman"),legend.position = "top",
                               legend.title = element_blank())


Fig.S7<-ggplot(Fig_S7,aes(xmin=start,xmax=stop,y=Function,forward=orientation)) +
  geom_gene_arrow(color="black",
                  arrow_body_height = unit(4,"mm"),
                  arrowhead_height = unit(8, "mm"), 
                  arrowhead_width = unit(2, "mm")   )  +
  geom_gene_label(aes(label=gene)) +
  facet_wrap(~Facet,scales = "free",ncol = 1)+theme_bw()+
  theme(axis.title.x=element_blank(),axis.line.x = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor =element_blank(), 
        legend.key.size = unit(10,"pt"),
        strip.text.x = element_blank(),legend.position = "none",
        panel.border = element_blank(),panel.spacing=unit(0.1,"lines"),
        strip.background = element_blank())+ylab("Function")

graph2ppt(x=Fig.S7,file="FigureS7",margins=c(0,0,0,0),upscale=T,
          append=T,width=14,height=10)

