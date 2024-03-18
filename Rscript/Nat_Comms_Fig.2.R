library(ggplot2)
#Fig.2A line plot
Fig_2A_1<-ggplot(data=Fig_2A,aes(x=Day,y=Amount))+ 
  geom_point(data = Fig_2A,aes(x=Day,y=Amount,shape=Gas,fill=Gas,color=Gas),size=3,show.legend = F)+
  geom_line(data = Fig_2A,aes(x=Day,y=Amount,group=Group,color=Gas),size=0.5,show.legend = F)+
  word_mytheme+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  scale_shape_manual(breaks = c("Pyruvate","N2O"),values = c(1,21))+
  scale_color_manual(breaks = c("Pyruvate","N2O"),values = c("black","black"))+
  ylab(str_wrap("Pyruvate and N2O (µmol)",
                width = 30))+
  theme(axis.title.x = element_blank())+
  scale_fill_manual(breaks = c("Pyruvate","N2O"),values = c("black","black"))



#Fig.2A bar plot
Fig_2A_bar<-ggplot()+
  geom_bar(data = Fig_2A_1,mapping=aes(x=Day,y=Percentage,fill=Compound),stat="identity",width = 1)+word_mytheme+
  scale_y_continuous(limits = c(0,100.1),expand = c(0,0))+
  scale_fill_manual(breaks=c("Glutamate","Methionine","N-Acetyl-beta-alanine","N-Acetylglutamine","Aspartate",
                             "Tyrosine","Valine","Alanine","Isoleucine","N-Acetylglutamate"),
                    values=c("Red","Orange","Yellow","green","Blue","Purple","Pink","Brown","Gray","Black"))+
  theme(legend.position = "bottom",axis.ticks.x = element_blank(),axis.title.x = element_blank(),
        legend.key.size = unit(10,"pt"),axis.text.x = element_blank(),
        legend.text = element_text(size = 10,family = "Times New Roman"))+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
guides(fill=guide_legend(ncol=2))  



# pcoa plot
pcoa_mytheme<-theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor =element_blank(), 
                               legend.key.size = unit(10,"pt"),strip.text.x = element_text(size=10,family = "Times New Roman"),
                               panel.spacing=unit(0.3,"lines"),
                               strip.background = element_blank(),
                               axis.ticks.length = unit(-0.10,"cm"),
                               axis.title.x = element_text(hjust=0.5,vjust=-2.0,size=10,family = "Times New Roman"),
                               axis.title.y = element_text(hjust=0.5,vjust=1.0,size=10,family = "Times New Roman"),
                               axis.title.y.right = element_text(angle = 90),
                               axis.text.x.bottom = element_text(vjust=0.5,size = 10,colour="black",family = "Times New Roman"),
                               axis.text.y  = element_text(hjust=0.5,size = 10,colour="black",family = "Times New Roman"),
                               legend.text = element_text(size = 10,family = "Times New Roman"),legend.position = "top",
                               legend.title = element_blank())

library(ggalt)
library(phyloseq)
library(ggplot2)
library(ggforce)
AMI<- otu_table(Fig_2B[,c(-1:-2)], taxa_are_rows = F) # Import amino acid profile as microbiome data
Meta<-sample_data(Fig_2B[,c(1:2)]) 

phyAMI<-phyloseq(AMI,Meta)

AMI.ord<-ordinate(phyAMI,"PCoA","bray")
sample_data(phyAMI)

#Fig.2B
Fig.2B<-plot_ordination(phyAMI,AMI.ord,color="Sample.definition")+
  geom_point(size=3)+
  pcoa_mytheme+xlab("PCoA1 (53%)")+ylab("PCoA2 (37.1%)")+
  scale_color_manual(breaks = c("Day 0","Phase I","Phase II"),
    values=c("black","blue","red"))+
  theme(legend.position = "none",legend.key.size = unit(10,"pt"),
legend.text = element_text(size = 10,family = "Times New Roman"))+
  ylim(-0.3,0.7)+
  scale_y_continuous(breaks = c(-0.2,0.0,0.2,0.4,0.6))+
  geom_mark_ellipse (aes(fill=Sample.definition))+
  scale_fill_manual(breaks = c("Day 0","Phase I","Phase II"),
                     values=c("black","blue","red"))

library(vegan)
metadata_adonis <- as(sample_data(phyAMI), "data.frame")

permanova_result<-adonis2(distance(phyAMI, method="bray") ~ Sample.definition,
       data = metadata_adonis) #permanova analysis




library(ggplot2)
library(stringr)

#Fig.2C amino acid growth experimnets with qPCR assay
  Fig.2C<-ggplot(data = Fig_2C,aes(x=(Day),y=N2O))+
    geom_point(data = Fig_2C,aes(x=(Day),y=N2O,fill=as.character(Group),shape=Gas,color=as.character(Group)),size=2,show.legend = F)+
  geom_line(data = Fig_2C,aes(x=(Day),y=N2O,color=as.character(Group),group=Group),size=0.5,show.legend = F)+
  geom_errorbar(data = Fig_2C,aes(ymin=N2O-SD1,ymax=N2O+SD1,color=as.character(Group)),width=0.3,show.legend = F)+
  geom_point(data = Fig_2C,aes(x=(Day),y=qPCR*250/400,fill=as.character(Group),shape=Gas,color=as.character(Group)),size=2,show.legend = F)+
  geom_line(data = Fig_2C,aes(x=(Day),y=qPCR*250/400,color=as.character(Group)),size=0.5,show.legend = F)+
  geom_errorbar(data = Fig_2C,aes(ymin=(qPCR-SD2)*250/400,ymax=(qPCR+SD2)*250/400,color=as.character(Group)),width=0.3,show.legend = F)+
  scale_fill_manual(breaks = c("1","2","3","4"),values = c("red","red","blue","orange"))+
  scale_color_manual(breaks = c("1","2","3","4"),values = c("red","red","blue","orange"))+
  scale_shape_manual(breaks = c("Pyruvate","N2O","Desulfosporosinus"),values = c(21,21,5))+word_mytheme+
  scale_y_continuous(limits =c(0,450), breaks = c(0,200,400),
                     sec.axis = sec_axis(~ . * 400/250, breaks = c(0,200,400,600)))+xlab("Time (days)")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  annotate("segment", x=Inf, xend=Inf, y=-Inf, yend=Inf,size=0.5)+
  ylab(str_wrap("N2O (µmol)",
                width = 10)) +xlim(0,22)+
  ggbreak::scale_x_break(c(7.9,17.9),ticklabels = c(0,2,4,6,7,20) )+
    theme(axis.title = element_blank())
  


library(ggplot2)
library(patchwork)
  
  #combine Fig2A, and B
  Figure2<-((Fig_2A_1/Fig_2A_bar)|(Fig.2B/plot_spacer()))+
    plot_layout(ncol = 3,widths = c(1,1,0.7),
                nrow = 2,heights = c(1,0.3))
# combine Fig.2A,B and C
Figure2_qPCR<-((N2Oplot/barplot)|(pcoa_plot/qAMINO))+
  plot_layout(ncol = 3,widths = c(1,1,0.7),
              nrow = 2,heights = c(1,0.4))

library(export)
graph2ppt(x=Figure21,file="Figure2",margins=c(0,0,0,0),upscale=T,
          append=T,width=14,height=10) # export Fig.2A and B to PPT

graph2ppt(x=Figure2,file="Figure2",margins=c(0,0,0,0),upscale=T,
          append=T,width=14,height=10) #export Fig.2ABC to PPT




save.image(file = "PNAS_main_figure2.Rdata")
