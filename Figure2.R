
color_point<-ifelse(minimal_amino_acid$Gas=="Pyruvate", "blue","red")

N2Odata<-minimal_amino_acid


N2Oplot<-ggplot(data=N2Odata,aes(x=Day,y=Amount))+ 
  geom_point(data = N2Odata,aes(x=Day,y=Amount,shape=Gas,fill=Gas,color=Gas),size=3,show.legend = F)+
  geom_line(data = N2Odata,aes(x=Day,y=Amount,group=Group,color=Gas),size=0.5,show.legend = F)+
  word_mytheme+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  scale_shape_manual(breaks = c("Pyruvate","N2O"),values = c(1,21))+
  scale_color_manual(breaks = c("Pyruvate","N2O"),values = c("black","black"))+
  ylab(str_wrap("Pyruvate and N2O (µmol)",
                width = 30))+
  theme(axis.title.x = element_blank())+
  scale_fill_manual(breaks = c("Pyruvate","N2O"),values = c("black","black"))



require(tidyverse)
amm<- subset(minimal_amino_acid,compound!="Cystine") %>%
  group_by(Group) %>% 
  mutate(Percentage=paste0(round(Peak/sum(Peak)*100,3)))

amm$Percentage<-as.numeric(amm$Percentage)
class(amm$Percentage)

amm$Species<-amm$compound
amm$Species<-ifelse(amm$Percentage >= 5, amm$compound, "Others")



amm<- within(amm, compound <- factor(compound, levels = c("Glutamate","Methionine","N-Acetyl-beta-alanine","N-Acetylglutamine","Aspartate",
                                                          "Tyrosine","Valine","Alanine","Isoleucine","N-Acetylglutamate")))
with(amm, levels(compound))
amm<-transform(amm,sum=430)

amm<-subset(amm,Treatment!="Serratia")


barplot<-ggplot()+
  geom_bar(data = amm,mapping=aes(x=Day,y=Percentage,fill=compound),stat="identity",width = 1)+word_mytheme+
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

AMI<- otu_table(minimal_amino_acid, taxa_are_rows = F)
Meta<-sample_data(minimal_amino_acid)

phyAMI<-phyloseq(AMI,Meta)

AMI.ord<-ordinate(phyAMI,"PCoA","bray")

pcoa_plot<-plot_ordination(phyAMI,AMI.ord,color="Phase")+
  geom_point(size=3)+
  pcoa_mytheme+xlab("PCoA1 (53%)")+ylab("PCoA2 (37.1%)")+
  scale_color_manual(breaks = c("T0","Pyruvate","N2O"),
    values=c("black","blue","red"))+
  theme(legend.position = "none",legend.key.size = unit(10,"pt"),
legend.text = element_text(size = 10,family = "Times New Roman"))+
  ylim(-0.3,0.7)+
  scale_y_continuous(breaks = c(-0.2,0.0,0.2,0.4,0.6))+
  geom_mark_ellipse (aes(fill=Phase))+
  scale_fill_manual(breaks = c("T0","Pyruvate","N2O"),
                     values=c("black","blue","red"))

library(ggforce)

library(vegan)
metadata_adonis <- as(sample_data(phyAMI), "data.frame")

permanova_result<-adonis2(distance(phyAMI, method="bray") ~ Phase,
       data = metadata_adonis)

# Time-serial amino acid
aaa<-minimal_amino_acid
aaa<- within(aaa, compound <- factor(compound, levels = c("Alanine",  "Valine","Isoleucine","Aspartate","Tyrosine",
                                                        "N-Acetylglutamate","N-Acetylglutamine","N-Acetyl-beta-alanine","Glutamate","Methionine")))
with(aaa, levels(compound))

aaa$compound<-factor(aaa$compound,levels=c("Alanine","Valine","Aspartate","Tyrosine","Isoleucine","Glutamate"))

library(scales)
Total<-ggplot()+
  geom_point(data = aaa,aes(x=Day,y=Peak,color=Treatment),size=2)+
  geom_line(data = aaa,aes(x=Day,y=Peak,group=GR,linetype=Treatment,color=Treatment),size=0.5)+
  word_mytheme+
  scale_fill_manual(values = c("red","blue"))+
  xlab("Time (days)")+theme(legend.position = c(0.9,0.2))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  theme(axis.text.y = element_blank(),axis.title.x = element_blank())+
  facet_wrap(~compound,scales = "free_y",ncol = 2)+
  scale_color_manual(breaks=c("FLG","Serratia"),
                    values=c("Red","Orange"))+
  scale_linetype_manual(breaks = c("FLG","Serratia"),values = c(1,2))

graph2ppt(x=Total,file="FigureS5",margins=c(0,0,0,0),upscale=T,
          append=T,width=14,height=10)

  


Alanine<-ggplot()+
  geom_point(data = subset(aaa,compound=="Alanine"),aes(x=Day,y=Peak),size=3,color="black")+
  geom_line(data = subset(aaa,compound=="Alanine"),aes(x=Day,y=Peak,group=GR,linetype=Treatment),size=0.5,color="black")+
  word_mytheme+
  scale_fill_manual(values = c("red","blue"))+
  scale_y_continuous(labels = scales::scientific,n.breaks = 5)+xlab("Time (days)")+theme(legend.position = c(0.8,0.2))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  theme(axis.text.y = element_blank(),axis.title.x = element_blank())+scale_y_continuous(limits = c(0,3000000))



Valine<-ggplot()+
  geom_point(data = subset(aaa,compound=="Valine"),aes(x=Day,y=Peak),size=3,color="black")+
  geom_line(data = subset(aaa,compound=="Valine"),aes(x=Day,y=Peak,group=GR,linetype=Treatment),size=0.5,color="black")+
  word_mytheme+
  scale_fill_manual(values = c("red","blue"))+
  scale_y_continuous(labels = scales::scientific,n.breaks = 5)+xlab("Time (days)")+theme(legend.position = c(0.8,0.2))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  theme(axis.text.y = element_blank(),axis.title.x = element_blank())+scale_y_continuous(limits = c(0,8000000))

Isoleucine<-ggplot()+
  geom_point(data = subset(aaa,compound=="Isoleucine"),aes(x=Day,y=Peak),size=3,color="black")+
  geom_line(data = subset(aaa,compound=="Isoleucine"),aes(x=Day,y=Peak,group=GR,linetype=Treatment),size=0.5,color="black")+
  word_mytheme+
  scale_fill_manual(values = c("red","blue"))+
  scale_y_continuous(labels = scales::scientific,n.breaks = 5)+xlab("Time (days)")+theme(legend.position = c(0.8,0.2))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  theme(axis.text.y = element_blank(),axis.title.x = element_blank())+scale_y_continuous(limits = c(0,3000000))

Aspartate<-ggplot()+
  geom_point(data = subset(aaa,compound=="Aspartate"),aes(x=Day,y=Peak),size=3,color="black")+
  geom_line(data = subset(aaa,compound=="Aspartate"),aes(x=Day,y=Peak,group=GR,linetype=Treatment),size=0.5,color="black")+
  word_mytheme+
  scale_fill_manual(values = c("red","blue"))+
  scale_y_continuous(labels = scales::scientific,n.breaks = 5)+xlab("Time (days)")+theme(legend.position = c(0.8,0.2))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  theme(axis.text.y = element_blank(),axis.title.x = element_blank())+scale_y_continuous(limits = c(0,2000000))


# Amino acid with qPCR assay

Amino<-Amino_acid_qPCR

# not need when need to scale data
Amino$N2O<- ifelse( is.na(Amino$N2O)=="TRUE", Amino$qPCR, Amino$N2O)
Amino$SD1<- ifelse( is.na(Amino$SD1)=="TRUE", Amino$SD2, Amino$SD1)

library(ggplot2)
library(stringr)

  qAMINO<-ggplot(data = Amino,aes(x=(Day),y=N2O))+
    geom_point(data = Amino,aes(x=(Day),y=N2O,fill=as.character(Group),shape=Gas,color=as.character(Group)),size=2,show.legend = F)+
  geom_line(data = Amino,aes(x=(Day),y=N2O,color=as.character(Group),group=Group),size=0.5,show.legend = F)+
  geom_errorbar(data = Amino,aes(ymin=N2O-SD1,ymax=N2O+SD1,color=as.character(Group)),width=0.3,show.legend = F)+
  geom_point(data = Amino,aes(x=(Day),y=qPCR*250/400,fill=as.character(Group),shape=Gas,color=as.character(Group)),size=2,show.legend = F)+
  geom_line(data = Amino,aes(x=(Day),y=qPCR*250/400,color=as.character(Group)),size=0.5,show.legend = F)+
  geom_errorbar(data = Amino,aes(ymin=(qPCR-SD2)*250/400,ymax=(qPCR+SD2)*250/400,color=as.character(Group)),width=0.3,show.legend = F)+
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
  
  
  Figure2<-((N2Oplot/barplot)|(pcoa_plot/plot_spacer()))+
    plot_layout(ncol = 3,widths = c(1,1,0.7),
                nrow = 2,heights = c(1,0.3))

Figure21<-((N2Oplot/barplot)|(pcoa_plot/qAMINO))+
  plot_layout(ncol = 3,widths = c(1,1,0.7),
              nrow = 2,heights = c(1,0.4))

library(export)
graph2ppt(x=Figure21,file="Figure2",margins=c(0,0,0,0),upscale=T,
          append=T,width=14,height=10)

graph2ppt(x=Figure2,file="Figure2",margins=c(0,0,0,0),upscale=T,
          append=T,width=14,height=10)




save.image(file = "PNAS_main_figure2.Rdata")
