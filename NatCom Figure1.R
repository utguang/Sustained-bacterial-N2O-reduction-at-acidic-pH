library(reshape2)
library(ggalluvial)
library(ggplot2)
library(Biostrings)
library(stringr)
alluvial<-melt(Alluvial_data)

alluvial<- within(alluvial, Genus <- factor(Genus, levels = c("Others","Desulfosporosinus","Pelosinus","Pelotomaculum",
                                                              "Rhodoplanes",   "Clostridium", "Serratia",  "Desulfitobacterium")))
with(alluvial, levels(Genus))


culture<-ggplot(alluvial,aes(x=variable,y=value,fill=Genus,stratum=Genus,alluvium=Genus))+
  geom_bar(stat = "identity", position = position_stack(0.2),width=0.15)+
  theme_bw()+word_mytheme+theme(panel.border=element_blank(),legend.position = c(0.5,0.4)) + 
  labs(y="Relative Abundance")+
  xlab("")+ylab("Relative Abundance (%)")+
  theme(axis.text.x = element_text(angle=45,vjust = 0.5,size=10))+
  theme(axis.title.y = element_text(size=10))+
  scale_fill_manual(breaks=c("Others","Desulfosporosinus","Pelosinus","Pelotomaculum",
                             "Rhodoplanes", "Clostridium", "Serratia",  "Desulfitobacterium"),
                    values=c("black","red","pink","brown","yellow","orange","blue",
                             "cyan"))+
  theme(axis.ticks.x = element_blank())+ scale_y_continuous(expand = c(0, 0))+
  theme(legend.text = element_text(face="italic"),legend.position = "none")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)

# For N2O in batch III
qPCR<- Pyruvate_H2_qPCR_essay

qPCR<- within(qPCR, Gas <- factor(Gas, levels = c("Pyruvate",
                                                  "N2O","Desulfosporosinus","Serratia")))
with(qPCR, levels(Gas))



qPCR<- within(qPCR, Treatment <- factor(Treatment, levels = c("A. Pyruvate+H2+N2O",
                                                              "B. Pyruvate+H2")))
with(qPCR, levels(Treatment))
qPCR<-subset(qPCR,Treatment != "Serratia: H2+N2O+Pyruvate")
qPCR<-subset(qPCR,Gas != "Total 16S")


head(qPCR)
names(qPCR)<-c("Indicator","days","Treatment","Substrate","E1","Product","E2","Group",
               "Abundance","Serratia")

qPCR<- within(qPCR, Serratia <- factor(Serratia, levels = c("Others",
                                                            "DS","Ser")))
with(qPCR, levels(Serratia))

ampli<-subset(qPCR[c(47,48,53,54,59,60,65,66,67,68,69,70),])

gp5_present<-ggplot(data=ampli,aes(x=days,y=Substrate))+
  geom_bar(aes(x=days,y=Abundance*400/100,fill=Serratia,color=Serratia),
           stat = "identity",position = "stack",width = 0.7,show.legend = F) +
  facet_wrap(~Treatment,ncol=2,scales = "free_x")+
  geom_point(data=qPCR,aes(x=days,y=Substrate,shape=Indicator,
                           color=Indicator,fill=Indicator),size=3)+
  geom_line(data=qPCR,aes(x=days,y=Substrate,group=Group,color=Indicator,linetype=Indicator),size=0.5,show.legend = F)+
  scale_y_continuous(limits = c(0,445),breaks = c(0,200,400),
                     sec.axis = sec_axis(~ . * 160/400, 
                                         name="Cells (x10-4 mL-1)"))+
  facet_wrap(~Treatment,ncol=2)+
  scale_shape_manual(breaks = c("Pyruvate","N2O","Serratia","Desulfosporosinus","Ser","DS","Others"),
                     values = c(21,21,23,23))+
  scale_fill_manual(breaks = c("Pyruvate","N2O","Serratia","Desulfosporosinus","Ser","DS","Others"),
                    values = c("green","black","cyan","purple","blue","red","black"))+
  scale_color_manual(breaks = c("Pyruvate","N2O","Serratia","Desulfosporosinus","Ser","DS","Others"),
                     values = c("green","black","cyan","purple","blue","red","black"))+
  scale_linetype_manual(values = c("solid","solid","solid","solid", "solid"))+
  word_mytheme+guides(color="none",fill="none")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=Inf, yend=Inf,size=0.5)+
  ylab(str_wrap("Pyruvate, acetate, formate, CO2 and N2O and H2 (µmol vessel-1)",
                width = 30))+
  ylab(str_wrap("Pyruvate and N2O (µmol vessel-1)",
                width = 20))+
  guides( shape=guide_legend(ncol = 4))+xlab("Time (days)")+
  geom_errorbar(data=qPCR,aes(ymin=Substrate-E1,ymax=Substrate+E1,
                              color=Indicator),width=0.2)

Figure1<-(plot_spacer()|plot_spacer()|culture)/(gp5_present+theme(legend.position = "none"))
graph2ppt(x=Figure1,file="Data-go-to-co-culture",margins=c(0,0,0,0),upscale=T,
          append=T,width=14,height=10)

qPCR<-qPCR[c(1:42),]
qPCR<-qPCR[qPCR$Treatment == "A. Pyruvate+H2+N2O", ]

qPCR$poinsize<-ifelse(qPCR$Indicator=="Pyruvate"|
                        qPCR$Indicator=="N2O","large","small" )

size_point<-ifelse(qPCR$poinsize=="large",3,2)

# separate the line and barplot
qPCRplot<-ggplot(data=qPCR[qPCR$Treatment == "A. Pyruvate+H2+N2O", ],aes(x=days,y=Substrate))+
  geom_point(data=qPCR[qPCR$Treatment == "A. Pyruvate+H2+N2O", ],aes(x=days,y=Substrate,shape=Indicator,
                           color=Indicator,fill=Indicator),size=size_point,show.legend = F)+
  geom_line(data=qPCR[qPCR$Treatment == "A. Pyruvate+H2+N2O", ],aes(x=days,y=Substrate,group=Group,color=Indicator,linetype=Indicator),size=0.5,show.legend = F)+
  scale_y_continuous(limits = c(0,415),breaks = c(0,200,400),
                     sec.axis = sec_axis(~ . * 80/400, 
                                         name="Cells (x104 mL-1)"))+
  facet_wrap(~Treatment,ncol=2)+
  scale_shape_manual(breaks = c("Pyruvate","N2O","Serratia","Desulfosporosinus"),
                     values = c(1,21,5,5))+
  scale_fill_manual(breaks = c("Pyruvate","N2O","Serratia","Desulfosporosinus"),
                    values = c("black","black","blue","red"))+
  scale_color_manual(breaks = c("Pyruvate","N2O","Serratia","Desulfosporosinus"),
                     values = c("black","black","blue","red"))+
  scale_linetype_manual(values = c("solid","solid","solid","solid", "solid"))+
  word_mytheme+guides(color="none",fill="none")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=Inf, yend=Inf,size=0.5)+
  ylab(str_wrap("Pyruvate, acetate, formate, CO2 and N2O and H2 (µmol vessel-1)",
                width = 30))+
  ylab(str_wrap("Pyruvate and N2O (µmol vessel-1)",
                width = 20))+
  guides( shape=guide_legend(ncol = 4))+
  geom_errorbar(data=qPCR[qPCR$Treatment == "A. Pyruvate+H2+N2O", ],aes(ymin=Substrate-E1,ymax=Substrate+E1,
                              color=Indicator),width=0.2)+
  theme(axis.title.x = element_blank())


barplot1<-ggplot(data=ampli[ampli$Treatment == "A. Pyruvate+H2+N2O", ],aes(x=days,y=Substrate))+
  geom_bar(aes(x=days,y=Abundance,fill=Serratia),
           stat = "identity",position = "stack",width = 2,show.legend = F)+
  word_mytheme+theme(axis.ticks.x = element_blank())+
  scale_fill_manual(breaks = c("Ser","DS","Others"),
                     values = c("blue","red","black"))+ scale_y_continuous(expand = c(0, 0))

barplot2<-ggplot(data=ampli[ampli$Treatment == "B. Pyruvate+H2", ],aes(x=days,y=Substrate))+
  geom_bar(aes(x=days,y=Abundance,fill=Serratia),
           stat = "identity",position = "stack",width = 2,show.legend = F)+
  word_mytheme+theme(axis.ticks.x = element_blank())+
  scale_fill_manual(breaks = c("Ser","DS","Others"),
                    values = c("blue","red","black"))+ scale_y_continuous(expand = c(0, 0))
# transfer community change
culture<-ggplot(alluvial,aes(x=variable,y=value,fill=Genus,stratum=Genus,alluvium=Genus))+
  geom_bar(stat = "identity", position = position_stack(0.2),width=0.1)+
  theme_bw()+word_mytheme+theme(panel.border=element_blank(),legend.position = c(0.5,0.4)) + 
  labs(y="Relative Abundance")+
  xlab("")+ylab("Relative Abundance (%)")+
  theme(axis.text.x = element_text(angle=45,vjust = 0.5,size=10))+
  theme(axis.title.y = element_text(size=10))+
  scale_fill_manual(breaks=c("Others","Desulfosporosinus","Pelosinus","Pelotomaculum",
                             "Rhodoplanes", "Clostridium", "Serratia",  "Desulfitobacterium"),
                    values=c("black","red","pink","brown","yellow","orange","blue",
                             "cyan"))+
  theme(axis.ticks.x = element_blank())+ scale_y_continuous(expand = c(0, 0))+
  theme(legend.text = element_text(face="italic"))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)

library(ggbreak)
rec_data<-Recplot_data[,c(1,2,8)]
rec<-melt(rec_data)

rec<- within(rec, Dataset <- factor(Dataset, levels = c("Microcosm","Consortium")))
with(rec, levels(Dataset))

recplot<-ggplot(rec,aes(x=Dataset,y=value,fill=Genome))+
  geom_bar(stat = "identity", position = position_stack(0.2),width=0.1)+word_mytheme+
  scale_y_continuous(expand = c(0,0),limits = c(0,100))+
  scale_fill_manual(breaks = c("Serratia marcescens","Exhalaribacter nitrousreducens"),
                    values = c("blue","red"))+theme(legend.position = "none")+
  theme(axis.ticks.x = element_blank(),axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45,vjust = 0.5,size=10))
       
library(patchwork)

Figure1<-(plot_spacer()/culture)|(plot_spacer()/recplot)|(qPCRplot/(barplot1|barplot2))+plot_layout(widths = c(2,1))


graph2ppt(x=Figure1,file="Figure1",margins=c(0,0,0,0),upscale=T,
          append=T,width=14,height=10)


library(export)

save.image(file = "PNAS_main_figure1.Rdata")




