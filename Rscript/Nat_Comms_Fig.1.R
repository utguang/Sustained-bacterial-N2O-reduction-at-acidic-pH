library(reshape2)
library(ggalluvial)
library(ggplot2)
library(Biostrings)
library(stringr)
library(ggpubr)

# Import Fig.1B data using read.excel or import function in Rstudio
Fig_1B<- within(Fig_1B, Genus <- factor(Genus, levels = c("Others","Desulfosporosinus","Pelosinus","Pelotomaculum",
"Rhodoplanes",   "Clostridium", "Serratia",  "Desulfitobacterium"))) 
with(Fig_1B, levels(Genus)) # arrange genus sequence


Fig.1B<-ggplot(Fig_1B,aes(x=variable,y=Fig_1B$`Relative abundance (%)`,fill=Genus,stratum=Genus,alluvium=Genus))+
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

#Fig.1C

Fig.1C<-ggplot(Fig_1C,aes(x=Dataset,y=value,fill=Genome))+
  geom_bar(stat = "identity", position = position_stack(0.2),width=0.1)+word_mytheme+
  scale_y_continuous(expand = c(0,0),limits = c(0,100))+
  scale_fill_manual(breaks = c("Serratia marcescens","Exhalaribacter nitrousreducens"),
                    values = c("blue","red"))+theme(legend.position = "none")+
  theme(axis.ticks.x = element_blank(),axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45,vjust = 0.5,size=10))


# Fig.1D
Fig.1D<-ggplot(data=Fig_1D,aes(x=days,y=Substrate))+
  geom_point(data=Fig_1D,aes(x=days,y=Substrate,shape=Indicator,
                           color=Indicator,fill=Indicator),size=size_point,show.legend = F)+
  geom_line(data=Fig_1D,aes(x=days,y=Substrate,group=Group,color=Indicator,linetype=Indicator),size=0.5,show.legend = F)+
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


#Fig.1E
barplot1<-ggplot(data=Fig_1E[Fig_1E$Treatment == "A. Pyruvate+H2+N2O", ],aes(x=days,y=Substrate))+
  geom_bar(aes(x=days,y=Abundance,fill=Serratia),
           stat = "identity",position = "stack",width = 2,show.legend = F)+
  word_mytheme+theme(axis.ticks.x = element_blank())+
  scale_fill_manual(breaks = c("Ser","DS","Others"),
                     values = c("blue","red","black"))+ scale_y_continuous(expand = c(0, 0))

barplot2<-ggplot(data=Fig_1E[Fig_1E$Treatment == "B. Pyruvate+H2", ],aes(x=days,y=Substrate))+
  geom_bar(aes(x=days,y=Abundance,fill=Serratia),
           stat = "identity",position = "stack",width = 2,show.legend = F)+
  word_mytheme+theme(axis.ticks.x = element_blank())+
  scale_fill_manual(breaks = c("Ser","DS","Others"),
                    values = c("blue","red","black"))+ scale_y_continuous(expand = c(0, 0))


library(export) # load package export

graph2ppt(x=Figure1,file="Figure1",margins=c(0,0,0,0),upscale=T,
          append=T,width=14,height=10) #Export ggplot object to PPT for further combination and annotation








