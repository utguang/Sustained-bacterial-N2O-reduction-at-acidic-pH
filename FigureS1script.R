library(ggplot2)
library(stringr)
library(export)
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

# Figure 1
microcosm_N2O_curve<- Pyruvate_mass_balance

microcosm_N2O_curve<- within(microcosm_N2O_curve, 
                             Indicator <- factor(Indicator, levels = c("Pyruvate","CO2","Acetate","Formate",
                                                                       "N2O","H2","pH")))

with(microcosm_N2O_curve, levels(Indicator))
microcosm_N2O_curve<- within(microcosm_N2O_curve, 
                             Treatment <- factor(Treatment, levels = c("H2+N2O+Pyruvate",
                                                                       "N2O+Pyruvate","H2+Pyruvate","Low Pyruvate","CO2+H2+N2O","H2+N2O+Pyruvate-uninoculated")))
with(microcosm_N2O_curve, levels(Treatment))

names(microcosm_N2O_curve)<-c("Indicator","day",
                              "Treatment","Substrate","E1",
                              "Product","E2","Group","panel","Label")

first<-subset(microcosm_N2O_curve, !(Group %in% c("25","ab7","ab13","5","18",
                                                  "8","26","ab8","ab14",
                                                  "9","ab9","ab15") | Indicator == "pH"))


first<-subset(first, (day %in% c("0","3","5","7",
                                 "11","15",
                                 "18","18.1","21","25","30",
                                 "33","33.1","40","44")))

first$panel = ifelse((first$Indicator=="H2" | first$Indicator=="N2O" | first$Indicator=="CO2")& first$Treatment == "CO2+H2+N2O", "CO2+H2+N2O", first$panel)
first$panel = ifelse( first$Treatment == "Low Pyruvate", "Low Pyruvate", first$panel)
first$panel = ifelse((first$Indicator=="H2" | first$Indicator=="N2O" | first$Indicator=="Pyruvate")& first$Treatment == "H2+N2O+Pyruvate-uninoculated", "H2+N2O+Pyruvate-uninoculated", first$panel)
first$panel = ifelse((first$Indicator=="H2" | first$Indicator=="N2O" )& first$Treatment == "H2+N2O+Pyruvate", "H2+N2O+Pyruvate", first$panel)
first$panel = ifelse((first$Indicator=="H2" | first$Indicator=="N2O" )& first$Treatment == "H2+Pyruvate", "H2+Pyruvate", first$panel)
first$panel = ifelse((first$Indicator=="H2" | first$Indicator=="N2O" )& first$Treatment == "N2O+Pyruvate", "N2O+Pyruvate", first$panel)

first$panel = ifelse((first$Indicator=="Pyruvate" | first$Indicator=="Acetate" | first$Indicator=="Formate"| first$Indicator=="CO2")& first$Treatment == "H2+N2O+Pyruvate", "H2+N2O+Pyruvate-C", first$panel)
first$panel = ifelse((first$Indicator=="Pyruvate" | first$Indicator=="Acetate" | first$Indicator=="Formate"| first$Indicator=="CO2")& first$Treatment == "H2+Pyruvate", "H2+Pyruvate-C", first$panel)
first$panel = ifelse((first$Indicator=="Pyruvate" | first$Indicator=="Acetate" | first$Indicator=="Formate"| first$Indicator=="CO2")& first$Treatment == "N2O+Pyruvate", "N2O+Pyruvate-C", first$panel)

first<-within(first, Indicator<-factor(Indicator,levels=c("Pyruvate","Acetate","Formate",
                                                          "CO2","N2O","H2")))
with(first, levels(Indicator))

first$facet = ifelse((first$Indicator=="Pyruvate" | first$Indicator=="Acetate" | first$Indicator=="Formate"| first$Indicator=="CO2"), "Carbon", "Gas")
first$facet = ifelse((first$Treatment=="CO2+H2+N2O" | first$Treatment=="Low Pyruvate" | first$Treatment=="H2+N2O+Pyruvate-uninoculated"), "Carbon", first$facet)

first<-subset(first, !((Treatment=="H2+Pyruvate" | 
                          Treatment=="H2+N2O+Pyruvate"|
                          Treatment=="N2O+Pyruvate") &day >18 & Indicator=="CO2"))

first<-within(first, panel<-factor(panel,levels=c("H2+N2O+Pyruvate-C","H2+Pyruvate-C","N2O+Pyruvate-C",
                                                  "H2+N2O+Pyruvate","H2+Pyruvate","N2O+Pyruvate",
                                                  "Low Pyruvate",
                                                  "H2+N2O+Pyruvate-uninoculated",
                                                  "CO2+H2+N2O")))
with(first, levels(panel))

first$width_error<-ifelse(first$Indicator=="Pyruvate"|
                            first$Indicator=="Acetate"|
                            first$Indicator=="Formate",
                            0.5,1.222)

first$width_error<-ifelse(first$Group=="10"|
                            first$Group=="11"|
                            first$Group=="12",
                          0.5,first$width_error)

gp1<-ggplot(data=first,aes(x=day,y=Substrate))+
  geom_errorbar(aes(ymin=Substrate-E1,ymax=Substrate+E1,color=Indicator),width=first$width_error)+
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

save.image(file="FigureS1.Rdata")
