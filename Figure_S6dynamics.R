
library(ggplot2)
# Time-serial amino acid
aaa<-minimal_amino_acid
aaa<- within(aaa, compound <- factor(compound, levels = c("Alanine",  "Valine","Isoleucine","Aspartate","Tyrosine",
                                                          "Glutamate","Methionine")))
with(aaa, levels(compound))

aaa$compound<-factor(aaa$compound,levels=c("Alanine","Valine","Aspartate","Tyrosine","Isoleucine","Glutamate","Methionine"))

library(magrittr)

Total<-ggplot()+
  geom_point(data = aaa,aes(x=Day,y=AUC,color=Treatment),size=2)+
 geom_smooth(data = aaa,aes(x=Day,y=AUC,color=Treatment),linewidth=0.5)+
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

library(export)
graph2ppt(x=Total,file="FigureS6-7",margins=c(0,0,0,0),upscale=T,
          append=T,width=14,height=10)

Alanine<-ggplot()+
  geom_point(data = subset(aaa,compound=="Alanine"),aes(x=Day,y=Peak,color=Treatment),size=3)+
  geom_smooth(data = subset(aaa,compound=="Alanine"),aes(x=Day,y=Peak,color=Treatment),linewidth=0.5)+
  word_mytheme+
  scale_fill_manual(values = c("red","blue"))+
  scale_y_continuous(labels = scales::scientific,n.breaks = 5)+xlab("Time (days)")+theme(legend.position = c(0.8,0.2))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  scale_y_continuous(limits = c(0,3000000))+
  theme(legend.position = "none")+
  scale_color_manual(breaks=c("FLG","Serratia"),
                     values=c("Red","Orange"))+
  scale_linetype_manual(breaks = c("FLG","Serratia"),values = c(1,2))+
  theme(axis.text.x = element_blank(),axis.title.x = element_blank())

Valine<-ggplot()+
  geom_point(data = subset(aaa,compound=="Valine"),aes(x=Day,y=Peak,color=Treatment),size=3)+
  geom_smooth(data = subset(aaa,compound=="Valine"),aes(x=Day,y=Peak,color=Treatment),linewidth=0.5)+
  word_mytheme+
  scale_fill_manual(values = c("red","blue"))+
  scale_y_continuous(labels = scales::scientific,n.breaks = 5)+xlab("Time (days)")+theme(legend.position = c(0.8,0.2))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  scale_y_continuous(limits = c(0,8000000))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_blank(),axis.title.x = element_blank())


Isoleucine<-ggplot()+
  geom_point(data = subset(aaa,compound=="Isoleucine"),aes(x=Day,y=Peak,color=Treatment),size=3)+
  geom_smooth(data = subset(aaa,compound=="Isoleucine"),aes(x=Day,y=Peak,color=Treatment),linewidth=0.5)+
  word_mytheme+
  scale_fill_manual(values = c("red","blue"))+
  scale_y_continuous(labels = scales::scientific,n.breaks = 5)+xlab("Time (days)")+theme(legend.position = c(0.8,0.2))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  scale_y_continuous(limits = c(0,3000000))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_blank(),axis.title.x = element_blank())



Aspartate<-ggplot()+
  geom_point(data = subset(aaa,compound=="Aspartate"),aes(x=Day,y=Peak,color=Treatment),size=3)+
  geom_smooth(data = subset(aaa,compound=="Aspartate"),aes(x=Day,y=Peak,color=Treatment),linewidth=0.5)+
  word_mytheme+
  scale_fill_manual(values = c("red","blue"))+
  scale_y_continuous(labels = scales::scientific,n.breaks = 5)+xlab("Time (days)")+theme(legend.position = c(0.8,0.2))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  scale_y_continuous(limits = c(0,2000000))+
  theme(legend.position = "none")

Tyrosine<-ggplot()+
  geom_point(data = subset(aaa,compound=="Tyrosine"),aes(x=Day,y=Peak,color=Treatment),size=3)+
  geom_smooth(data = subset(aaa,compound=="Tyrosine"),aes(x=Day,y=Peak,color=Treatment),linewidth=0.5)+
  word_mytheme+
  scale_fill_manual(values = c("red","blue"))+
  scale_y_continuous(labels = scales::scientific,n.breaks = 5)+xlab("Time (days)")+theme(legend.position = c(0.8,0.2))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  scale_y_continuous(limits = c(0,600000))+
  theme(legend.position = "none")

Glutamate<-ggplot()+
  geom_point(data = subset(aaa,compound=="Glutamate"),aes(x=Day,y=Peak,color=Treatment),size=3)+
  geom_smooth(data = subset(aaa,compound=="Glutamate"),aes(x=Day,y=Peak,color=Treatment),linewidth=0.5)+
  word_mytheme+
  scale_fill_manual(values = c("red","blue"))+
  scale_y_continuous(labels = scales::scientific,n.breaks = 5)+xlab("Time (days)")+theme(legend.position = c(0.8,0.2))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  scale_y_continuous(limits = c(0,4000000))+
  theme(legend.position = "none")


Methionine<-ggplot()+
  geom_point(data = subset(aaa,compound=="Methionine"),aes(x=Day,y=Peak,color=Treatment),size=3)+
  geom_smooth(data = subset(aaa,compound=="Methionine"),aes(x=Day,y=Peak,color=Treatment),linewidth=0.5)+
  word_mytheme+
  scale_fill_manual(values = c("red","blue"))+
  scale_y_continuous(labels = scales::scientific,n.breaks = 5)+xlab("Time (days)")+theme(legend.position = c(0.8,0.2))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  scale_y_continuous(limits = c(0,60000))+
  theme(legend.position = "none")

library(patchwork)
(Alanine|Valine|Isoleucine|Aspartate)/(Tyrosine|Glutamate|Methionine|plot_spacer())


save.image(file = "FigureAAdynamics.Rdata")
