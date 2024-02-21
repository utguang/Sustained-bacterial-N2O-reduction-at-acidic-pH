# Load ggplot2 library
library(patchwork)
library(ggplot2)



pH_experiments<-subset(pH_dependency_temperal_inoculation,!(pH %in%c("3.5","8")))

N2O<-ggplot(pH_experiments,aes(Day,Substrate))+
  geom_point(data = pH_experiments,aes(Day,Substrate,color=as.character(pH)),size=3,show.legend = F)+
  geom_line(data = pH_experiments,aes(Day,Substrate,group=Group,color=as.character(pH)),show.legend = F)+
  word_mytheme+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  geom_errorbar(aes(ymin=Substrate-Error,ymax=Substrate+Error,color=as.character(pH)),width=0.8,show.legend = F)+
scale_y_continuous(breaks = c(0,200,400))+
  scale_color_manual(breaks = c("4.5","5","6","7"),
                     values = c("magenta","green","blue","red"))

pyruvate_rate<-pH_dependency_temperal_inoculation[,c(1,3,11)]

rate<-ggplot(pyruvate_rate, aes(x = as.character(Treatment), y = rate)) +
  geom_boxplot( color = "black", width = 1,aes(group=Treatment)) +
  geom_point(aes(x = as.character(Treatment), y = rate), color = "black", size = 1,
             position = position_jitter(width = 0.1,height = 0.2))  +
  word_mytheme+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  scale_x_discrete(breaks = c(4.5,5,6,7,8))


N2O_rate<-pH_dependency_temperal_inoculation

N_rate<-ggplot(N2O_rate, aes(x = as.character(Treatment), y = rate)) +
  geom_boxplot( color = "black", width = 0.5,aes(group=Treatment)) +
  geom_point(aes(x = as.character(Treatment), y = rate), color = "black", size = 1,
             position = position_jitter(width = 0.1,height = 0.2))  +
  word_mytheme+theme(axis.title.y = element_blank())+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  scale_x_discrete(breaks = c(4.5,5,6,7,8))+ylim(15,30)



FigureS2<-(N2O|rate|N_rate)+plot_layout(ncol = 3,widths = c(1,1,1))


library(export)
graph2ppt(x=FigureS2,file="FigureS7",margins=c(0,0,0,0),upscale=T,
          append=T,width=14,height=7.5)


save.image(file = "PNAS_supplemental_figureS2.Rdata")
