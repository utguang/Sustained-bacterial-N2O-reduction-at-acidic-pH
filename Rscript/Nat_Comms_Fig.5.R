
library(gggenes)
library(ggplot2)

#make dummy file to adjust arrow orientation
dummies <- make_alignment_dummies(
  Fig_5, aes(xmin = start, xmax = stop, y = Genome, id = gene),
  on = "nosZ" )

dummies$orientation<-1

# plot gene clusters using gggenes package
gene_cluster<-ggplot(Fig_5,aes(xmin=start,xmax=stop,y=Genome,forward=orientation)) +
  geom_gene_arrow(aes(fill=definition),color="black",
                  arrow_body_height = unit(4,"mm"),
                  arrowhead_height = unit(8, "mm"), 
                  arrowhead_width = unit(2, "mm")   )  +
  geom_gene_label(aes(label=gene),align = "centre",grow = T,
                  height = grid::unit(4, "mm")) +
  facet_wrap(~Genome,scales = "free",ncol = 1)+
  geom_blank(data = dummies)+
  scale_fill_manual(breaks = c("Nitrous oxide reductase",
                               "nos accessory",
                               "cy-c",
                               "cy-b",
                               "S",
                               "TM",
                               "PotA","unknown protein","Fe-S","HP"
  ),
  values = c("green","grey","red","orange","purple",
             "cyan","magenta","white","yellow","black"))+theme_bw()+
  theme(axis.title.x=element_blank(),axis.line.x = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),,axis.title.y = element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor =element_blank(), 
        legend.key.size = unit(10,"pt"),
        strip.text.x = element_blank(),legend.position = "none",
        panel.border = element_blank(),panel.spacing=unit(0.1,"lines"),
        strip.background = element_blank())+ylab("Genome")

graph2ppt(x=gene_cluster,file="Figure5",margins=c(0,0,0,0),upscale=T,
          append=T,width=14,height=10)
#export ggplot objective to PPT for further dereplication of gene clusters with same architecture.



