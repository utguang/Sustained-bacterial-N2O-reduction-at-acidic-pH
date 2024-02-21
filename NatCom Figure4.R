library(ggplot2)
library(ggtree)

ggtree(nosZ_tree,show.legend=F,branch.length='none',layout="circular") %<+% NosZ_name +
  scale_color_manual(breaks = c("acidic","alkaline"),values = c("red","blue"))+
  geom_tiplab(aes(label=Genus))+
  geom_fruit(geom = geom_tile,
             mapping = aes(fill=Genus),offset = 0.5)

save.image(file = "PNAS_figureS4.Rdata")
