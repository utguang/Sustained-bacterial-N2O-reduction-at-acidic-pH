library(ggtree)
library(ggtreeExtra)
library(treeio)
library(ggplot2)
library(ggtext)
library(tidyverse)
library(do)

#Tree theme setup
tree_mytheme<-theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor =element_blank(), 
                               legend.key.size = unit(10,"pt"),axis.title=element_blank(),axis.line = element_blank(),
                               axis.text=element_blank(),
                               axis.ticks=element_blank(),
                               panel.border = element_blank(),
                               strip.background = element_blank(),
                               legend.text = element_text(size = 10,family = "Arial",face = "italic"),legend.position = c(0.1,0.8),
                               legend.title = element_blank())


# import Genome phylogenetic tree
Desulfosporosinus_tree<-read.newick("Fig.3.raxml.support", node.label='support')
# make metadata file
Desulfosporosinus_metadata<-tibble(label=as.phylo(Desulfosporosinus_tree)$tip.label,strain=as.phylo(Desulfosporosinus_tree)$tip.label)
Desulfosporosinus_metadata$strain<-Replace(Desulfosporosinus_metadata$strain, from = "_", to = " ")

#merge metadata and tree objective
Desulfosporosinus_metadata[,c("genus","Others")]<-
  str_split_fixed(Desulfosporosinus_metadata$strain, " ", 2)
Desulfosporosinus_metadata$genus<-Replace(Desulfosporosinus_metadata$genus, 
                                          from = "Candidatus", to = "Desulfosporosinus")

Desulfosporosinus_tree<-full_join(Desulfosporosinus_tree, 
                                  Desulfosporosinus_metadata,by="label")



# Plot genome tree
DD1<-ggtree(Desulfosporosinus_tree,aes(color=genus),show.legend=F)+
  geom_tiplab(aes(label=strain),fontface="italic",
              offset = 0.005,size=3.5,family="Arial",show.legend=F)+
  geom_nodelab(aes(label=support,subset = !is.na(as.numeric(support)) & 
                     as.numeric(support) < 90),color="black",
               size=3,show.legend=F,hjust = 1.8,vjust=-0.5)



DD2<-DD1


#make AAI data

AAI<-subset(Fig_3AAI,Label.2=="Curated_Candidatus_Desulfosporosinus.fna") # Import AAI data
AAI<-AAI[,c(4,6)] # Subset the useful data
AAI$Label.1<-Replace(AAI$Label.1, from = ".fna", to = "")

AAI[,c("genus","Others")]<-
  str_split_fixed(AAI$Label.1, "_", 2)

colnames(AAI)<-c("id","AAI","Genus","others")
AAI$AAI<-round(AAI$AAI,1)
AAI$Genus<-Replace(AAI$Genus, from = "Candidatus", to = "Desulfosporosinus")

#Append an AAI facet to the tree plot
DD3<-facet_plot(DD2, panel='AAI',
                data = AAI,
                geom = geom_segment,aes(x=60,xend=AAI,color=Genus,
                                        y=y,yend=y,size=3))

# Append the AAI values (i.e., text)
DD4<-     facet_plot(DD3, panel='AAI',
                     data = AAI,
                     geom = geom_text,aes(x=AAI+5,label=AAI,
                                          y=y,size=3,family="Times"),color="black",
                     show.legend=F)+xlim_tree(1.1)+guides(size="none")+
  theme(axis.line.x = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())+
  theme(strip.text.x = element_blank())+xlim_tree(1)+tree_mytheme+
  geom_treescale(0,-0.5,linesize = 0.5,fontsize = 3)+
  scale_color_manual(breaks = c("Candidatus","Desulfosporosinus","Desulfitobacterium","Dehalobacter","Syntrophobotulus","Curated"),
                     values = c("black","black","black","black","black","black"))+
  theme(legend.position = c(0.1,0.9))


# Adjust the tree and AAI facet scale
DD5<- facet_widths(DD4, widths = c(4,1))

#export the tree to PPT for further annotation
graph2ppt(x=DD5,file="Test",margins=c(0,0,0,0),upscale=T,
          append=T,width=14,height=10)



