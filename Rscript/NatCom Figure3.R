library(ggtree)
library(ggtreeExtra)
library(treeio)
library(ggplot2)
library(ggtext)



# Genome phylogenetic tree
Desulfosporosinus_tree<-read.newick("gtdbtk.bac120.user_msa.fasta.raxml.support", node.label='support')
#Tree theme setup
tree_mytheme<-theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor =element_blank(), 
                               legend.key.size = unit(10,"pt"),axis.title=element_blank(),axis.line = element_blank(),
                               axis.text=element_blank(),
                               axis.ticks=element_blank(),
                               panel.border = element_blank(),
                               strip.background = element_blank(),
                               legend.text = element_text(size = 10,family = "Arial",face = "italic"),legend.position = c(0.1,0.8),
                               legend.title = element_blank())
# make metadata file
Desulfosporosinus_metadata<-tibble(label=as.phylo(Desulfosporosinus_tree)$tip.label,strain=as.phylo(Desulfosporosinus_tree)$tip.label)
Desulfosporosinus_metadata$strain<-Replace(Desulfosporosinus_metadata$strain, from = "_", to = " ")
#make AAI file

AAI<-subset(Total_out_aai,Label.2=="Curated_Candidatus_Desulfosporosinus.fna")
AAI<-AAI[,c(3,5)]
AAI$Label.1<-Replace(AAI$Label.1, from = ".fna", to = "")

AAI[,c("genus","Others")]<-
  str_split_fixed(AAI$Label.1, "_", 2)

colnames(AAI)<-c("id","AAI","Genus","others")
AAI$AAI<-round(AAI$AAI,1)
AAI$Genus<-Replace(AAI$Genus, from = "Candidatus", to = "Desulfosporosinus")
#merge metadata and AAI
Desulfosporosinus_metadata[,c("genus","Others")]<-
  str_split_fixed(Desulfosporosinus_metadata$strain, " ", 2)
Desulfosporosinus_metadata$genus<-Replace(Desulfosporosinus_metadata$genus, 
                                          from = "Candidatus", to = "Desulfosporosinus")

Desulfosporosinus_tree<-full_join(Desulfosporosinus_tree, 
                                  Desulfosporosinus_metadata,by="label")




root(Desulfosporosinus_tree,outgroup = "Syntrophobotulus_glycolicus_DSM_8271") -> tree1



DD1<-ggtree(tree1,aes(color=genus),show.legend=F)+
  geom_tiplab(aes(label=strain),fontface="italic",
              offset = 0.005,size=3.5,family="Arial",show.legend=F)+
  geom_nodelab(aes(label=support,subset = !is.na(as.numeric(support)) & 
                     as.numeric(support) < 90),color="black",
               size=3,show.legend=F,hjust = 1.8,vjust=-0.5)



DD2<-DD1





DD3<-facet_plot(DD2, panel='AAI',
                data = AAI,
                geom = geom_segment,aes(x=60,xend=AAI,color=Genus,
                                        y=y,yend=y,size=3))


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



DD5<- facet_widths(DD4, widths = c(4,1))


graph2ppt(x=DD5,file="Test",margins=c(0,0,0,0),upscale=T,
          append=T,width=14,height=10)

# nosZ based phylogenetic tree


nosZ_tree<-read.newick("Guang_added_nosZ.fasta.raxml.support", node.label='support')



nosZ_metadata<-NosZ_name[,c(1,2)]



colnames(nosZ_metadata)<-c("label","Species")
nosZ_metadata$Species<-Replace(nosZ_metadata$Species, from = " GN", to = "")


nosZ_metadata[,c("Genus","Spec","Others")]<-
  str_split_fixed(nosZ_metadata$Species,  " ", 3)
library(treeio)
nosZ_metadata<-unite(nosZ_metadata,"Genus Spec",Genus,Spec, sep=" ")
nosZ_metadata<-nosZ_metadata[,c(1,3)]
colnames(nosZ_metadata)<-c("label","Species")



Annotate_nosZ_tree<-full_join(nosZ_tree,nosZ_metadata,by="label")

root(Annotate_nosZ_tree,outgroup = "tr|Q5V4X2|Q5V4X2_HALMA") -> Annotate_nosZ_tree

root(nosZ_tree,outgroup = "tr|Q5V4X2|Q5V4X2_HALMA") -> nosZ_tree

nosZtree_label<-as_tibble(nosZ_tree)[c(0:181),c(4,5)]
metadata<-full_join(nosZtree_label,NosZ_name)
metadata$Genus<-ifelse(grepl("Brady",metadata$label),"Bradyrhizobium",metadata$Genus)
metadata$Genus<-ifelse(is.na(metadata$Genus),metadata$label,metadata$Genus)
metadata$clade<-ifelse(metadata$label %in% get_taxa_name(circular_nosZ,node=237), "Clade II", "Clade I")
metadata$clade<-ifelse(metadata$label %in% get_taxa_name(circular_nosZ,node=311)|
                         metadata$label %in% get_taxa_name(circular_nosZ,node=308), "Archaea NosZ", metadata$clade)
metadata$clade<-ifelse(metadata$Genus=="Haloarcula", "Archaea NosZ", metadata$clade)

metadata$mark<-ifelse(metadata$Genus=="Exhalaribacter", "Ca. Desulfosporosinus nitrousreducens", NA)
metadata$mark<-ifelse(metadata$Genus=="Desulfosporosinus", "Desulfosporosinus meridiei", metadata$mark)

root(nosZ_tree,outgroup = "tr|U8MLX6|U8MLX6_PSEAI") -> nosZ_tree

nn<-ggtree(nosZ_tree,show.legend=F) %<+% NosZ_name + xlim_tree(9)+
  geom_tiplab(aes(label=Species),fontface="italic",
              offset = 0.005,size=3.5,family="Arial",show.legend=F)+
  scale_color_manual(breaks = c("acidic","alkaline"),values = c("red","blue"))+
geom_nodelab(aes(label=support,subset = !is.na(as.numeric(support)) & 
                   as.numeric(support) < 90),color="black",
             size=3,show.legend=F,hjust = 1.8,vjust=-0.5)+
  geom_text(aes(label=node))

viewClade(nn,238)


get_taxa_name(nn,node=238)

nnnosZ_tree<-nn%>% collapse(node=307)%>% collapse(node=236)+
  geom_point2(aes(subset=(node==307)),show.legend = F)+
  geom_point2(aes(subset=(node==236)),show.legend = F)+
  geom_point2(aes(subset=(node==247)),show.legend = F)+
  geom_treescale(0,-0.5,linesize = 0.5,fontsize = 3)


library(ape)
library(treeio)
library(ggplot2)
library(ggtree)
library(ggtreeExtra)



subset_clade2<-ggtree(tree_subset(nosZ_tree,87,levels_back = 4))%<+% NosZ_name + xlim_tree(9)+
  geom_tiplab(aes(label=Species),fontface="italic",
              offset = 0.005,size=3.5,family="Arial",show.legend=F)+
  scale_color_manual(breaks = c("acidic","alkaline"),values = c("red","blue"))+
  geom_nodelab(aes(label=support,subset = !is.na(as.numeric(support)) & 
                     as.numeric(support) < 90),color="black",
               size=3,show.legend=F,hjust = 1.8,vjust=-0.5)+
  geom_text(aes(label=node))


tree_tip_taxa<-full_join(nosZtree_label,NosZ_name,by="label")
tree_tip_taxa$tip<-rownames(tree_tip_taxa)




label_clade2<-ggtree(tree_subset(nosZ_tree,87,levels_back = 4)) %<+% NosZ_name + xlim_tree(9)+
  geom_tiplab(aes(label=Species),fontface="italic",
              offset = 0.005,size=3.5,family="Arial",show.legend=F)+
  scale_color_manual(breaks = c("acidic","alkaline"),values = c("red","blue"))+
  geom_text(aes(label=node))

to_drop1 <- get_taxa_name(label_clade2,82)


to_drop2<-to_drop1[to_drop1 !="tr|F8EB68|F8EB68_RUNSL"&
                     to_drop1 !="tr|C1A867|C1A867_GEMAT" ]

Anaero_to_drop<-get_taxa_name(label_clade2,136)

Anaero_to_drop<-Anaero_to_drop[Anaero_to_drop !="tr|B8J545|B8J545_ANAD2"]

Salini_to_drop<-"tr|D5H5K0|D5H5K0_SALRM"

Desulfi_to_drop<-get_taxa_name(label_clade2,76)
Desulfi_to_drop<-Desulfi_to_drop[Desulfi_to_drop !="tr|L0F3H3|L0F3H3_DESDL"]

Geobacilllus_to_drop<-"tr|S5ZD70|S5ZD70_9BACI"

Nitratiruptor_to_drop<-"Nitratiruptor"

Sulfurimona_to_drop<-"tr|E0URE5|E0URE5_SULAO"

Camp_to_drop<-get_taxa_name(label_clade2,126)
Camp_to_drop<-Camp_to_drop[Camp_to_drop !="tr|A7H0S2|A7H0S2_CAMC5"]

Rhodo_to_drop<-"tr|G2SK29|G2SK29_RHOMR"
Pyro_to_drop<-get_taxa_name(label_clade2,133)

Halo_to_drop1<-get_taxa_name(label_clade4,247)
Halo_to_drop2<-get_taxa_name(label_clade4,245)
Halo_to_drop3<-c(Halo_to_drop1,Halo_to_drop2)
Halo_to_drop<-Halo_to_drop3[Halo_to_drop3 !="tr|Q5V4X2|Q5V4X2_HALMA"&
                              Halo_to_drop3 !="tr|B9LN21|B9LN21_HALLT" ]

Azospira_to_drop<-"tr|G8QIC8|G8QIC8_AZOSU"
Sulfurimonas_to_drop<-"tr|Q30R05|Q30R05_SULDN"


total_to_drop<-c(to_drop2,Anaero_to_drop,Salini_to_drop,Desulfi_to_drop,Geobacilllus_to_drop,
                 Nitratiruptor_to_drop,Sulfurimona_to_drop,Camp_to_drop,
                 Pyro_to_drop,Rhodo_to_drop,Halo_to_drop,Azospira_to_drop,Sulfurimonas_to_drop
                )
subset_tree<-drop.tip(tree_subset(nosZ_tree,87,levels_back = 4),total_to_drop)

root(subset_tree,outgroup = "tr|C1A867|C1A867_GEMAT") -> subset_tree

label_clade4<-ggtree(subset_tree) %<+% Heat_nosZ + xlim_tree(9)+
  geom_tiplab(aes(label=Species),fontface="italic",
              offset = 0.005,size=5.5,family="Arial",show.legend=F)+
  scale_color_manual(breaks = c("acidic","alkaline"),values = c("red","blue"))

label_clade5<-flip(label_clade4,29,50)

heat<-as.data.frame(Identity)
heat$label<-rownames(heat)
heat<-melt(heat)
Heat_nosZ<-full_join(heat,NosZ_name,by="label")
Heat_nosZ$value2<-ifelse(Heat_nosZ$variable=="Ca_Desulfosporosinus",
                         Heat_nosZ$value,NA)
Heat_nosZ$value3<-ifelse(Heat_nosZ$variable=="tr|J7IU03|J7IU03_DESMD",
                         Heat_nosZ$value,NA)

label_clade6<-flip(label_clade5,3,2)+
  geom_nodelab(aes(label=support,subset = !is.na(as.numeric(support)) & 
                     as.numeric(support) < 90),color="black",
               size=3,show.legend=F,hjust = 1.8,vjust=-0.5)+
  geom_treescale()



library(reshape2)

#heatmap for identity

Identity<-as.matrix(nosZ_similarity[,c(2:3)])
rownames(Identity)<-nosZ_similarity$...1
Identity[is.na(Identity)]<-100

library(ComplexHeatmap)
library(colorRamp2)
col_fun = colorRamp2(c(0,50,90), c("black", "blue","green"))

Heat<-Heatmap(Identity,col=col_fun,show_row_names=F,  show_column_names = F,
width = 0.1,height = 0.1,
              cluster_rows = F,
              cell_fun = function(j, i, x, y, width, height, fill) {
                grid.text(sprintf("%.1f", Identity[i, j]), x, y, gp = gpar(fontsize = 10))
              },
              column_order = order(as.factor(c("Ca_Desulfosporosinus",
                                             "D. meridiei"))))
plot_list(label_clade6,Heat)


mapping = aes(xmin = start, xmax = end, fill = gene, forward = orientation)


my_pal <- colorRampPalette(rev(brewer.pal(n = 10, name = "Set3")))

#tree + gene maps + msa

library(gggenes)
label_clade6   

dummies <- make_alignment_dummies(
  Total_rast, aes(xmin = start, xmax = end, y = label, id = gene),
  on = "nosZ" )
dummies$orientation<-1



Total_rast$label <- reorder(Total_rast$label , match(Total_rast$label , label_clade6$data$label))



nos_structure<-ggplot(Total_rast,aes(xmin=start,xmax=end,y=label,forward=orientation)) +
  geom_gene_arrow(aes(fill=definition),color="black",
                  arrow_body_height = unit(4,"mm"),
                  arrowhead_height = unit(8, "mm"), 
                  arrowhead_width = unit(2, "mm")   )  +
  geom_gene_label(aes(label=gene),align = "centre",grow = T,
                  height = grid::unit(12, "mm")) +
  facet_wrap(~label,scales = "free",ncol = 1)+
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
  




  nosZ_AAI<- gheatmap(label_clade6,Identity,offset=5,width = 0.25,colnames_angle=45,
         low="white",high="blue")
  
write.csv(Heat_nosZ,"similarity.csv")
library(export)
graph2ppt(x=nosZ_AAI,file="Figure4",margins=c(0,0,0,0),upscale=T,
          append=T,width=18,height=10)

graph2ppt(x=nos_structure,file="Figure4",margins=c(0,0,0,0),upscale=T,
          append=T,width=18,height=10)


label_clade4<-ggtree(drop.tip(nosZ_tree,total_to_drop)) %<+% NosZ_name + xlim_tree(9)+
  geom_tiplab(aes(label=Species),fontface="italic",align = T,
              offset = 0.005,size=5.5,family="Arial",show.legend=F)+
  scale_color_manual(breaks = c("acidic","alkaline"),values = c("red","blue"))+
  geom_text(aes(label=node))

label_clade5<-flip(viewClade(label_clade4,187),189,192)
label_clade6<-flip(label_clade5,57,196)
label_clade7<-flip(label_clade6,190,54)




get_taxa_name(label_clade4,187)

library(ape)
# circular nosZ tree

circular_nosZ<-ggtree(nosZ_tree,layout="circular",branch.length = "none",show.legend=F) %<+% metadata + xlim_tree(9)+
  geom_fruit(aes(fill=clade,color=clade),geom = geom_tile,show.legend=F)+
  geom_fruit(aes(starshape=mark,fill=mark),geom=geom_star,show.legend=F,offset = 0.1,size=2)+
  scale_fill_manual(breaks = c("Clade I","Clade II","Archaea NosZ",
                               "Ca. Desulfosporosinus nitrousreducens","Desulfosporosinus meridiei"),
                    values = c("green","blue","red","black","black"))+
  scale_color_manual(breaks = c("Clade I","Clade II","Archaea NosZ",
                               "Ca. Desulfosporosinus nitrousreducens","Desulfosporosinus meridiei"),
                    values = c("green","blue","red","black","black"))+
  scale_starshape_manual(breaks=c("Ca. Desulfosporosinus nitrousreducens","Desulfosporosinus meridiei"),
                         values = c(28,15))



get_taxa_name(circular_nosZ,node=128)


Figure3<-DD5|plot_spacer()|OD_plot

Figure3<-DD5|circular_nosZ


library(export)
library(patchwork)

graph2ppt(x=label_clade6,file="Figure4",margins=c(0,0,0,0),upscale=T,
          append=T,width=18,height=10)

save.image(file = "PNAS_main_figure3.Rdata")


