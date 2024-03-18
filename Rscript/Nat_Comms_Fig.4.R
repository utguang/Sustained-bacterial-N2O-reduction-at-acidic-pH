
# Import the subset tree extracted from nosZ tree comprising 174 nosZ sequence.
subset_tree<-read.jtree("Fig4.nosZtree")

#reroot tree
root(subset_tree,outgroup = "tr|C1A867|C1A867_GEMAT") -> subset_tree

#plot tree
label_clade4<-ggtree(subset_tree) +geom_text(aes(label=node))
# make a flip of branches
label_clade5<-flip(ggtree(subset_tree) ,34,31)

# Import metadata and nosZ amino acid identity data
NosZ_name<-Fig_4AI # import the Fig_4AI NosZ_name sheet

nosZ_similarity<-Fig_4AI #import the Fig_4AI nosZ AI sheet
Identity<-as.matrix(nosZ_similarity[,c(2:3)])
rownames(Identity)<-nosZ_similarity$...1
Identity[is.na(Identity)]<-100

# Make the heatmap annotation data
heat<-as.data.frame(Identity)
heat$label<-rownames(heat)
heat<-melt(heat)
Heat_nosZ<-full_join(heat,NosZ_name,by="label")
Heat_nosZ$value2<-ifelse(Heat_nosZ$variable=="Ca_Desulfosporosinus",
                         Heat_nosZ$value,NA)
Heat_nosZ$value3<-ifelse(Heat_nosZ$variable=="tr|J7IU03|J7IU03_DESMD",
                         Heat_nosZ$value,NA)

label_clade6<-flip(label_clade5,5,6)+
  geom_nodelab(aes(label=support,subset = !is.na(as.numeric(support)) & 
                     as.numeric(support) < 90),color="black",
               size=3,show.legend=F,hjust = 1.8,vjust=-0.5)+
  geom_treescale()

label_clade7<-label_clade6%<+% Heat_nosZ + xlim_tree(9)+
  geom_tiplab(aes(label=Species),fontface="italic",
              offset = 0.005,size=5.5,family="Arial",show.legend=F)+
  scale_color_manual(breaks = c("acidic","alkaline"),values = c("red","blue"))

nosZ_AAI<- gheatmap(label_clade7,Identity,offset=5,width = 0.25,colnames_angle=45,
                    low="white",high="blue") # export tree and AI data to PPT for further annotation

