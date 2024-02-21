install.packages("gggenes")
library(gggenes)
library(ggplot2)
library(do)
library(ggalluvial)
library(export)
library(ggrepel)

# Import blat file
Anaeromy_blast<-Anaeromyxobacter_dehalogenans_2CP.1.blast
colnames(Anaeromy_blast)<-c("qseqid","sseqid","pident","length","mismatch","gapopen","qstart","qend","sstart","send","evalue","bitscore")
# Import ID file
Anaeromy_gene<-Anaeromyxobacter_dehalogenans_2CP.1.blast.txt.gene
colnames(Anaeromy_gene)<-c("sseqid","gene")

Anaeromy_annotation<-Anaeromyxobacter_dehalogenans_2CP_1_blast_txt_gene[,c(1,5)]
colnames(Anaeromy_annotation)<-c("sseqid","protein")
Anaeromy_gene_mapped<-join_inner(Anaeromy_blast,Anaeromy_gene,by="sseqid")
Anaeromy_gene_mapped<-join_inner(Anaeromy_gene_mapped,Anaeromy_annotation,by="sseqid")

# Import location file 
Anaeromy_location<-Anaeromyxobacter_dehalogenans_2CP.1.fasta.gff.gff.txt.location[,c(1,4,5,7,10)]
colnames(Anaeromy_location)<-c("NCBI","Start","End","Strand","qseqid")

Anaeromy_gene_cluster<-join_inner(Anaeromy_gene_mapped,Anaeromy_location,by="qseqid")
Anaeromy_gene_cluster$Genome<-"Anaeromyxobacter dehalogenans strain 2CP-1"
write.csv(Anaeromy_gene_cluster,"Anaeromy_gene_cluster.csv")



ggplot(Anaeromy_nos,aes(xmin=Start,xmax=End,y=Genome,fill=gene))+ 
  geom_gene_arrow() +geom_text(mapping=aes(x=(Start+End)/2,y=Genome,label=gene))

# Import blat file
Alicycli_blast<-Alicycliphilus_denitrificans_I51.blast
colnames(Alicycli_blast)<-c("qseqid","sseqid","pident","length","mismatch","gapopen","qstart","qend","sstart","send","evalue","bitscore")
# Import ID file
Alicycli_gene<-Alicycliphilus_denitrificans_I51.blast.txt.gene
colnames(Alicycli_gene)<-c("sseqid","gene")
Alicycli_annotation<-Alicycliphilus_denitrificans_I51_blast_txt_gene[,c(1,5)]
colnames(Alicycli_annotation)<-c("sseqid","protein")
Alicycli_gene_mapped<-join_inner(Alicycli_blast,Alicycli_gene,by="sseqid")
Alicycli_gene_mapped<-join_inner(Alicycli_gene_mapped,Alicycli_annotation,by="sseqid")

# Import location file 
Alicycli_location<-Alicycliphilus_denitrificans_I51.fasta.gff.gff.txt.location[,c(1,4,5,7,10)]
colnames(Alicycli_location)<-c("NCBI","Start","End","Strand","qseqid")

Alicycli_gene_cluster<-join_inner(Alicycli_gene_mapped,Alicycli_location,by="qseqid")
Alicycli_gene_cluster$Genome<-"Alicycliphilus denitrificans I51"
write.csv(Alicycli_gene_cluster,"Alicycli_gene_cluster.csv")


ggplot(Alicycli_nos,aes(xmin=Start,xmax=End,y=Genome,fill=gene))+ 
  geom_gene_arrow() +geom_text(mapping=aes(x=(Start+End)/2,y=Genome,label=gene))


# Import blat file
Brady_USDA110_blast<-Bradyrhizobium_diazoefficiens_USDA_110.blast
colnames(Brady_USDA110_blast)<-c("qseqid","sseqid","pident","length","mismatch","gapopen","qstart","qend","sstart","send","evalue","bitscore")
# Import ID file
Brady_USDA110_gene<-Bradyrhizobium_diazoefficiens_USDA_110.blast.txt.gene
colnames(Brady_USDA110_gene)<-c("sseqid","gene")
Brady_USDA110_annotation<-Bradyrhizobium_diazoefficiens_USDA_110_blast[,c(1,5)]
colnames(Brady_USDA110_annotation)<-c("sseqid","protein")
Brady_USDA110_gene_mapped<-join_inner(Brady_USDA110_blast,Brady_USDA110_gene,by="sseqid")
Brady_USDA110_gene_mapped<-join_inner(Brady_USDA110_gene_mapped,Brady_USDA110_annotation,by="sseqid")


# Import location file 
Brady_USDA110_location<-Bradyrhizobium_diazoefficiens_USDA_110.fasta.gff.gff.txt.location[,c(1,4,5,7,10)]
colnames(Brady_USDA110_location)<-c("NCBI","Start","End","Strand","qseqid")

Brady_USDA110_gene_cluster<-join_inner(Brady_USDA110_gene_mapped,Brady_USDA110_location,by="qseqid")
Brady_USDA110_gene_cluster$Genome<-"Bradyrhizobium diazoefficiens strain USDA110"
write.csv(Brady_USDA110_gene_cluster,"Brady_USDA110_gene_cluster.csv")




ggplot(Brady_USDA110_nos,aes(xmin=Start,xmax=End,y=Genome,fill=gene))+ 
  geom_gene_arrow() +geom_text(mapping=aes(x=(Start+End)/2,y=Genome,label=gene))


# Import blat file
Brady_spc4_blast<-Bradyrhizobium_diazoefficiens_strain_110spc4.blast
colnames(Brady_spc4_blast)<-c("qseqid","sseqid","pident","length","mismatch","gapopen","qstart","qend","sstart","send","evalue","bitscore")
# Import ID file
Brady_spc4_gene<-Bradyrhizobium_diazoefficiens_strain_110spc4.blast.txt.gene
colnames(Brady_spc4_gene)<-c("sseqid","gene")
Brady_spc4_annotation<-Bradyrhizobium_diazoefficiens_USDA_110_blast[,c(1,5)]
colnames(Brady_spc4_annotation)<-c("sseqid","protein")
Brady_spc4_gene_mapped<-join_inner(Brady_spc4_blast,Brady_spc4_gene,by="sseqid")
Brady_spc4_gene_mapped<-join_inner(Brady_spc4_gene_mapped,Brady_spc4_annotation,by="sseqid")


# Import location file 
Brady_spc4_location<-Bradyrhizobium_diazoefficiens_strain_110spc4.fasta.gff.gff.txt.location[,c(1,4,5,7,10)]
colnames(Brady_spc4_location)<-c("NCBI","Start","End","Strand","qseqid")

Brady_spc4_gene_cluster<-join_inner(Brady_spc4_gene_mapped,Brady_spc4_location,by="qseqid")
Brady_spc4_gene_cluster$Genome<-"Bradyrhizobium diazoefficiens strain 110spc4"
write.csv(Brady_spc4_gene_cluster,"Brady_spc4_gene_cluster.csv")




ggplot(Brady_spc4_nos,aes(xmin=Start,xmax=End,y=Genome,fill=gene))+ 
  geom_gene_arrow() +geom_text(mapping=aes(x=(Start+End)/2,y=Genome,label=gene))


# Import blat file
Nitratiruptor_blast<-Nitratiruptor_labii_HRV44.blast
colnames(Nitratiruptor_blast)<-c("qseqid","sseqid","pident","length","mismatch","gapopen","qstart","qend","sstart","send","evalue","bitscore")
# Import ID file
Nitratiruptor_gene<-Nitratiruptor_labii_HRV44.blast.txt.gene
colnames(Nitratiruptor_gene)<-c("sseqid","gene")
Nitratiruptor_annotation<-Nitratiruptor_labii_HRV44_blast_txt_gene[,c(1,5)]
colnames(Nitratiruptor_annotation)<-c("sseqid","protein")
Nitratiruptor_gene_mapped<-join_inner(Nitratiruptor_blast,Nitratiruptor_gene,by="sseqid")
Nitratiruptor_gene_mapped<-join_inner(Nitratiruptor_gene_mapped,Nitratiruptor_annotation,by="sseqid")

# Import location file 
Nitratiruptor_location<-Nitratiruptor_labii_HRV44.fasta.gff.gff.txt.location[,c(1,4,5,7,10)]
colnames(Nitratiruptor_location)<-c("NCBI","Start","End","Strand","qseqid")
Nitratiruptor_location$qseqid<-Replace(Nitratiruptor_location$qseqid, from = ">", to = "")

Nitratiruptor_gene_cluster<-join_inner(Nitratiruptor_gene_mapped,Nitratiruptor_location,by="qseqid")
Nitratiruptor_gene_cluster$Genome<-"Nitratiruptor labii HRV44"
write.csv(Nitratiruptor_gene_cluster,"Nitratiruptor_gene_cluster.csv")



ggplot(Nitratiruptor_nos,aes(xmin=Start,xmax=End,y=Genome,fill=gene))+ 
  geom_gene_arrow() +geom_text(mapping=aes(x=(Start+End)/2,y=Genome,label=gene))



# Import blat file
Paracoccus_blast<-Paracoccus_denitrificans_strain_DSM_413.blast
colnames(Paracoccus_blast)<-c("qseqid","sseqid","pident","length","mismatch","gapopen","qstart","qend","sstart","send","evalue","bitscore")
# Import ID file
Paracoccus_gene<-Paracoccus_denitrificans_strain_DSM_413.blast.txt.gene
colnames(Paracoccus_gene)<-c("sseqid","gene")
Paracoccus_annotation<-Paracoccus_denitrificans_strain_DSM_413_blast[,c(1,5)]
colnames(Paracoccus_annotation)<-c("sseqid","protein")
Paracoccus_gene_mapped<-join_inner(Paracoccus_blast,Paracoccus_gene,by="sseqid")
Paracoccus_gene_mapped<-join_inner(Paracoccus_gene_mapped,Paracoccus_annotation,by="sseqid")



# Import location file 
Paracoccus_location<-Paracoccus_denitrificans_strain_DSM_413.fasta.gff.gff.txt.location[,c(1,4,5,7,10)]
colnames(Paracoccus_location)<-c("NCBI","Start","End","Strand","qseqid")
Paracoccus_location$qseqid<-Replace(Paracoccus_location$qseqid, from = ">", to = "")

Paracoccus_gene_cluster<-join_inner(Paracoccus_gene_mapped,Paracoccus_location,by="qseqid")
Paracoccus_gene_cluster$Genome<-"Paracoccus denitrificans strain DSM 413"

write.csv(Paracoccus_gene_cluster,"Paracoccus_gene_cluster.csv")


ggplot(Paracoccus_nos,aes(xmin=Start,xmax=End,y=Genome,fill=gene))+ 
  geom_gene_arrow() +geom_text(mapping=aes(x=(Start+End)/2,y=Genome,label=gene))





# Import blat file
Shewanella_blast<-Shewanella_loihica_PV.4.blast
colnames(Shewanella_blast)<-c("qseqid","sseqid","pident","length","mismatch","gapopen","qstart","qend","sstart","send","evalue","bitscore")
# Import ID file
Shewanella_gene<-Shewanella_loihica_PV.4.blast.txt.gene
colnames(Shewanella_gene)<-c("sseqid","gene")
Shewanella_annotation<-Shewanella_loihica_PV_4_blast[,c(1,5)]
colnames(Shewanella_annotation)<-c("sseqid","protein")
Shewanella_gene_mapped<-join_inner(Shewanella_blast,Shewanella_gene,by="sseqid")
Shewanella_gene_mapped<-join_inner(Shewanella_gene_mapped,Shewanella_annotation,by="sseqid")



# Import location file 
Shewanella_location<-Shewanella_loihica_PV.4.fasta.gff.gff.txt.location[,c(1,4,5,7,10)]
colnames(Shewanella_location)<-c("NCBI","Start","End","Strand","qseqid")


Shewanella_gene_cluster<-join_inner(Shewanella_gene_mapped,Shewanella_location,by="qseqid")
Shewanella_gene_cluster$Genome<-"Shewanella loihica PV-4,"
write.csv(Shewanella_gene_cluster,"Shewanella_gene_cluster.csv")


Shewanella_nos<-subset(Shewanella_gene_cluster, Start >=4046800 & End <= 4054735)

ggplot(Shewanella_nos,aes(xmin=Start,xmax=End,y=Genome,fill=gene))+ 
  geom_gene_arrow() +geom_text(mapping=aes(x=(Start+End)/2,y=Genome,label=gene))










# Candidate_CSC1_unidirection gene cluster
# Import blat file
Candidate_CSC1_blast<-Curated_Candidatus_Desulfosporosinus.blast
colnames(Candidate_CSC1_blast)<-c("qseqid","sseqid","pident","length","mismatch","gapopen","qstart","qend","sstart","send","evalue","bitscore")
# Import ID file
Candidate_CSC1_gene<-Curated_Candidatus_Desulfosporosinus.blast.txt.gene
colnames(Candidate_CSC1_gene)<-c("sseqid","gene")

Candidate_CSC1_annotation<-Curated_Candidatus_Desulfosporosinus_blast[,c(1,5)]
colnames(Candidate_CSC1_annotation)<-c("sseqid","protein")
Candidate_CSC1_gene_mapped<-join_inner(Candidate_CSC1_blast,Candidate_CSC1_gene,by="sseqid")
Candidate_CSC1_gene_mapped<-join_inner(Candidate_CSC1_gene_mapped,Candidate_CSC1_annotation,by="sseqid")



# Import location file 
Candidate_CSC1_location<-Curated_Candidatus_Desulfosporosinus.fasta.gff.gff.txt.location[,c(1,4,5,7,10)]
colnames(Candidate_CSC1_location)<-c("NCBI","Start","End","Strand","qseqid")

Candidate_CSC1_gene_cluster<-join_inner(Candidate_CSC1_gene_mapped,Candidate_CSC1_location,by="qseqid")
Candidate_CSC1_gene_cluster$Genome<-"Candidate_CSC1"
write.csv(Candidate_CSC1_gene_cluster,"Candidate_CSC1_gene_cluster.csv")


Candidate_CSC1_nos<-subset(Candidate_CSC1_gene_cluster, Start >=309060 & End <= 314700 & Strand == "+")


ggplot(Candidate_CSC1_nos,aes(xmin=Start,xmax=End,y=Genome,fill=gene))+ 
  geom_gene_arrow() +geom_text(mapping=aes(x=(Start+End)/2,y=Genome,label=gene))

# Candidate_CSC1_bidirection gene cluster
# Import blat file
Candidate_CSC1_blast<-Curated_Candidatus_Desulfosporosinus.blast
colnames(Candidate_CSC1_blast)<-c("qseqid","sseqid","pident","length","mismatch","gapopen","qstart","qend","sstart","send","evalue","bitscore")
# Import ID file
Candidate_CSC1_gene<-Curated_Candidatus_Desulfosporosinus.blast.txt.gene
colnames(Candidate_CSC1_gene)<-c("sseqid","gene")
Candidate_CSC1_gene_mapped<-join_inner(Candidate_CSC1_blast,Candidate_CSC1_gene,by="sseqid")

# Import location file 
Candidate_CSC1_location<-Curated_Candidatus_Desulfosporosinus.fasta.gff.gff.txt.location[,c(1,4,5,7,10)]
colnames(Candidate_CSC1_location)<-c("NCBI","Start","End","Strand","qseqid")

Candidate_CSC1_gene_cluster<-join_inner(Candidate_CSC1_gene_mapped,Candidate_CSC1_location,by="qseqid")
Candidate_CSC1_gene_cluster$Genome<-"Candidate_CSC1"
Candidate_CSC1_nos<-subset(Candidate_CSC1_gene_cluster, Start >=309060 & End <= 314700)
Candidate_CSC1_nos$direction<-ifelse(Candidate_CSC1_nos$Strand == "+", 1,-1)




# Nos operon and acceossary genes
Alicycli_nos<-subset(Alicycli_gene_cluster[c(3277:3284),])
Anaeromy_nos<-subset(Anaeromy_gene_cluster[c(1638:1648),])
Brady_spc4_nos<-subset(Brady_spc4_gene_cluster[c(237:244),])
Brady_110_nos<-subset(Brady_USDA110_gene_cluster[c(316:323),])

Candidate_nos<-subset(Candidate_CSC1_gene_cluster[c(5333:5351),])
Nitrati_nos<-subset(Nitratiruptor_gene_cluster[c(868:885),])
Paracoccus_nos<-subset(Paracoccus_gene_cluster[c(3432:3439),])
Shewanella_nos<-subset(Shewanella_gene_cluster[c(3567:3572),])

Total_nos<-rbind(Alicycli_nos,Anaeromy_nos,Brady_spc4_nos,Brady_110_nos,
                 Candidate_nos,Nitrati_nos,Paracoccus_nos, Shewanella_nos)
Total_nos$direction<-ifelse(Total_nos$Strand == "+", 1,-1)

Total_nos<- within(Total_nos, 
                          Genome <- factor(Genome, levels = c("Candidate_CSC1",
                                                              "Nitratiruptor labii HRV44",
                                                              "Anaeromyxobacter dehalogenans strain 2CP-1",
                                                              "Alicycliphilus denitrificans I51",
                                                              "Bradyrhizobium diazoefficiens strain USDA110",
                                                              "Paracoccus denitrificans strain DSM 413",
                                                              "Shewanella loihica PV-4,",
                                                              "Bradyrhizobium diazoefficiens strain 110spc4")))

with(Total_nos, levels(Genome))



write_xlsx(Total_nos,file="Total_nos.xlsx")


dummies <- make_alignment_dummies(
  Total_nos, aes(xmin = Start, xmax = End, y = Genome, id = gene),
  on = "nosZ" )


ggplot(Total_nos,aes(xmin=Start,xmax=End,y=Genome,fill=gene)) +
  geom_gene_arrow()  +
  geom_gene_label(aes(label=gene),align = "left",grow = T) +
  facet_wrap(~Genome,scales = "free",ncol = 1)+
  geom_blank(data = dummies)+theme_genes()+
  theme(axis.title.x=element_blank(),axis.line.x = element_blank(),
        axis.text.x=element_blank(),axis.text.y = element_text(size=10),
        axis.ticks.x=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor =element_blank(), 
        legend.key.size = unit(10,"pt"),
        strip.text.x = element_blank(),
        panel.border = element_blank(),panel.spacing=unit(0.3,"lines"),
        strip.background = element_blank())+ylab("Genome")



# RAST annotation based cluster
Anaeromyxo_RAST<-Anaeromyxobacter[,c(4:9)]
Anaeromyxo_RAST$Genome<-"Anaeromyxobacter dehalogenans strain 2CP-1"
Anaeromyxo_RAST<-Anaeromyxo_RAST[complete.cases(Anaeromyxo_RAST),]


BradyUSDA_RAST<-Brady_USDA110[,c(4:9)]
BradyUSDA_RAST$Genome<-"Bradyrhizobium diazoefficiens strain USDA110"
BradyUSDA_RAST<-BradyUSDA_RAST[complete.cases(BradyUSDA_RAST),]

Exhalaribacter_RAST<-Exhalaribacter[,c(4:9)]
Exhalaribacter_RAST$Genome<-"Exhalaribacter nitrousreducens"
Exhalaribacter_RAST<-Exhalaribacter_RAST[complete.cases(Exhalaribacter_RAST),]


Shewanella_RAST<-Shewanella[,c(4:9)]
Shewanella_RAST$Genome<-"Shewanella loihica PV-4"
Shewanella_RAST<-Shewanella_RAST[complete.cases(Shewanella_RAST),]

Nitratiruptor_RAST<-Nitratiruptor[,c(4:9)]
Nitratiruptor_RAST$Genome<-"Nitratiruptor labii HRV44"
Nitratiruptor_RAST<-Nitratiruptor_RAST[complete.cases(Nitratiruptor_RAST),]

Paracoccus_RAST<-Paracoccus[,c(4:9)]
Paracoccus_RAST$Genome<-"Paracoccus denitrificans strain DSM 413"
Paracoccus_RAST<-Paracoccus_RAST[complete.cases(Paracoccus_RAST),]

Alicycliphilus_RAST<-Alicycliphilus[,c(4:9)]
Alicycliphilus_RAST$Genome<-"Alicycliphilus denitrificans I51"
Alicycliphilus_RAST<-Alicycliphilus_RAST[complete.cases(Alicycliphilus_RAST),]

Brady_spc_RAST<-Brady_spc110[,c(4:9)]
Brady_spc_RAST$Genome<-"Bradyrhizobium diazoefficiens strain 110spc4"
Brady_spc_RAST<-Brady_spc_RAST[complete.cases(Brady_spc_RAST),]

Total_RAST<-rbind(Exhalaribacter_RAST,Anaeromyxo_RAST,
                  BradyUSDA_RAST,
                  Shewanella_RAST,
                  Nitratiruptor_RAST,
                  Paracoccus_RAST,
                  Alicycliphilus_RAST,
                  Brady_spc_RAST)

Total_RAST<-rbind(Total_RAST,Desulfosporosinus_meridiei)

Total_RAST$definition<-ifelse(Total_RAST$gene %in% c("nosD","nosL","nosY","nosF",
                                                "nosR","nosX"  ),
                             "nos accessory", "" )
Total_RAST$definition<-ifelse(Total_RAST$gene %in% c("nosZ"  ),
                              "Nitrous oxide reductase", Total_RAST$definition )

Total_RAST$definition<-ifelse(Total_RAST$gene %in% c("nosZ","nosD","nosL","nosY","nosF",
                                                        "nosR","nosX"   ),
                              Total_RAST$definition, Total_RAST$gene )

Total_RAST$definition<-ifelse(Total_RAST$gene %in% c("nosZ"),
                              "Nitrous oxide reductase", Total_RAST$definition )
Total_RAST<- within(Total_RAST, 
                   Genome <- factor(Genome, levels = c("Exhalaribacter nitrousreducens",
                                                       "Desulfosporosinus meridiei",
                                                       "Nitratiruptor labii HRV44",
                                                       "Anaeromyxobacter dehalogenans strain 2CP-1",
                                                       "Alicycliphilus denitrificans I51",
                                                       
                                                       "Paracoccus denitrificans strain DSM 413",
                                                       "Shewanella loihica PV-4",
                                                       "Bradyrhizobium diazoefficiens strain USDA110",
                                                       "Bradyrhizobium diazoefficiens strain 110spc4")))

with(Total_RAST, levels(Genome))


Total_RAST<-subset(Total_RAST,Genome %in% c("Exhalaribacter nitrousreducens","Nitratiruptor labii HRV44",
                                            "Anaeromyxobacter dehalogenans strain 2CP-1",
                                            "Alicycliphilus denitrificans I51",
                                            "Paracoccus denitrificans strain DSM 413",
                                            "Shewanella loihica PV-4", "Bradyrhizobium diazoefficiens strain 110spc4"))

Total_RAST$gene<-ifelse(Total_RAST$gene=="PotA","nosF",Total_RAST$gene)
Total_RAST$orientation<-ifelse(Total_RAST$strand=="+",1,-1)

library(gggenes)

dummies <- make_alignment_dummies(
  Total_RAST, aes(xmin = start, xmax = stop, y = Genome, id = gene),
  on = "nosZ" )
dummies$orientation<-1

gene_cluster<-ggplot(Total_RAST,aes(xmin=start,xmax=stop,y=Genome,forward=orientation)) +
  geom_gene_arrow(aes(fill=definition),color="black",
                  arrow_body_height = unit(4,"mm"),
                  arrowhead_height = unit(8, "mm"), 
                  arrowhead_width = unit(2, "mm")   )  +
  geom_gene_label(aes(label=gene),align = "centre",grow = T,
                  height = grid::unit(12, "mm")) +
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



  geom_label_repel(x=330000,aes(label=Genome))


write.csv(Total_RAST,file="Total_rast.csv")

save.image("cluster_rast.Rdata")


library(export)
graph2ppt(x=gene_cluster,file="Figure5",margins=c(0,0,0,0),upscale=T,
          append=T,width=14,height=10)


# gene cluster of other enrichment
library(gggenes)
nosZcluster<-subset(nosZ_data_copy, !is.na(nosZ_data_copy$gene))

nosZcluster$strand<-"forward"

nosZcluster<-nosZcluster[,c(1,2,3,13,4,5)]

dummies <- make_alignment_dummies(
  nosZcluster,
  aes(xmin = start, xmax = end, y = MAG, id = gene),
  on = "nosZ"
)

ggplot(nosZcluster,aes(xmin=start,xmax=end,y=MAG,fill=gene,label=gene)) +
  geom_gene_arrow()+
  geom_blank(data = dummies) +
  facet_wrap(~ MAG, scales = "free", ncol = 1) +
  scale_fill_brewer(palette = "Set3") +
  theme_genes() +
  geom_gene_label(align = "left")

  
  
  dummies <- make_alignment_dummies(
    example_genes,
    aes(xmin = start, xmax = end, y = molecule, id = gene),
    on = "genE"
  )
  
  ggplot(example_genes, aes(xmin = start, xmax = end, y = molecule, fill = gene)) +
    geom_gene_arrow() +
    geom_blank(data = dummies) +
    facet_wrap(~ molecule, scales = "free", ncol = 1) +
    scale_fill_brewer(palette = "Set3") +
    theme_genes()



# gene cluster comparing RCB and DN
  library(gggenes)
  
  mapping = aes(xmin = start, xmax = stop, fill = gene, forward = direction)
  
  gene_cluster$orientation<-ifelse(gene_cluster$strand=="-","reverse","forward")
  gene_cluster$direction<-ifelse(gene_cluster$strand=="-",-1, 1)
  
  ggplot(gene_cluster)+geom_motif(data=gene_cluster,mapping = mapping,on = 'nosZ')
  
  dummies <- make_alignment_dummies(
    gene_cluster_H,
    aes(xmin = start, xmax = stop, y = contig_id, id = gene,direction=direction),
    on = "nosZ"
  )
  dummies$direction <- 1
  
  gene_cluster_H<-gene_cluster[c(37:46),]
  ggplot(gene_cluster_H,aes(xmin=start,xmax=stop,y=contig_id,fill=gene,
                          label=gene,forward=direction))+
    geom_gene_arrow()+
    geom_blank(data = dummies)
  
  
