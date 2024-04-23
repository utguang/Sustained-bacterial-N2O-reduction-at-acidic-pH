library(ggmsa)
library(Biostrings)
library(treeio)

#Import nosZ alignment data
nosZ_align<-read.fasta("FigS9_nosZ.aln.fasta")

# extract the CuA representing consensus motif sequences


CuA_2ndH<-seqlogo(nosZ_align, start = 999, end = 1010, color = "Chemistry_AA", font = "DroidSansMono")




graph2ppt(x=  CuA_2ndH,file="Figure",margins=c(0,0,0,0),upscale=T,
          append=T,width=14,height=10) #export to PPT


#CuZ conserved histidine: the CuZ site comprises seven discrete, but conserved histidine residues. The position information of these histidine residues was inferred from structural modelling analysis

CuZ_1H<-seqlogo(nosZ_align, start = 365, end = 370, color = "Chemistry_AA", font = "DroidSansMono")
CuZ_3H<-seqlogo(nosZ_align, start = 411, end = 417, color = "Chemistry_AA", font = "DroidSansMono")
CuZ_4H<-seqlogo(nosZ_align, start = 648, end = 653, color = "Chemistry_AA", font = "DroidSansMono")
CuZ_5H<-seqlogo(nosZ_align, start = 717, end = 720, color = "Chemistry_AA", font = "DroidSansMono")
CuZ_6H<-seqlogo(nosZ_align, start = 775, end = 778, color = "Chemistry_AA", font = "DroidSansMono")
CuZ_7H<-seqlogo(nosZ_align, start = 843, end = 848, color = "Chemistry_AA", font = "DroidSansMono")

Fig.S9<-(CuA_firstH|CuA_2ndH)/((CuZ_1H|CuZ_3H|CuZ_4H)/(CuZ_5H|CuZ_6H|CuZ_7H))

graph2ppt(x=  Fig.S9,file="Figure",margins=c(0,0,0,0),upscale=T,
          append=T,width=14,height=10)



