library(ggmsa)
library(Biostrings)
library(treeio)

#CuA motif
nosZ_align<-read.fasta("Guang_added_nosZ.fasta")


CuA_firstH<-seqlogo(nosZ_align, start = 962, end = 965, color = "Chemistry_AA", font = "DroidSansMono")
dummy <- subset(CuA_firstH$layers[[1]]$data)

CuA_firstH<-CuA_firstH+ 
  geom_rect(data = dummy, mapping = aes(xmin =961.5, xmax = 962.5,ymin = -Inf, ymax = Inf),
            alpha=0.2, 
            color = "red", 
            size = 0.5, 
            fill = NA)

CuA_2ndH<-seqlogo(nosZ_align, start = 999, end = 1010, color = "Chemistry_AA", font = "DroidSansMono")


CuA_firstH|CuA_2ndH

graph2ppt(x=  CuA_firstH|CuA_2ndH,file="Figure",margins=c(0,0,0,0),upscale=T,
          append=T,width=14,height=10)
#CuZ conserved histidine
CuZ_1H<-seqlogo(nosZ_align, start = 365, end = 370, color = "Chemistry_AA", font = "DroidSansMono")
CuZ_3H<-seqlogo(nosZ_align, start = 411, end = 417, color = "Chemistry_AA", font = "DroidSansMono")
CuZ_4H<-seqlogo(nosZ_align, start = 648, end = 653, color = "Chemistry_AA", font = "DroidSansMono")
CuZ_5H<-seqlogo(nosZ_align, start = 717, end = 720, color = "Chemistry_AA", font = "DroidSansMono")
CuZ_6H<-seqlogo(nosZ_align, start = 775, end = 778, color = "Chemistry_AA", font = "DroidSansMono")
CuZ_7H<-seqlogo(nosZ_align, start = 843, end = 848, color = "Chemistry_AA", font = "DroidSansMono")

Total_site<-(CuA_firstH|CuA_2ndH)/((CuZ_1H|CuZ_3H|CuZ_4H)/(CuZ_5H|CuZ_6H|CuZ_7H))

graph2ppt(x=  Total_site,file="Figure",margins=c(0,0,0,0),upscale=T,
          append=T,width=14,height=10)

save.image(file = "Motif.Rdata")


