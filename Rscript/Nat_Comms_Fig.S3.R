
install.packages("colorRamp2")
BiocManager::install("ComplexHeatmap")

library(ComplexHeatmap)
library(colorRamp2)
library(export)

#Prepare heatmap matrix formatted data
Matrix<-as.matrix(Fig_S3[,-1])
rownames(Matrix)<-Fig_S3$...1
row_names <- factor(row.names(Matrix), levels = list(rownames(Matrix)))


# define a col_function
col_fun = colorRamp2(c(0,50,90,95,100), c("black", "blue","white","green", "red"))

#plot heatmap using ComplexHeatmap package
FigureS3<-Heatmap(Matrix,col=col_fun,row_order = order(as.factor(rownames(Matrix))),
        column_order = order(as.factor(rownames(Matrix))) ,
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.1f", Matrix[i, j]), x, y, gp = gpar(fontsize = 10))
        })


save.image(file = "16S_similarity.Rdata")


graph2ppt(x=FigureS4,file="FigureS4",margins=c(0,0,0,0),upscale=T,
          append=T,width=14,height=10)
