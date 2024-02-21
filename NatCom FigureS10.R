library(ggplot2)
library(randomForest)
library(ggpubr)
library("rnaturalearth")
library("rnaturalearthdata")
library(scatterpie)
library(patchwork)
library(export)
library(reshap)

library(piecewiseSEM)

word_mytheme<-theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor =element_blank(), 
                               legend.key.size = unit(10,"pt"),strip.text.x = element_blank(),
                               panel.border = element_blank(),panel.spacing=unit(0.1,"lines"),
                               
                               strip.background = element_blank(),
                               axis.ticks.length = unit(-0.10,"cm"),
                               axis.title.x = element_text(hjust=0.5,vjust=-2.0,size=10,family = "Times New Roman"),
                               axis.title.y = element_text(hjust=0.5,vjust=1.0,size=10,family = "Times New Roman"),
                               axis.title.y.right = element_text(angle = 90),
                               axis.text.x.bottom = element_text(vjust=0.5,size = 10,colour="black",family = "Times New Roman"),
                               axis.text.y  = element_text(hjust=0.5,size = 10,colour="black",family = "Times New Roman"),
                               legend.text = element_text(size = 10,family = "Times New Roman"),legend.position = "top",
                               legend.title = element_blank())

coverm_wetland<-full_join(coverm_relative[,c(2:4)],
                          metadata[,c(4,7)],by="run_accession")

wet_land_meta<-full_join(coverm_wetland,soil_characteristics[,-2],by="Sample_ID")



averages <- aggregate(. ~ Site, data = wet_land_meta[,c(2,3,5,9,10)], FUN = mean)

averages <- aggregate(. ~ Site, data = wet_land_meta[,c(2,3,5,9,10)],
                      FUN = function(x) c(mean = mean(x), se = sd(x) / sqrt(length(x))))

list(averages$pH<7)


averages$Longtitude<-ifelse(averages$Site %in% wet_land_meta$Site,
                            wet_land_meta$Longitude,NA)

averages$Latitude<-ifelse(averages$Site %in% wet_land_meta$Site,
                            wet_land_meta$Latitude,NA)

averages<-full_join(averages,soil_characteristics[,c(1,6,8,9,10)])

#random forest analysis
set.seed(315)

taxa.forest <- randomForest(Curated_DN~., data = averages[,c(2,3,4,5,8,9,10,11)], importance = TRUE,
                            na.action = na.omit)
round(importance(taxa.forest), 2)

predictor<-varImpPlot(taxa.forest, n.var = min(20, nrow(taxa.forest$importance)),type = 1,
           main = 'Variable importance')

importance<-data.frame(subset(predictor,predictor>0))
importance$predictor<-rownames(importance)

importance$predictor <- factor(importance$predictor, 
                              levels =importance$predictor[order(importance$X.IncMSE)])

predictor_gg<-ggplot(data=importance,aes(x=importance$X.IncMSE,y=predictor))+
  geom_bar(stat="identity",width = 0.5,fill="black")+word_mytheme+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  scale_x_continuous(expand = c(0,0))+
  theme(axis.ticks.y=element_blank())

# linear correlation
NO3subet<-subset(averages,NO3>0)
correlation_test <- cor.test(averages$Curated_DN, averages$NO3
                             )

p_value <- correlation_test$p.value

correlation<-ggplot(averages, aes(x = Curated_DN, y = Serratia.contigs)) +
  geom_point() +
  geom_smooth(size=0.5,color="black")  +
  word_mytheme+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)+
  geom_text(aes(x = 0.05, y = 1.0, label = paste("y =", round(coef(lm(Serratia.contigs ~ Curated_DN))["Curated_DN"], 2), "x +", round(coef(lm(Serratia.contigs ~ Curated_DN))["(Intercept)"], 2))),
            color = "black", size = 4, hjust = 0)


correlation_OM<-ggplot(averages, aes(x = Curated_DN, y = OrM)) +
  geom_point() +
  geom_smooth(size=0.5,color="black")  +
  word_mytheme+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)


correlation_NO3<-ggplot(subset(averages,NO3>0), aes(x = Curated_DN, y = NO3)) +
  geom_point() +
  geom_smooth(size=0.5,color="black")  +
  word_mytheme+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)

ggplot(averages, aes(x = Curated_DN, y = NO3)) +
  geom_point() +
  geom_smooth(size=0.5,color="black")  +
  word_mytheme+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=0.5)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=0.5)


# map summarizing distribution 
world <- ne_countries(scale = "medium", returnclass = "sf")


averages$classify<-ifelse(averages$pH<6,"Acidic","Circumneutral")
summary(averages$classify=="Acidic")

map<-ggplot(data = world) +
  geom_sf()+
  geom_point(data = averages,aes(x=Longtitude,y=Latitude,color=classify),size=3,
             position = position_jitter(width = 10, height = 10))+
  theme(legend.position = "none")+
  scale_x_continuous(breaks = c(-100,0,100))+
  scale_color_manual(breaks = c("Acidic","Circumneutral"),
                     values = c("red","blue"))

Distribution<-map/(predictor_gg|correlation)+plot_layout(heights = c(1.5,1))

Nega_correlation<-correlation_OM|correlation_NO3

graph2ppt(x=Distribution,file="Figure7",margins=c(0,0,0,0),upscale=T,
          append=T,width=14,height=10)


save.image(file = "Distribution.Rdata")


