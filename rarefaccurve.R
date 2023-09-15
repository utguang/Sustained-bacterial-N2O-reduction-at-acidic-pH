install.packages('Nonpareil');
library(Nonpareil);

Nonpareil.curve('microcosm_El.npo')
                
list<-data.frame(File=c("EV_R1.npo","Soil.npo"),
                 Name=c("Co-culture","Soil"))

attach(list)

rarefac<-Nonpareil.set(c("EV_R1.npo","Soil.npo"), labels=list$Name, 
              col=c("green","blue","black"),
              plot.opts=list(plot.observed=FALSE))


