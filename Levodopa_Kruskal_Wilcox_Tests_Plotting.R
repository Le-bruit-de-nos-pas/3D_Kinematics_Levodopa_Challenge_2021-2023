allnewpatients = read.csv("allnewpatients.csv", sep = ";", dec = ",", na.strings=c("", " ","NA"))

allnewpatients$Group <- factor(allnewpatients$Group, levels = c("OFF", "20min", "40min", "60min", "80min"))

library(DAAG)
library(nlme)
library(ggplot2)
library(ggpubr)
library(viridisLite)
library(tidyverse)
library(rstatix)
library(ggsci)





init <- 1
while (init < 57) {
  test_results <- kruskal.test(levodopa[,init] ~ Treatment, data = levodopa)
  print(test_results)
  init = init+1
}


initpair <- 1
while (initpair < 57) {
  pairtestresults <- pairwise.wilcox.test(levodopa[,initpair], levodopa$Treatment,
                     p.adjust.method = "BH")
  print(pairtestresults)
  initpair = initpair+1
}



kruskal.test(levodopa[,1] ~ Treatment, data = levodopa)

pairwise.wilcox.test(levodopa[,1], levodopa$Treatment,
                     p.adjust.method = "BH")









ggplot(data = allnewpatients,aes(x = Group, y = allnewpatients[,1], fill = Group))+
  scale_fill_jama()+
  geom_violin(alpha=0.7, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.7, show.legend = F)+
  geom_point( shape = 21,size=7, position = position_jitterdodge(), color="black",alpha=0.8, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("Speed (m/s)")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)



