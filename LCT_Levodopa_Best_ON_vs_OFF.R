allnewpatients = read.csv("allnewpatients.csv", sep = ";", dec = ",", na.strings=c("", " ","NA"))
allnewpatients$Group <- factor(allnewpatients$Group, levels = c("OFF", "20min", "40min", "60min", "80min"))


BestON_OFF_DEC_8 = read.csv("BestON_OFF_DEC_8.csv", sep = ";", dec = ",", na.strings=c("", " ","NA"))
BestON_OFF_DEC_8$Group <- factor(BestON_OFF_DEC_8$Group, levels = c("OFF", "BESTON"))

library(DAAG)
library(nlme)
library(ggplot2)
library(ggpubr)
library(viridisLite)
library(tidyverse)
library(rstatix)
library(ggsci)

init <- 1
while (init < 65) {
  test_results <- wilcox.test(BestON_OFF_DEC_8[,init] ~ BestON_OFF_DEC_8$Group,paired = TRUE, na.action = remove())
  print(test_results)
  init = init+1
}


################################################################################################
################################################################################################
################################################################################################

ggplot(data = allnewpatients,aes(x = Group, y = allnewpatients[,63], fill = Group))+
  scale_fill_jama()+
  geom_violin(alpha=0.7, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.7, show.legend = F)+
  geom_point( shape = 21,size=7, position = position_jitterdodge(), color="black",alpha=0.8, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("Swing Fraction Worst Side Asymmetry (percent cycle)")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)


################################################################################################
################################################################################################
################################################################################################


ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,1], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("Speed m/s")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,2], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("Cadence (steps/min)")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,3], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("Step Time - worst side (s)")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,4], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("Step Length - worst side (m)")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,5], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("Stride Time (s)")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,6], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("Stride Length (m)")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,7], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("Step Width (m)")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,8], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("Stance Fraction - worst side (% cycle)")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,9], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("Stance Time - worst time (s)")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,10], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("Swing Fraction - worst side (% cycle)")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,11], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("Swing Time - worst side (s)")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,12], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("Double Support (% cycle)")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,13], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("Double Support - worst side (% cycle)")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,14], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("Double Support Worst Time")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,15], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("Double Support Time (s)")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,16], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("Single Support - worst side (% cycle)")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,17], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("Single Support Time - worst side")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,18], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("hip_flexion_rom_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,19], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("hip_adduction_rom_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,20], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("hip_rotation_rom_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,21], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("knee_angle_rom_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)


ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,22], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("ankle_angle_rom_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,23], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("hip_flexion_mean_vel_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,24], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("hip_adduction_mean_vel_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,25], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("hip_rotation_mean_vel_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,26], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("knee_angle_r_mean_vel_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,27], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("ankle_angle_mean_vel_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,28], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("arm_flex_rom_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,29], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("arm_add_rom_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,30], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("elbow_flex_rom_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,31], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("arm_rot_rom_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,32], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("pro_sup_rom_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,33], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("wrist_flex_rom_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,34], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("wrist_dev_rom_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,35], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("arm_flex_mean_vel_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,36], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("arm_add_mean_vel_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,37], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("arm_rot_mean_vel_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,38], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("elbow_flex_mean_vel_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,39], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("pro_sup_mean_vel_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,40], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("wrist_flex_mean_vel_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,41], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("wrist_dev_mean_vel_worstside")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,42], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("entropy1")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)


ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,43], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("entropy2")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,44], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("entropy3")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,45], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("hr_ap")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,46], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("hr_vert")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,47], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("hr_ml")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,48], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("com_rms_ap")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,49], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("com_rms_vert")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,50], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("com_rms_ml")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestON_OFF_DEC_8,aes(x = Group, y = BestON_OFF_DEC_8[,60], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("StepLengthAsymetry")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestON_OFF_DEC_8,aes(x = Group, y = BestON_OFF_DEC_8[,59], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("StepTimeAsymetry")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestON_OFF_DEC_8,aes(x = Group, y = BestON_OFF_DEC_8[,63], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("SwingFractionAsymetry")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestON_OFF_DEC_8,aes(x = Group, y = BestON_OFF_DEC_8[,64], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("SwingTimeAsymetry")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestON_OFF_DEC_8,aes(x = Group, y = BestON_OFF_DEC_8[,61], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("StanceFractionAsymetry")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestON_OFF_DEC_8,aes(x = Group, y = BestON_OFF_DEC_8[,62], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("StanceTimeAsymetry")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,57], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("CV Speed")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,58], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("Cv Step Width")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,59], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("Cv Stride Lenght")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,60], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("CV Stride Time")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,61], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("CV Step Length")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,62], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("CV Step time")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,63], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("CV Double Support")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)

ggplot(data = BestONvsOFF17patients,aes(x = Group, y = BestONvsOFF17patients[,64], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  xlab ( NULL)+
  ylab(  c("CV Stance Fraction")  )  +
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15)



