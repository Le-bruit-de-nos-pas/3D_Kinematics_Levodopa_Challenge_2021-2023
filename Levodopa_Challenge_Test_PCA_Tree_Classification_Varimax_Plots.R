library(ggplot2)
library(caret)
library(C50)
library(lattice)
library(ggfortify)
library(plotly)
library(tidyverse)
library(viridis)
library(MASS)
library(ROCR)
library(ROCR)


vdata <- read.csv("Best_ON_Control.csv", sep = ";", dec = ",", header = T,)

df <- vdata[c(1:56)]
autoplot(prcomp(df), data= vdata, colour = 'Group', shape= 'Group' ) +
  geom_point(aes(size =40,  colour = Group, shape= Group), show.legend = FALSE) +
  theme_minimal()


set.seed(4)
levodopamodel <- preProcess(vdata[,-57], method = c("pca"))
PC <- predict(levodopamodel, vdata[,-57])
str(PC)

tr <- cbind(PC, label=vdata[,57])
tr$label<-as.factor(tr$label)

vdatapart <- createDataPartition(tr$label, p = 0.7, list = F)

traindata <- tr[vdatapart,]
validdata <- tr[-vdatapart,]


model <- C5.0(label ~ ., data = traindata)

summary(model)

result <- predict(model, validdata[,-21])
result
(accuracy<-sum(result == validdata$label)/nrow(validdata))


result == validdata$label

plot(model, colours(distinct = T))

########################################

prcompvdata <- prcomp(vdata[c(1:56)], center = TRUE, scale = TRUE)
summary(prcompvdata)

prcompvdata$rotation[,1:12]

my.var = varimax(prcompvdata$rotation)

myvarshort <-my.var$loadings[,1:12]
myvarshort <- as.data.frame(myvarshort)

screeplot(prcompvdata, type = "l", npcs = 12, main = "Screeplot of the first 12 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)


cumpro2 <- cumsum(prcompvdata$sdev^2 / sum(prcompvdata$sdev^2))
plot(cumpro2[0:12], xlab = "PC #", ylab = "Cumulative proportion of explained variance", main = "Cumulative variance plot")
abline(v = 12, col="blue", lty=5)
abline(h = 0.862, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC12"),
       col=c("blue"), lty=5, cex=0.6)



