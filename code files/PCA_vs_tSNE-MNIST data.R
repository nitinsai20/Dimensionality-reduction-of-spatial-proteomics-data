#PCA vs t-SNE for MNIST data set
install.packages("pls")
library(pls)
# Principal Component Analysis

setwd("G:\\My Drive\\coursework\\spring-2020\\R Files\\Data Files\\project")

#Reading MNIST data

hw=read.csv("train_MNIST.csv",header=T)

hw1=hw[1:1000,-1]
hw2=hw[1:1000,]

#performing PCA
pcac=princomp(hw1,cor=FALSE)
summary(pcac)
loadings(pcac)
plot(pcac)
screeplot(pcac,type="l")
names(pcac)

plot(pcac$loadings[,1],pcac$loadings[,2])
text(pcac$loadings[,1],pcac$loadings[,2], labels= hw1$label, col=hw1$label)
abline(h=0,col='black')
abline(v=0,col='black')

plot(pcac$scores[,1],pcac$scores[,2])
text(pcac$scores[,1],pcac$scores[,2], labels= hw1$label, col=hw1$label)
legend(-2000,1500,legend= (hw1$label),col=hw1$label )

pc.df= data.frame(pcac$scores[,1],pcac$scores[,2])
lb <- as.factor(hw2$label)

#plotting pca dimensionality reduction 
gg <- ggplot(data=pc.df,aes(x=pcac$scores[,1], y=pcac$scores[,2])) +
  geom_point(aes(colour= lb)) +
  #scale_colour_brewer(palette = "YlOrRd") + 
  labs(colour="Digits") +
  xlab("PC1") + ylab("PC2") +
  ggtitle("Dimensionality Reduction using PCA") +
  geom_text(aes(label = lb))
gg

#importing tsne package
install.packages("Rtsne")
library(Rtsne)

#RUNNING TSNE code
tsne <- Rtsne(hw1, dims = 2, perplexity= 40, verbose=TRUE, max_iter = 500)
plot(tsne$Y)
text(tsne$Y, labels=hw2$label, col=hw2$label)

ts.df= data.frame(tsne$Y)

#plotting tsne dimensionality reduction 
ggt <- ggplot(data=ts.df,aes(x=ts.df[,1], y=ts.df[,2])) +
  geom_point(aes(colour= lb)) +
  #scale_colour_brewer(palette = "YlOrRd") + 
  labs(colour="Digits") +
  xlab("Comp1") + ylab("Comp2") +
  ggtitle("Dimensionality Reduction using t-SNE") 
geom_text(aes(label = lb))
ggt
