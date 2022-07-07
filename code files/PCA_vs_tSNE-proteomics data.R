#PCA vs t-SNE for MNIST data set
install.packages("pls")
library(pls)
install.packages("Rtsne")
library(Rtsne)


setwd("G:\\My Drive\\coursework\\spring-2020\\R Files\\Data Files\\project")


#importing tsne package
install.packages("Rtsne")
library(Rtsne)

#importing organelle markers(om) data
om= read.csv("om.csv", header=TRUE, stringsAsFactors=FALSE)
attach(om)
co <-as.factor(RPA_24$localization)

#importing 24 hour post data
RPA_24=read.csv("RPA.csv",header=T, stringsAsFactors=FALSE)

#adding an empty column
RPA_24 <- RPA_24 %>%
  mutate(localisation = "NA")


#comparing all data = organelle marker data
for (i in 1:nrow(om)){
  for(j in 1:nrow(RPA_24)){
    if(om[i,1] == RPA_24[j,1]){
      RPA_24[j,10] <- om[i,3]
    }
  }
}
co <-as.factor(RPA_24$localisation)

#tsne for spatial-temporal data
RPA_24_m = RPA_24[,4:9]
RPA_tsne <- Rtsne(RPA_24_m, dims = 2, perplexity= 60, verbose=TRUE, max_iter = 500)
RPA.df= data.frame(RPA_tsne$Y)

#plotting tsne dimensionality reduction
ggt <- ggplot(data=RPA.df,aes(x=RPA.df[,1], y=RPA.df[,2]))+
  geom_point(aes(colour= co)) +
  scale_colour_manual(values=c("blue", "pink", "green","yellow","orange","purple","white","grey","red","black")) +
  labs(colour="Organelle") +
  xlab("Comp1") + ylab("Comp2") +
  ggtitle("Dimensionality Reduction using t-SNE") 
ggt

#importing infected data for 24-120hours.
RPA_24=read.csv("RPA_24.csv",header=T, stringsAsFactors=FALSE)
RPA_48=read.csv("RPA_I48.csv",header=T, stringsAsFactors=FALSE)
RPA_72=read.csv("RPA_I72.csv",header=T, stringsAsFactors=FALSE)
RPA_96=read.csv("RPA_I96.csv",header=T, stringsAsFactors=FALSE)
RPA_120=read.csv("RPA_I120.csv",header=T, stringsAsFactors=FALSE)
assloc= read.csv("assloc.csv",skip=1,header=T, stringsAsFactors=FALSE)

#defining a function for adding a column and comparing with assigned localization
addloc <- function(x,y) {
  if(y==24){
    z <- 5
  }else if(y == 48) {
    z <- 6
  } else if(y == 72){
    z <- 7
  }else if(y==96){
    z<- 8
  } else {
    z<-9
  }
  x <- x %>%
    mutate(localisation = "NA")
  for (i in 1:nrow(assloc)){
    for(j in 1:nrow(x)){
      if(assloc[i,1] == x[j,1]){
        x[j,10] <- assloc[i,z]
      }
    }
  }
  return(x)
}
RPA_24_m <- addloc(RPA_24,24)
RPA_48_m <-addloc(RPA_48,48)
RPA_72_m <- addloc(RPA_72,72)
RPA_96_m <- addloc(RPA_96,96)
RPA_120_m <- addloc(RPA_120,120)

#removing unspecified and NA proteins. 

#function for tsne and plotting. 
tsne <- function(x,y){
  x = na.omit(x)
  x <- x[(x$Organism != "#N/A"),]
  co <-as.factor(x$localisation)  
  or <-as.factor(x$Organism)
  x = x[,4:9]
  RPA_tsne <- Rtsne(x, dims = 2, perplexity= 60, verbose=TRUE, max_iter = 500)
  RPA.df= data.frame(RPA_tsne$Y)
  
  ggt <- ggplot(data=RPA.df,aes(x=RPA.df[,1], y=RPA.df[,2]))+
    geom_point(aes(shape=or,stroke=3.5))+
    scale_shape_manual(values= c(16,17)) +
    geom_point(aes(shape= or, size=or,colour= co))+
    scale_size_manual(values=c(1,4)) +
    
    scale_colour_manual(values=c("blue", "pink", "green","yellow","orange","purple","grey","darkred","red","white")) +
    labs(colour="Organelle") +
    xlab("Comp1") + ylab("Comp2") +
    ggtitle("48 hpi test")
  ggt
  
  #ggt <- ggplot(data=RPA.df,aes(x=RPA.df[,1], y=RPA.df[,2]))+
  #geom_point(aes(shape=or,size=or,colour=co))+
  #scale_size_manual(values=c(5,2)) +
  #scale_colour_manual(values=c("blue", "pink", "green","yellow","orange","purple","grey","darkred","red","white")) +
  #labs(colour="Organelle") +
  #labs(size="Organism") +
  #xlab("Comp1") + ylab("Comp2") +
  #ggtitle("120 hpi-60 and 5000")
  #ggt
  
}

#plotting tsne dimensionality reduction   librar
tsne(RPA_48_m,48)



#improting infected data for 24-120hours.
RPA_m24=read.csv("RPA_M24.csv",skip=2,header=T, stringsAsFactors=FALSE)
RPA_m48=read.csv("RPA_M48.csv",skip=2,header=T, stringsAsFactors=FALSE)
RPA_m72=read.csv("RPA_M72.csv",skip=2,header=T, stringsAsFactors=FALSE)
RPA_m96=read.csv("RPA_M96.csv",skip=2,header=T, stringsAsFactors=FALSE)
RPA_m120=read.csv("RPA_M120.csv",skip=2,header=T, stringsAsFactors=FALSE)

#adding a columnn and assigning localisation
addlocm <- function(x,y) {
  if(y==24){
    z <- 10
  }else if(y == 48) {
    z <- 11
  } else if(y == 72){
    z <- 12
  }else if(y==96){
    z<- 13
  } else {
    z<-14
  }
  x <- x %>%
    mutate(localisation = "NA")
  for (i in 1:nrow(assloc)){
    for(j in 1:nrow(x)){
      if(assloc[i,1] == x[j,1]){
        x[j,10] <- assloc[i,z]
      }
    }
  }
  return(x)
}
RPA_m24_m <- addlocm(RPA_m24,24)
RPA_m48_m <-addlocm(RPA_m48,48)
RPA_m72_m <- addlocm(RPA_m72,72)
RPA_m96_m <- addlocm(RPA_m96,96)
RPA_m120_m <- addlocm(RPA_m120,120)

#tsne for mock infected
tsnem <- function(x,y){
  x = na.omit(x)
  x <- x[(x$Organism != "#N/A"),]
  co <-as.factor(x$localisation)  
  or <-as.factor(x$Organism)
  x = x[,4:9]
  RPA_tsne <- Rtsne(x, dims = 2, perplexity= 100, verbose=TRUE, max_iter = 500)
  RPA.df= data.frame(RPA_tsne$Y)
  
  ggt <- ggplot(data=RPA.df,aes(x=RPA.df[,1], y=RPA.df[,2]))+
    geom_point(aes(size=or,colour=co))+
    scale_size_manual(values=c(2,2)) +
    scale_colour_manual(values=c("blue", "green","yellow","orange","purple","darkred","red","white")) +
    labs(colour="Organelle") +
    labs(size="Organism") +
    xlab("Comp1") + ylab("Comp2") +
    ggtitle("120 Mock Infected-100-500")
  ggt
  
}
tsnem(RPA_m120_m,120)

#subsetting mitochondria data
RPA_m120_mito <- RPA_m120_m[(RPA_m120_m$localisation == "Mitochondria"),]
RPA_tsne_mito <- Rtsne(RPA_m120_mito[,4:9], dims = 2, perplexity= 60, verbose=TRUE, max_iter = 1000)
plot(RPA_tsne_mito$Y)
text(RPA_tsne_mito$Y, labels=RPA_m120_mito$Uniprot.Accession)

#write.csv(RPA_m120_mito,"G:\\My Drive\\coursework\\spring-2020\\R Files\\Data Files\\project") 
ni= c(1,2,3,4,5,6)
for (i in 1:nrow(RPA_m120_mito)){
  lines(ni, RPA_m120_mito[i,4:9])
  par(new=TRUE)
}
