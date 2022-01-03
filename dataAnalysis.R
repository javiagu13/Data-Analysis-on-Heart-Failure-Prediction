#TONTERIA INICIAL
library(ggplot2)
data <- read.csv("heart.csv")

data[,1]
data<-data[-450,]
data.pca <- prcomp(data[,c(4:6,8,10,12)], center = TRUE,scale. = TRUE)
summary(data.pca)


data1<-data.frame(data[,12],data[,4])
names(data1)<-c("HeartDisease","RestingBP")
datHealthy<-subset(data1, HeartDisease == 0)
datUnhealthy<-subset(data1, HeartDisease == 1)
datUnhealthy[,2]
hist(as.numeric(unlist(datUnhealthy[,2])), col='red')
hist(as.numeric(unlist(datHealthy[,2])),col='blue', add=TRUE)


hist(as.numeric(unlist(datHealthy[,2])),col='blue')
hist(as.numeric(unlist(datUnhealthy[,2])), col='red', add=TRUE)


#TONTERIA SEGUNDA
#Preprocesado del data
out<-data.matrix(data)
out[,1]
euclideanDistance<-dist(out[,1], method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
euclideanDistance

data.pca <- prcomp(out, center = TRUE,scale. = TRUE)
summary(data.pca)

#CONTENIDO REAL
data <- read.csv("heart.csv")
data<-data[-450,]
data<-data.matrix(data)

myPr <- prcomp(data[,c(1, 4:5,8)], scale = TRUE)

summary(myPr)
plot(myPr, type = "l")
biplot(myPr)
biplot(myPr, scale = 0)
str(myPr)
myPr$x

data2 <- cbind(data, myPr$x)
library(ggplot2)
data2<-data.frame(data2)
ggplot(data2, aes(PC1, PC2, col = HeartDisease, fill = HeartDisease)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")

cor(data[,c(1, 4:5,8)], data2[, 13:16])


#CONTENIDO REAL EMPIEZA AQUI ######################################
data <- read.csv("heart.csv")
data<-data[-450,]
data<-data.matrix(data)

myPr <- prcomp(data[,c(1:11)], scale = TRUE)

summary(myPr)
plot(myPr, type = "l")
biplot(myPr)
biplot(myPr, scale = 0)
str(myPr)
myPr$x

data2 <- cbind(data, myPr$x)
library(ggplot2)
data2<-data.frame(data2)
ggplot(data2, aes(PC1, PC2, col = HeartDisease, fill = HeartDisease)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")

cor(data[,c(1:11)], data2[, 13:16])

##GGPLOTS: Age 1, ExerciseAngina 9, RestingBP 4, Oldpeak 10

dataAge<-data.frame(data[,12],data[,1])
names(dataAge)<-c("HeartDisease","Age")
dataAngina<-data.frame(data[,12],data[,9])
names(dataAngina)<-c("HeartDisease","ExerciseAngina")
dataResting<-data.frame(data[,12],data[,4])
names(dataResting)<-c("HeartDisease","RestingBP")
dataPeak<-data.frame(data[,12],data[,10])
names(dataPeak)<-c("HeartDisease","Oldpeak")

#Age Study
datHealthy<-subset(dataAge, HeartDisease == 0)
datUnhealthy<-subset(dataAge, HeartDisease == 1)
datUnhealthy[,2]
hist(as.numeric(unlist(datUnhealthy[,2])), col='red')
hist(as.numeric(unlist(datHealthy[,2])),col='blue', add=TRUE)
#second
hist(as.numeric(unlist(datHealthy[,2])),col='blue')
hist(as.numeric(unlist(datUnhealthy[,2])), col='red', add=TRUE)
#plot
p<-ggplot() + 
  geom_point(data=data2, mapping=aes(x = Age, y = HeartDisease,color = "blue"))+
  xlab("nth sample (10,25,50...)") +
  ylab("variance value")


#Angina study
datHealthy<-subset(dataAngina, HeartDisease == 0)
datUnhealthy<-subset(dataAngina, HeartDisease == 1)
datUnhealthy[,2]
hist(as.numeric(unlist(datUnhealthy[,2])), col='red')
hist(as.numeric(unlist(datHealthy[,2])),col='blue', add=TRUE)
#second
hist(as.numeric(unlist(datHealthy[,2])),col='blue')
hist(as.numeric(unlist(datUnhealthy[,2])), col='red', add=TRUE)
#plot
p<-ggplot() + 
  geom_point(data=data2, mapping=aes(x = ExerciseAngina, y = HeartDisease,color = "blue"))+
  xlab("nth sample (10,25,50...)") +
  ylab("variance value")

#RestingBP study
datHealthy<-subset(dataResting, HeartDisease == 0)
datUnhealthy<-subset(dataResting, HeartDisease == 1)
datUnhealthy[,2]
hist(as.numeric(unlist(datUnhealthy[,2])), col='red')
hist(as.numeric(unlist(datHealthy[,2])),col='blue', add=TRUE)
#second
hist(as.numeric(unlist(datHealthy[,2])),col='blue')
hist(as.numeric(unlist(datUnhealthy[,2])), col='red', add=TRUE)
#plot
p<-ggplot() + 
  geom_point(data=data2, mapping=aes(x = RestingBP, y = HeartDisease,color = "blue"))+
  xlab("nth sample (10,25,50...)") +
  ylab("variance value")


#Oldpeak study
datHealthy<-subset(dataPeak, HeartDisease == 0)
datUnhealthy<-subset(dataPeak, HeartDisease == 1)
datUnhealthy[,2]
hist(as.numeric(unlist(datUnhealthy[,2])), col='red')
hist(as.numeric(unlist(datHealthy[,2])),col='blue', add=TRUE)
#second
hist(as.numeric(unlist(datHealthy[,2])),col='blue')
hist(as.numeric(unlist(datUnhealthy[,2])), col='red', add=TRUE)
#plot
p<-ggplot() + 
  geom_point(data=data2, mapping=aes(x = Oldpeak, y = HeartDisease,color = "blue"))+
  xlab("nth sample (10,25,50...)") +
  ylab("variance value")

#BASURA
p<-ggplot() + 
  #geom_line(data=data2, mapping=aes(x = Age, y = HeartDisease,color = "blue"))+
  geom_point(data=data2, mapping=aes(x = Age, y = HeartDisease,color = "blue"))+
  xlab("nth sample (10,25,50...)") +
  ylab("variance value")

p + scale_color_manual(labels = c("alpha variance"),values=c("dodgerblue4"))
