setwd("~/Desktop/NYUSH/Spring2015/MachineLearning/FinalProject/ProgressReport")
library(e1071)
source("ggbiplot.R",encoding="ASCII")
source("http://bioconductor.org/biocLite.R")
biocLite("Biobase")
install.packages("NMF")
library(NMF)

#AQI for 6 pollutants in Shanghai from 2014 to 2015
PMdata1315AQI<-read.csv("Shanghai13-15AQI.csv",header=F)
#Concentration for 6 pollutants in Shanghai from 2014 to 2015
PMdata1315Con<-read.csv("Shanghai13-15CON.csv",header=F)

#SVM Method
AQI_train_label<-PMdata1315AQI[2:732,10]
AQI_train_label<-as.factor(AQI_train_label)
as.numeric(AQI_train_label)

AQI_train<-PMdata1315AQI[2:732,3:8]

PMdata1315AQI.svm<-svm(AQI_train_label~.,data=AQI_train,kernel='radial',cost=1,gamma=1/784,scale=TRUE,type='C-classification',cross=10)
summary(PMdata1315AQI.svm)

#PCA Method
PMdata1315Con_data<-matrix(unlist(PMdata1315Con[2:732,3:9]),731,7)
colnames(PMdata1315Con_data)=c("PM2.5","PM10","O3","SO2","NO2","CO","Season")
#PMdata1315Con_data<-as.numeric(PMdata1315Con_data)

log.con<-matrix(log(as.numeric(PMdata1315Con_data[,1:6])),731,6)
colnames(log.con)=c("PM2.5","PM10","O3","SO2","NO2","CO")
con <- prcomp(log.con,center=TRUE,scale.=TRUE)
con.class<-PMdata1315Con_data[,7]

con$sdev
con$x
head(con$rotation)
print(con)
summary(con)

plot(con, type = "b")

g <- ggbiplot(con, obs.scale = 1, var.scale = 1, 
              groups = con.class, ellipse = TRUE, circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
              legend.position = 'top')
print(g)

mat<-matrix(unlist(PMdata1315AQI[2:732,3:8]),6,731,byrow=TRUE)

mat<-matrix(as.numeric(mat),6,731,byrow=FALSE)
colsum<-colSums(mat)
for (i in 1:731){
  for (j in 1:6){
    mat[j,i]<-mat[j,i]/colsum[i]
  }
}

rownames(mat)=c("PM2.5","PM10","O3","SO2","NO2","CO")
colnames(mat)=PMdata1315Con[2:732,2]

matnmf<-nmf(mat,1,"lee")
fit(matnmf)