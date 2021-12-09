# week 11
rm(list=ls())

# Q1
library(ISLR)
#check the structure
str(OJ)

set.seed(3)
train <- sample(1:nrow(OJ),750) #training sample size is 750

# (a)
library(tree)
attach(OJ)

OJ.train <- OJ[train,]
OJ.test <- OJ[-train,]
OJ.tree <- tree(Purchase~. ,data = OJ.train)
summary(OJ.tree)

plot(OJ.tree)
text(OJ.tree,pretty = 0)

#(b) check the performance on testing data
OJ.treePredict <- predict(OJ.tree,newdata = OJ.test,type= 'class')
table(OJ.treePredict,OJ.test$Purchase)

#misclassification rate
(23+43)/nrow(OJ.test)

#(c)
# install.packages("randomForest")
library(randomForest)
?randomForest
OJ.bag <- randomForest(Purchase~.,data=OJ.train,mtry=17,importance = T)
OJ.bag

# check the performance on testing data
OJ.bagPredict <- predict(OJ.bag,newdata = OJ.test,type='class')
table(OJ.bagPredict,OJ.test$Purchase)

# miclassification rate
(21+40)/nrow(OJ.test)

importance(OJ.bag)
#visualize the importance of variables
varImpPlot(OJ.bag, col=c("blue","red"))

# (d)
# sqrt(17)
OJ.RF <- randomForest(Purchase~.,data=OJ.train,mtry=5,importance = T)
OJ.RF

OJ.RFPredict <- predict(OJ.RF,newdata = OJ.test,type='class')
table(OJ.RFPredict,OJ.test$Purchase)

# miclassification rate
(33+24)/nrow(OJ.test)

detach(OJ)


#Q4
customers=read.csv("wholesaleCustomers.csv")
str(customers)
customers6=customers[,3:8]
km5=kmeans(customers6,5,nstart=20)
attach(customers)
table(Channel, km5$cluster)

C2=rep("No",440)
C2[km5$cluster==2]="Yes"
C2 <- factor(C2)
customerC2=data.frame(customers6, C2)
treeC2=tree(C2~.,customerC2)
summary(treeC2)

?plot.tree
plot(treeC2,col='blue',lwd=3,type='uniform')
text(treeC2)


#(b) repeat the analysis for cluster 4 and 5
C4 <- rep('No',440)
C5 <- rep('No',440)
C4[km5$cluster == 4] <- 'Yes'
C5[km5$cluster == 5] <- 'Yes'
C4 <- factor(C4)
C5 <- factor(C5)

customerC4 <- data.frame(customers6,C4)
customerC5 <- data.frame(customers6,C5)

treeC4 <- tree(C4~.,customerC4)
treeC5 <- tree(C5~.,customerC5)

plot(treeC4,col='blue',lwd=3,type='uniform')
text(treeC4)

plot(treeC5,col='blue',lwd=3,type='uniform')
text(treeC5)


#Q5
#(a)
f2000=read.csv("Forbes2000In2017.csv", skip=6, header=T)
f2000n=na.omit(f2000)

#(b)
hc.f2000=hclust(dist(f2000n[,4:7]), method="complete") 
C20=cutree(hc.f2000, 20) 
table(C20)

#(c)
attach(f2000n) 
# complete linkage
# Complete-linkage (farthest neighbor) is where distance is measured 
# between the farthest pair of observations in two clusters.
hc.f30c=hclust(dist(f2000n[1:30,4:7]), method="complete") 
plot(hc.f30c, label=Company.Name[1:30], main="Cluster Dendrogram with Complete Linkage",
     col="turquoise4", xlab="", cex.main=2)

# single linkage
# Single-linkage (nearest neighbor) is the shortest distance 
# between a pair of observations in two clusters

hc.f30s=hclust(dist(f2000n[1:30,4:7]), method="single")
plot(hc.f30s, label=Company.Name[1:30], 
     main="Cluster Dendrogram with Single Linkage", col="darkred", xlab="", cex.main=2)
