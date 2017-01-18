
rm(list=ls())
setwd("")


#######Enter Train dataset ONCE 

D1 <- 
  read.table("XYZ.csv", sep=",", as.is=TRUE, fill=TRUE, header=TRUE);

colnames(D1)

k<-N1 #INPUT THE COL NUMBER UPTO WHICH THERE ARE NO GENE VALUES. 

train=D1[,-c(1:k)]

View(train)

train1<-as.data.frame(cbind(Group=D1$Group, train));
View(train1)

nrow(train1)
ncol(train1)


#### RUN Random Forest
##Train data
install.packages("randomForest")
library("randomForest")

train1.rf <- randomForest(as.factor(Group)~., ntree=500, data=train1, importance=TRUE,  na.action=na.omit,
                        proximity=TRUE, replace=TRUE)
print(train1.rf)
attributes(train1.rf)
predict(train1.rf)

####Plot important genes

pdf("Gene_Importance_Plot.pdf");
	varImpPlot(train1.rf)
	dev.off();

###Enter Test dataset ONE AT A TIME
D2 <- 
  read.table("ABC.csv", sep=",", as.is=TRUE, fill=TRUE, header=TRUE);


colnames(D2)

k<-N2 #INPUT THE COL NUMBER UPTO WHICH THERE ARE NO GENE VALUES. 

test=as.data.frame(D2[,-c(1:k)])

nrow(test)
ncol(test)


###prediction using Test data
p1<-predict(train1.rf, newdata=test, type="response")


###prediction probabilities using Test data
p2<-predict(train1.rf, newdata=test, type="prob")

ABC_rf_Probabilites<-as.data.frame(cbind(prob=p1, "RLike"=p2[,1], "SLike"=p2[,2], D2))
write.csv(ABC_rf_Probabilites, "Predictions_ABC.csv")


