rm(list=ls())
setwd("")

D1=read.table("XYZ.csv", sep=",", as.is=TRUE, fill=TRUE, header=TRUE);

colnames(D1)

k<-N1 #INPUT THE COL NUMBER UPTO WHICH THERE ARE NO GENE VALUES. 

train=D1[,-c(1:k)]

###to scale-center TRAIN data

train.scale.centered<-scale(train, center=TRUE, scale=TRUE)


##############Random Survival Forest
##to use scale-centered TRAINING data in Random survival forest
train1<-as.data.frame(cbind(time=D1$pfstim, status=D1$pfsind, train.scale.centered));
View(train1)

colnames(train1)
install.packages("randomForestSRC")


library(randomForestSRC)
train1.obj <- rfsrc(Surv(time, status) ~ ., train1, ntree = 500, importance=TRUE,
                        proximity=TRUE, replace=TRUE)
print(train1.obj)

pp1=predict(train1.obj, times=3*365)

vs.train1 <- var.select(object = train1.obj) ##Variable Selection

topvars <- vs.train1$topvars ##Top Variable Selection

write.table(topvars, "Top genes_variables.txt")


###Test dataset
Dataset2 <- 
  read.table("ABC.csv", sep=",", as.is=TRUE, fill=TRUE, header=TRUE);

colnames(Dataset2)

k1<-N2 #INPUT THE COLUMN NUMBER UPTO WHICH THERE ARE NO GENE VALUES. 

test=Dataset2[,-c(1:k1)]

a<-colnames(D1)
b<-colnames(test.scaled)

intersect(a,b)
setdiff(a,b)

test.scaled=as.data.frame(scale(test, center=TRUE, scale=TRUE));
colnames(test.scaled)

pp3=predict(train1.obj, newdata=test.scaled, times=3*365);


TestSet_rf_Probabilites<-cbind(Prob=pp3$predicted, Dataset2[,c(1:k1)])
write.csv(TestSet_rf_Probabilites, "ABC_RSF_predictions.csv")


