#coursera assignment:har.
#create validation,training and testing groups.
traindata<-read.csv("C:/Users/tomer/Desktop/pml-training.csv",header=TRUE,sep=",")
testingdata<-read.csv("C:/Users/tomer/Desktop/pml-testing.csv",header=TRUE,sep=",")
inbuild<-createDataPartition(traindata$classe,p=0.7,list=FALSE)
validation<-traindata[-inbuild,];buildata<-traindata[inbuild,]
intrain<-createDataPartition(buildata$classe,p=0.7,list=FALSE)
training<-buildata[intrain,];testing<-buildata[-intrain,]
dim(validation);dim(training);dim(testing)

#setting up parallel run:
library(parallel)
library(doParallel)
cluster<-makeCluster(detectCores()-1)
registerDoParallel(cluster)

stopCluster(cluster)#<-after the run!

#create rf/boosting models, consider ensamble. preprocess -using pca. bagging?,boostraping/cv
my_trcont<-trainControl(method="cv",number=10,allowParallel=TRUE)
my_preproc<-preProcess(training,method="pca")
forest1<-train(classe~.,method="rf",data=training,trControl=my_trcont)# <-ended up testing this
forest2<-train(classe~.,method="rf",data=training,trControl=my_trcont,preProcess="pca")#ignore
boosting1<-train(classe~.,method="gbm",data=training,trControl=my_trcont)#ended up using this
boosting2<-train(classe~.,method="gbm",data=training,trControl=my_trcont,preProcess="pca")
bag1<-train(classe~.,method="treebag",data=training,trControl=my_trcont)#ignore

#predicting:
fidgling<-na.omit(testing)#create testing set without NA's
goodvalid<-na.omit(validation)#create validation set without NA's
forestpred1<-predict(forest1,testing)#preddict with rf, ended up not using
boostingpred1<-predict(boosting1,testing)#predict with boosted trees. ended up using
validpred<-predict(boosting1,goodvalid)#predict with voosting on validation set
confusionMatrix(validpred,goodvalid$classe)# asses sucsess, acuracy 97%
