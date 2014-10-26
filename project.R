library(caret)
training<-read.csv('PML/pml-training.csv',header= TRUE)
testing<-read.csv('PML/pml-testing.csv',header=TRUE)
##data cleaning
#remove NAs
training<-training[,colSums(is.na(training)) == 0]

#remove near zero variance
nearZero<-nearZeroVar(training, saveMetrics=TRUE)
training<-training[, nearZero$nzv==FALSE]

#remove unrelevant columns
training<-training[,7:ncol(training)]
dim(training)

## Build a model use random forest
set.seed(1990)
inTrain<-createDataPartition(training$classe,p=0.7,list=FALSE)
train<-training[inTrain,]
test<-training[-inTrain,]

ctrl<-trainControl(method="cv", number=4, allowParallel=TRUE)
modFit<-train(classe ~ ., data=train, method="rf", trControl=ctrl)
modFit$finalModel

#cross validation and out-of sample error
testingPredict<-predict(modFit, test)
cm<-confusionMatrix(testingPredict,test$classe)
cm
outError<-sum(diag(cm$table))/sum(cm$table)
outError

## Predicting 20 test cases
predict20 <- predict(modFit, testing)
print( predict20 )

#Create one file for each submission
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}
pml_write_files(predict20)