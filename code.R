library(caret)
library(doSNOW)
##Read the raw file
##ottotrain<-read.csv("/home/dipanjan/DipanjanRepository/train.csv/train.csv")
train<-read.csv(paste0(getwd(),"/data/train.csv"))

train$date<-as.Date(train$Open.Date, format="%m/%d/%Y")
base <- strptime("1.4.2015", format="%d.%m.%Y")
train$age<-difftime(base,train$date, units="weeks")

train$Open.Date<-NULL
train$date<-NULL
train2<-train[,c(-1,-2,-4)]
citydummies <- dummyVars(City ~ ., data = train)
cityGroupdummies <- dummyVars(City.Group ~ ., data = train)
typedummies <- dummyVars(Type ~ ., data = train)

as.data.frame(predict(citydummies, newdata = train))->train2
as.data.frame(predict(cityGroupdummies, newdata = train))->train2
as.data.frame(predict(typedummies, newdata = train))->train2

set.seed(1)
## creating 5 fold cross validation for training control 
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

# eGrid <- expand.grid(.alpha = (1:10) * 0.1, 
#                      .lambda = (1:10) * 0.1)

cl <- makeCluster(3)
registerDoSNOW(cl)
## fitting a Gradient Boosting Machine with 5 CV training control
set.seed(1)
glmFit <- train(revenue ~ ., data = train2,
               method = "gbm",
               trControl = fitControl
#                tuneGrid =eGrid
              )

stopCluster(cl)

save(rfFit,file="rf.RData")
predict(rfFit, newdata = ts)->pre
confusionMatrix(ts$target, pre)

save(gbmFit,file="gbm.RData")
predict(gbmFit, newdata = ts)->pre
confusionMatrix(ts$target, pre)


## model scoring on new data
ottotest<-read.csv(paste0(getwd(),"/test.csv/test.csv"))
##predict(gbmFit, newdata = ottotest)->prediction
predict(rfFit, newdata = ottotest)->prediction
ottotest$pred<-prediction
ottotest<-ottotest[,c(1,95)]
dcast(ottotest, id ~ pred)->int
int->final
final[!is.na(final)]<-1
final[is.na(final)]<-0
final$id<-int$id
write.csv(final,"rfSub.csv", row.names=FALSE)

knnfit<-train(target ~ ., data = tr,
              method = "knn",
              preProcess = c("center","scale"),
              tuneLength = 20,
              trControl = fitControl)





MultiLogLoss <- function(act, pred)
{
  eps = 1e-15;
  nr <- nrow(pred)
  pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
  pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
  ll = sum(act*log(pred) )
  ll = ll * -1/(nrow(act))      
  return(ll);
}