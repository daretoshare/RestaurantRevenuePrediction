library(Boruta)
library(caret)
library(doSNOW)

train <- read.csv("data/train.csv")
test  <- read.csv("data/test.csv")
cityCW  <- read.csv("data/city_CW.csv")
train<-merge(train, cityCW, by="City", all.x = TRUE)
test<-merge(test, cityCW, by="City", all.x = TRUE)
n.train <- nrow(train)

train$City<-train$City2
test$City<-test$City2
train$City2<-NULL
test$City2<-NULL
test$revenue <- 1
myData<-rbind(train,test)
rm(train, test)

#Tranform Time
myData$Open.Date <- as.POSIXlt("01/01/2015", format="%m/%d/%Y") - as.POSIXlt(myData$Open.Date, format="%m/%d/%Y")
myData$Open.Date <- as.numeric(myData$Open.Date / 1000) #Scale for factors

#Consolidate Cities
myData$City                                      <- as.character(myData$City)
myData$City[myData$City.Group == "Other"]        <- "Other"
myData$City[myData$City == unique(myData$City)[4]] <- unique(myData$City)[2]
myData$City                                      <- as.factor(myData$City)
myData$City.Group                                <- NULL

#Consolidate Types
myData$Type <- as.character(myData$Type)
myData$Type[myData$Type=="DT"] <- "IL"
myData$Type[myData$Type=="MB"] <- "FC"
myData$Type <- as.factor(myData$Type)
myData$Id<-NULL

#Log Transform P Variables and Revenue
myData[, paste("P", 1:37, sep="")] <- log(1 +myData[, paste("P", 1:37, sep="")])

pp<-preProcess(myData[, paste("P", 1:37, sep="")])
myData[, paste("P", 1:37, sep="")]<-predict(pp, myData[, paste("P", 1:37, sep="")])
myData$revenue <- sqrt(log(myData$revenue))
myData$City2 <- NULL
important <- Boruta(revenue~., data=dat[1:n.train, ])

fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  ## repeated ten times
  repeats = 5)

rfGrid <-  expand.grid(mtry=(2:10))


cl <- makeCluster(3)
registerDoSNOW(cl)
set.seed(1)
#Random Forest
model <- train(revenue~., 
     #          data=dat[1:n.train, c(important$finalDecision != "Rejected", TRUE)],
        #       data=myData[1:n.train, c(important$finalDecision != "Rejected", TRUE)],
          data=dat[1:n.train,],
#               data=myData[1:n.train,],
               method = "RRF",
 #             preProcess=c("center","scale"),
               trControl = fitControl)
              
#,         tuneGrid = rfGrid)


stopCluster(cl)
#Make a Prediction
prediction <- predict(model, dat[-c(1:n.train), ])
prediction<-prediction**2
prediction<-exp(prediction)
#Make Submission
submit<-as.data.frame(cbind(seq(0, length(prediction) - 1, by=1), prediction))
colnames(submit)<-c("Id","Prediction")
write.csv(submit,"submission427145.csv",row.names=FALSE,quote=FALSE)

save(model, file="RRF26.RData")



library(reshape)
d <- melt(test[,-c(1:5)])

ggplot(d,aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x") + 
  geom_histogram()

for (i in 1:37){

myData$Pi<-as.factor(myData$Pi)
}

library(dplyr)
dat <- mutate_each(myData, funs(factor), c(P1,P5:P12,P14:P25,P30:P37))

